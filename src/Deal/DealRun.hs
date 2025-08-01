{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealRun (
   run
   ,accrueRC
) where

import qualified Data.Set as S
import qualified Data.DList as DL
import Data.List
import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Monad
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Control.Monad.Loops (allM,anyM)

import qualified Asset as Ast
import qualified Cashflow as CF
import qualified Accounts as A
import qualified Data.Map as Map hiding (mapEither)
import qualified Waterfall as W
import qualified Liability as L
import qualified Reports as Rpt
import qualified Pool as P
import qualified Assumptions as AP
import qualified Hedge as HE
import qualified CreditEnhancement as CE
import qualified InterestRate as IR
import Triggers

import Deal.DealBase
import Deal.DealAction
import Deal.DealQuery
import Deal.DealCollection
import Revolving
import Hedge
import Stmt
import Types

import Util
import Lib
-- ^ execute effects of trigger: making changes to deal
-- TODO seems position of arugments can be changed : f :: a -> b -> m a  => f:: b -> a -> m a
runEffects :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent) -> Date -> TriggerEffect 
           -> Either String (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent)
runEffects (t@TestDeal{accounts = accMap, fees = feeMap ,status=st, bonds = bondMap, pool=pt
                      ,collects = collRules}, rc, actions, logs) d te
  = case te of 
      DealStatusTo _ds -> return (t {status = _ds}, rc, actions, logs)
      DoAccrueFee fns -> do
                           newFeeList <- traverse (calcDueFee t d)  $ (feeMap Map.!) <$> fns
                           let newFeeMap = Map.fromList (zip fns newFeeList) <> feeMap
                           return (t {fees = newFeeMap}, rc, actions, logs)

      ChangeReserveBalance accName rAmt ->
          return (t {accounts = Map.adjust (set A.accTypeLens (Just rAmt)) accName accMap }
                    , rc, actions, logs)
      
      TriggerEffects efs -> foldM (`runEffects` d) (t, rc, actions, logs) efs
      
      RunActions wActions -> do
                              (newT, newRc, newLogs) <- foldM (performActionWrap d) (t, rc, DL.empty) wActions
                              return (newT, newRc, actions, DL.append logs newLogs)

      ChangeBondRate bName bRateType bRate -> 
        let 
          -- accrual rate
          -- set current rate 
          -- update rate component
          updateFn b = L.accrueInt d b  
                      & set L.interestInfoTraversal bRateType
                      & set L.curRatesTraversal bRate 
          -- updated deal
          t' = t {bonds = updateBondInMap bName updateFn bondMap}
          -- build bond rate reset actions
          newActions = case getBondByName t' True bName of 
                        Just bnd -> [ ResetBondRate _d bName | _d <- L.buildRateResetDates bnd d (getDate (last actions))]
                        Nothing -> []
        in 
          return (t' , rc, sortBy sortActionOnDate (newActions++actions), logs) 

      DoNothing -> return (t, rc, actions, DL.empty)
      _ -> Left $ "Date:"++ show d++" Failed to match trigger effects: "++show te

setBondStepUpRate :: Date -> [RateAssumption] -> L.Bond -> Either String L.Bond
setBondStepUpRate d ras b@(L.Bond _ _ _ ii (Just sp) _ _ _ _ _ _ _ _ _)
  = return $ 
      let 
        newII = L.stepUpInterestInfo sp ii
        newRate = AP.applyFloatRate ii d ras
      in 
        (L.accrueInt d b) { L.bndInterestInfo = newII, L.bndRate = newRate }

setBondStepUpRate d ras b@(L.MultiIntBond bn _ _ iis (Just sps) _ _ _ _ _ _ _ _ _)
  = return $ 
      let 
        newIIs = zipWith L.stepUpInterestInfo sps iis
        newRates = (\x -> AP.applyFloatRate x d ras) <$> newIIs
      in 
        (L.accrueInt d b) { L.bndInterestInfos = newIIs, L.bndRates = newRates }  -- `debug` (show d ++ ">> accure due to step up rate "++ bn)

setBondStepUpRate d ras bg@(L.BondGroup bMap pt)
  = do 
      m <- mapM (setBondStepUpRate d ras) bMap
      return $ L.BondGroup m pt

-- ^ update bond interest rate from rate assumption
setBondNewRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> L.Bond -> Either String L.Bond
setBondNewRate t d ras b@(L.Bond _ _ L.OriginalInfo{ L.originDate = od} ii _ bal currentRate _ dueInt _ Nothing _ _ _)
  = setBondNewRate t d ras b {L.bndDueIntDate = Just od}

-- ^ Floater rate
setBondNewRate t d ras b@(L.Bond _ _ _ ii@(L.Floater br idx _spd rset dc mf mc) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = return $ (L.accrueInt d b){ L.bndRate = AP.applyFloatRate ii d ras }

-- ^ Fix rate, do nothing
setBondNewRate t d ras b@(L.Bond _ _ _ L.Fix {} _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = return b

-- ^ Ref rate
setBondNewRate t d ras b@(L.Bond _ _ _ (L.RefRate sr ds factor _) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = do
      let b' = L.accrueInt d b
      rate <- queryCompound t d (patchDateToStats d ds)
      return b' {L.bndRate = fromRational (rate * toRational factor) }

-- ^ cap & floor & IoI
setBondNewRate t d ras b@(L.Bond _ _ _ ii _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = return $ (L.accrueInt d b) { L.bndRate = AP.applyFloatRate ii d ras}

-- ^ bond group
setBondNewRate t d ras bg@(L.BondGroup bMap pt)
  = do 
      m <- mapM (setBondNewRate t d ras) bMap
      return $ L.BondGroup m pt

-- ^ apply all rates for multi-int bond
setBondNewRate t d ras b@(L.MultiIntBond bn _ _ iis _ bal currentRates _ dueInts dueIoIs _ _ _ _)
  = let 
      newRates = AP.applyFloatRate <$> iis <*> pure d <*> pure ras
      b' = L.accrueInt d b -- `debug` ("accrue due to new rate "++ bn)
    in
      return $ b' { L.bndRates = newRates } 

-- ^ accure rate cap 
accrueRC :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> RateCap -> Either String RateCap
accrueRC t d rs rc@RateCap{rcNetCash = amt, rcStrikeRate = strike,rcIndex = index
                        ,rcStartDate = sd, rcEndDate = ed, rcNotional = notional
                        ,rcLastStlDate = mlsd
                        ,rcStmt = mstmt} 
  | d > ed || d < sd = return rc 
  | otherwise = do
                  r <- AP.lookupRate0 rs index d
                  balance <- case notional of
                               Fixed bal -> Right . toRational $ bal
                               Base ds -> queryCompound t d (patchDateToStats d ds)
                               Schedule ts -> return $ getValByDate ts Inc d

                  let accRate = max 0 $ r - fromRational (getValByDate strike Inc d) -- `debug` ("Rate from curve"++show (getValByDate strike Inc d))
                  let addAmt = case mlsd of 
                                 Nothing -> IR.calcInt (fromRational balance) sd d accRate DC_ACT_365F
                                 Just lstD -> IR.calcInt (fromRational balance) lstD d accRate DC_ACT_365F

                  let newAmt = amt + addAmt  -- `debug` ("Accrue AMT"++ show addAmt)
                  let newStmt = appendStmt (IrsTxn d newAmt addAmt 0 0 0 SwapAccrue) mstmt 
                  return $ rc { rcLastStlDate = Just d ,rcNetCash = newAmt, rcStmt = newStmt }

updateRateSwapBal :: Ast.Asset a => TestDeal a -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapBal t d rs@HE.RateSwap{ HE.rsNotional = base }
  =  case base of 
        HE.Fixed _ -> return rs  
        HE.Schedule ts -> return $ rs { HE.rsRefBalance = fromRational (getValByDate ts Inc d) }
        HE.Base ds -> 
            do 
              v <- queryCompound t d (patchDateToStats d ds) 
              return rs { HE.rsRefBalance = fromRational v} -- `debug` ("query Result"++ show (patchDateToStats d ds) )

updateRateSwapRate :: Ast.Asset a => TestDeal a -> Maybe [RateAssumption] -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapRate t Nothing _ _ = Left "Failed to update rate swap: No rate input assumption"
updateRateSwapRate t (Just rAssumps) d rs@HE.RateSwap{ HE.rsType = rt } 
  = let 
      getRate x = AP.lookupRate rAssumps x d
    in
      do  
        (pRate,rRate) <- case rt of 
                           HE.FloatingToFloating flter1 flter2 ->
                             do 
                               r1 <- getRate flter1
                               r2 <- getRate flter2
                               return (r1, r2)
                           HE.FloatingToFixed flter r -> 
                             do 
                               _r <- getRate flter
                               return (_r, r)
                           HE.FixedToFloating r flter -> 
                             do 
                               _r <- getRate flter
                               return (r, _r)
                           HE.FormulaToFloating ds flter -> 
                             do 
                               _r <- queryCompound t d (patchDateToStats d ds)
                               r <- getRate flter
                               return (fromRational _r, r)
                           HE.FloatingToFormula flter ds -> 
                             do 
                               r <- getRate flter
                               _r <- queryCompound t d (patchDateToStats d ds)
                               return (r, fromRational _r)
        return rs {HE.rsPayingRate = pRate, HE.rsReceivingRate = rRate }

updateLiqProviderRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> CE.LiqFacility -> CE.LiqFacility
updateLiqProviderRate t d ras liq@CE.LiqFacility{CE.liqRateType = mRt, CE.liqPremiumRateType = mPrt
                                                , CE.liqRate = mr, CE.liqPremiumRate = mPr }
  = let 
      newMr =  AP.evalFloaterRate d ras <$> mRt
      newMpr = AP.evalFloaterRate d ras <$> mPrt
      -- TODO probably need to accure int when interest rate changes ? 
    in 
      liq {CE.liqRate = newMr, CE.liqPremiumRate = newMpr }

runTriggers :: Ast.Asset a => (TestDeal a, RunContext a, [ActionOnDate]) -> Date -> DealCycle -> Either String (TestDeal a, RunContext a, [ActionOnDate], DL.DList ResultComponent)
runTriggers (t@TestDeal{status=oldStatus, triggers = Nothing},rc, actions) d dcycle = return (t, rc, actions, DL.empty)
runTriggers (t@TestDeal{status=oldStatus, triggers = Just trgM},rc, actions) d dcycle = 
  do
    let trgsMap = Map.findWithDefault Map.empty dcycle trgM
    let trgsToTest = Map.filter   
                           (\trg -> (not (trgStatus trg) || trgStatus trg && trgCurable trg))
                           trgsMap
    triggeredTrgs <- mapM (testTrigger t d) trgsToTest
    let triggeredEffects = [ trgEffects _trg | _trg <- Map.elems triggeredTrgs, (trgStatus _trg) ] 
    (newDeal, newRc, newActions, logsFromTrigger) <- foldM (`runEffects` d) (t,rc,actions, DL.empty) triggeredEffects
    let newStatus = status newDeal 
    let newLogs = DL.fromList [DealStatusChangeTo d oldStatus newStatus "By trigger"|  newStatus /= oldStatus] -- `debug` (">>"++show d++"trigger : new st"++ show newStatus++"old st"++show oldStatus)
    let newTriggers = Map.union triggeredTrgs trgsMap
    return (newDeal {triggers = Just (Map.insert dcycle newTriggers trgM)}
           , newRc
           , newActions
           , DL.append newLogs logsFromTrigger) -- `debug` ("New logs from trigger"++ show d ++">>>"++show newLogs)

appendCollectedCF :: Ast.Asset a => Date -> TestDeal a -> Map.Map PoolId CF.PoolCashflow -> TestDeal a
-- ^ append cashflow frame (consolidate by a date) into deals collected pool
appendCollectedCF d t@TestDeal { pool = MultiPool poolM } poolInflowMap
  = let
      newPt = MultiPool $
                   Map.foldrWithKey
                     (\k (CF.CashFlowFrame st txnCollected, mAssetFlow) acc ->
                       let 
                         currentStats = case view (P.poolFutureCf . _Just . _1 . CF.cashflowTxn) (acc Map.! k) of
                                         [] -> P.poolBegStats (acc Map.! k)
                                         txns -> fromMaybe (0,0,0,0,0,0) $ view CF.txnCumulativeStats (last txns)
                         balInCollected = case length txnCollected of 
                                            0 -> 0 
                                            _ ->  view CF.tsRowBalance $ last txnCollected
                         txnToAppend = CF.patchCumulative currentStats [] txnCollected
                         accUpdated =  Map.adjust
                                         (\_v -> case (P.futureCf _v) of
                                                   Nothing -> set P.poolFutureCf (Just (CF.CashFlowFrame st txnCollected , Nothing)) _v
                                                   Just _ -> over (P.poolFutureCf . _Just . _1 . CF.cashflowTxn) (++ txnToAppend) _v
                                         )
        		   	                     k
       			   	                     acc 
       			 -- insert breakdown asset flow
       			 accUpdated' = case mAssetFlow of 
       			   	  Nothing -> accUpdated
       			   	  Just collectedAssetFlow -> 
       			   	    let 
       			   	      appendFn Nothing = Just collectedAssetFlow   
       			   	      appendFn (Just cfs) 
       			   	        | length cfs == length collectedAssetFlow 
       	                                           = Just $ [ origin & over CF.cashflowTxn (++ (view CF.cashflowTxn new)) | (origin,new) <- zip cfs  collectedAssetFlow ] 
       			   		| length collectedAssetFlow  > length cfs 
                                                          = let 
                                                              dummyCashFrames = replicate (length collectedAssetFlow - length cfs) CF.emptyCashflow 
       			   		      in 
       			   		        Just $ [ origin & over (CF.cashflowTxn) (++ (view CF.cashflowTxn new)) | (origin,new) <- zip (cfs++dummyCashFrames) collectedAssetFlow ]
       			   		| otherwise = error "incomping cashflow number shall greater than existing cashflow number"
       			   	    in 
       			   	      accUpdated & ix k %~ (over (P.poolFutureCf . _Just . _2) appendFn)
                       in 
                         Map.adjust 
                           (over P.poolIssuanceStat (Map.insert RuntimeCurrentPoolBalance balInCollected))
                           k accUpdated') 
                     poolM 
                     poolInflowMap
    in 
      t {pool = newPt} 

appendCollectedCF d t@TestDeal { pool = ResecDeal uds } poolInflowMap
 = let 
     newPt =  ResecDeal $ 
                Map.foldrWithKey
                  (\k (CF.CashFlowFrame _ newTxns, _) acc-> Map.adjust (over uDealFutureCf (`CF.appendMCashFlow` newTxns)) k acc)
                  uds
		          poolInflowMap
   in 
     t {pool = newPt} 

run :: Ast.Asset a => TestDeal a -> Map.Map PoolId CF.PoolCashflow -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe ([Pre],[Pre])
        -> Maybe (Map.Map String (RevolvingPool,AP.ApplyAssumptionType)) -> DL.DList ResultComponent 
        -> Either String (TestDeal a, DL.DList ResultComponent, Map.Map PoolId CF.PoolCashflow)
run t@TestDeal{status=(Ended endedDate)} pCfM ads _ _ _ log  = return (t,DL.snoc log (EndRun endedDate "By Status:Ended"), pCfM)
run t pCfM (Just []) _ _ _ log  = return (t,DL.snoc log (EndRun Nothing "No Actions"), pCfM)
run t pCfM (Just [HitStatedMaturity d]) _ _ _ log  = return (t, DL.snoc log (EndRun (Just d) "Stop: Stated Maturity"), pCfM)
run t pCfM (Just (StopRunFlag d:_)) _ _ _ log  = return (t, DL.snoc log (EndRun (Just d) "Stop Run Flag"), pCfM)
run t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap,status=dStatus
              ,waterfall=waterfallM,name=dealName,pool=pt,stats=_stat}
    poolFlowMap (Just (ad:ads)) rates calls rAssump log
  | futureCashToCollectFlag && (queryCompound t (getDate ad) AllAccBalance == Right 0) && (dStatus /= Revolving) && (dStatus /= Warehousing Nothing) --TODO need to use prsim here to cover all warehouse status
     = do 
        let runContext = RunContext poolFlowMap rAssump rates --- `debug` ("ending at date " ++ show (getDate ad))
        (finalDeal,_,newLogs) <- foldM (performActionWrap (getDate ad)) (t,runContext,log) cleanUpActions 
        return (finalDeal
                , DL.snoc newLogs (EndRun (Just (getDate ad)) "No Pool Cashflow/All Account is zero/Not revolving")
                , poolFlowMap)
  | otherwise
    = case ad of 
        -- TODO : need to seperate waterfall execution in pool collection
        PoolCollection d _ ->
          if any (> 0) remainCollectionNum then
            let 
              cutOffPoolFlowMap = Map.map (\(pflow,mAssetFlow) -> 
                                            (CF.splitCashFlowFrameByDate pflow d EqToLeft
                                              ,(\xs -> [ CF.splitCashFlowFrameByDate x d EqToLeft | x <- xs ]) <$> mAssetFlow))
                                          poolFlowMap 
              collectedFlow =  Map.map (bimap fst ((\xs -> [ fst x | x <- xs ]) <$>)) cutOffPoolFlowMap  
              outstandingFlow = Map.map (bimap snd ((\xs -> [ snd x | x <- xs ]) <$>)) cutOffPoolFlowMap  

              -- deposit cashflow to SPV from external pool cf               
            in 
              do 
                accs <- depositPoolFlow (collects t) d collectedFlow accMap 
                let dAfterDeposit = (appendCollectedCF d t collectedFlow) {accounts=accs}
                -- nScheduleFlowMap = Map.map (over CF.cashflowTxn (cutBy Exc Future d)) (fromMaybe Map.empty (getScheduledCashflow t Nothing))
                let newPt = case pool dAfterDeposit of 
	  		      MultiPool pm -> MultiPool $ (over (mapped . P.poolFutureScheduleCf . _Just . _1 . CF.cashflowTxn) (cutBy Exc Future d)) pm 
			      ResecDeal dMap -> ResecDeal $ (over (mapped . uDealFutureScheduleCf . _Just . CF.cashflowTxn) (cutBy Exc Future d)) dMap
                let runContext = RunContext outstandingFlow rAssump rates  
                (dRunWithTrigger0, rc1, ads2, newLogs0) <- runTriggers (dAfterDeposit {pool = newPt},runContext,ads) d EndCollection 
                let eopActionsLog = DL.fromList [ RunningWaterfall d W.EndOfPoolCollection | Map.member W.EndOfPoolCollection waterfallM ] -- `debug` ("new logs from trigger 1"++ show newLogs0)
                let waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection (waterfall t) 
                (dAfterAction,rc2,newLogs) <- foldM (performActionWrap d) (dRunWithTrigger0 ,rc1 ,log ) waterfallToExe 
                (dRunWithTrigger1,rc3,ads3,newLogs1) <- runTriggers (dAfterAction,rc2,ads2) d EndCollectionWF 
                run (increasePoolCollectedPeriod dRunWithTrigger1 )
                    (runPoolFlow rc3) 
                    (Just ads3) 
                    rates 
                    calls 
                    rAssump 
                    (DL.concat [newLogs0,newLogs,eopActionsLog,newLogs1]) 
          else
            run t poolFlowMap (Just ads) rates calls rAssump log 

        RunWaterfall d "" -> 
          let
            runContext = RunContext poolFlowMap rAssump rates
            waterfallKey 
              | Map.member (W.DistributionDay dStatus) waterfallM = W.DistributionDay dStatus
              | otherwise = W.DefaultDistribution

            waterfallToExe = Map.findWithDefault [] waterfallKey waterfallM
            callTest = fst $ fromMaybe ([]::[Pre],[]::[Pre]) calls
          in 
            do 
              -- Run triggers before waterfall distribution
              (dRunWithTrigger0, rc1, ads1, newLogs0) <- runTriggers (t, runContext, ads) d BeginDistributionWF 
              let logsBeforeDist
                    | Map.notMember waterfallKey waterfallM 
                        = DL.snoc newLogs0 (WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey))
                    | otherwise = newLogs0
              flag <- anyM (testPre d dRunWithTrigger0) callTest 
              if flag then
                -- Clean Up Waterfall Actions
                do
                  let newStLogs
                        | null cleanUpActions = [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution"]
                        | otherwise = [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution", RunningWaterfall d W.CleanUp] 
                  (dealAfterCleanUp, rc_, newLogWaterfall_) <- foldM (performActionWrap d) (dRunWithTrigger0, rc1,log) cleanUpActions 
                  endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp d newLogWaterfall_
                  return (dealAfterCleanUp
                         , DL.concat [logsBeforeDist,endingLogs,DL.fromList (newStLogs++[EndRun (Just d) "Clean Up"])]
                         , runPoolFlow rc_
                         )
              else
                -- Non-Clean Up Waterfall Actions
                do
                  (dAfterWaterfall, rc2, newLogsWaterfall) <- foldM (performActionWrap d) (dRunWithTrigger0,rc1,log) waterfallToExe 
                  (dRunWithTrigger1, rc3, ads2, newLogs2) <- runTriggers (dAfterWaterfall,rc2,ads1) d EndDistributionWF
                  run (increaseBondPaidPeriod dRunWithTrigger1)
                      (runPoolFlow rc3) 
                      (Just ads2) 
                      rates 
                      calls 
                      rAssump 
                      (DL.concat [newLogsWaterfall, newLogs2 ,logsBeforeDist,DL.fromList [RunningWaterfall d waterfallKey]])

        -- Custom waterfall execution action from custom dates
        RunWaterfall d wName -> 
          let
            runContext = RunContext poolFlowMap rAssump rates
            waterfallKey = W.CustomWaterfall wName
          in 
            do
              waterfallToExe <- maybeToEither
                                  ("No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) $
                                  Map.lookup waterfallKey waterfallM
              let logsBeforeDist =[ WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) 
                                    | Map.notMember waterfallKey waterfallM ]  
              (dAfterWaterfall, rc2, newLogsWaterfall) <- foldM (performActionWrap d) (t,runContext,log) waterfallToExe 
              run dAfterWaterfall (runPoolFlow rc2) (Just ads) rates calls rAssump 
                  (DL.concat [newLogsWaterfall,DL.fromList (logsBeforeDist ++ [RunningWaterfall d waterfallKey])]) 

        EarnAccInt d accName ->
          let 
            newAcc = Map.adjust (A.depositInt d) accName accMap
          in 
            run (t {accounts = newAcc}) poolFlowMap (Just ads) rates calls rAssump log

        AccrueFee d feeName -> 
          do 
            fToAcc <- maybeToEither ("Failed to find fee "++feeName) (Map.lookup feeName feeMap)
            newF <- calcDueFee t d fToAcc
            let newFeeMap = Map.fromList [(feeName,newF)] <> feeMap
            run (t{fees=newFeeMap}) poolFlowMap (Just ads) rates calls rAssump log

        ResetLiqProvider d liqName -> 
          case liqProvider t of 
            Nothing -> run t poolFlowMap (Just ads) rates calls rAssump log
            (Just mLiqProvider) 
              -> let -- update credit 
                   newLiqMap = Map.adjust (updateLiqProvider t d) liqName mLiqProvider
                 in
                   run (t{liqProvider = Just newLiqMap}) poolFlowMap (Just ads) rates calls rAssump log

        ResetLiqProviderRate d liqName -> 
          case liqProvider t of 
            Nothing -> run t poolFlowMap (Just ads) rates calls rAssump log
            (Just mLiqProvider) 
              -> let -- update rate 
                   newLiqMap = Map.adjust (updateLiqProviderRate t d (fromMaybe [] rates)) liqName mLiqProvider
                 in
                   run (t{liqProvider = Just newLiqMap}) poolFlowMap (Just ads) rates calls rAssump log
        
        DealClosed d ->
          let
            w = Map.findWithDefault [] W.OnClosingDay (waterfall t) 
            rc = RunContext poolFlowMap rAssump rates  
            logForClosed =  [RunningWaterfall d W.OnClosingDay| not (null w)]
          in 
            do
              newSt <- case dStatus of
                         (PreClosing st) -> return st
                         _ -> Left $ "DealClosed action is not in PreClosing status but got"++ show dStatus
              (newDeal, newRc, newLog) <- foldM (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
              run newDeal{status=newSt} (runPoolFlow newRc) (Just ads) rates calls rAssump 
                  (DL.concat [newLog, DL.fromList ([DealStatusChangeTo d (PreClosing newSt) newSt "By Deal Close"]++logForClosed)]) -- `debug` ("new st at closing"++ show newSt)

        ChangeDealStatusTo d s -> run (t{status=s}) poolFlowMap (Just ads) rates calls rAssump log

        CalcIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                newRateSwap_rate <- adjustM (updateRateSwapRate t rates d) sn rSwap
                newRateSwap_bal <- adjustM (updateRateSwapBal t d) sn newRateSwap_rate 
                let newRateSwap_acc = Map.adjust (HE.accrueIRS d) sn newRateSwap_bal
                run (t{rateSwap = Just newRateSwap_acc}) poolFlowMap (Just ads) rates calls rAssump log

        SettleIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                acc <- case HE.rsSettleDates (rSwap Map.! sn) of 
                          Nothing -> Left $ "No settle date found for "++ sn
                          Just (_, _accName) -> return $ accMap Map.! _accName
                let accBal = A.accBalance acc
                let rs = rSwap Map.! sn
                let settleAmt = HE.rsNetCash rs
                let accName = A.accName acc
                case (settleAmt <0, accBal < abs settleAmt) of 
                  (True, True) ->
                    do
                      newAcc <- adjustM (A.draw d accBal (SwapOutSettle sn)) accName accMap
                      let newRsMap = Just $ Map.adjust (HE.payoutIRS d accBal) sn rSwap
                      run (t {accounts = newAcc, rateSwap = newRsMap})
                          poolFlowMap (Just ads) 
                          rates 
                          calls 
                          rAssump 
                          (DL.snoc log (WarningMsg ("Settle Rate Swap Error: "++ show d ++" Insufficient balance to settle "++ sn)))
                      -- Left $ "Settle Rate Swap Error: "++ show d ++" Insufficient balance to settle "++ sn
                  (True, False) -> 
                    do
                      newAcc <- adjustM (A.draw d (abs settleAmt) (SwapOutSettle sn)) accName  accMap
                      let newRsMap = Just $ Map.adjust (HE.payoutIRS d settleAmt) sn rSwap 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) poolFlowMap (Just ads) rates calls rAssump log
                  (False, _) -> 
                    let 
                      newAcc = Map.adjust (A.deposit settleAmt d (SwapInSettle sn)) accName accMap
                      newRsMap = Just $ Map.adjust (HE.receiveIRS d) sn rSwap 
                    in 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) poolFlowMap (Just ads) rates calls rAssump log

        AccrueCapRate d cn -> 
          case rateCap t of 
            Nothing -> Left $ " No rate cap found for "++ cn
            Just rCap ->
              let
                _rates = fromMaybe [] rates
              in 
                do 
                  newRateCap <- adjustM (accrueRC t d _rates) cn rCap
                  run (t{rateCap = Just newRateCap}) poolFlowMap (Just ads) rates calls rAssump log

        InspectDS d dss -> 
          do
            newlog <- inspectListVars t d dss 
            run t poolFlowMap (Just ads) rates calls rAssump $ DL.append log (DL.fromList newlog)
        
        ResetBondRate d bn  -> 
          let 
            rateList = fromMaybe [] rates
            bnd = bndMap Map.! bn
          in 
            do 
              newBnd <- setBondNewRate t d rateList bnd 
              run t{bonds = Map.fromList [(bn,newBnd)] <> bndMap} poolFlowMap (Just ads) rates calls rAssump log
        
        StepUpBondRate d bn -> 
          let 
            bnd = bndMap Map.! bn
          in 
            do 
              newBndMap <- adjustM (setBondStepUpRate d (fromMaybe [] rates)) bn bndMap
              run t{bonds = newBndMap } poolFlowMap (Just ads) rates calls rAssump log
        
        ResetAccRate d accName -> 
          do
            newAccMap <- adjustM 
                          (\a@(A.Account _ _ (Just (A.InvestmentAccount idx spd dp dp1 lastDay _)) _ _)
                            -> do
                                 newRate <- AP.lookupRate (fromMaybe [] rates) (idx,spd) d 
                                 let accWithNewInt = A.depositInt d a
                                 return accWithNewInt { A.accInterest = Just (A.InvestmentAccount idx spd dp dp1 lastDay newRate)})
                          accName accMap
            run t{accounts = newAccMap} poolFlowMap (Just ads) rates calls rAssump log

        BuildReport sd ed ->
          let 
            cashReport = Rpt.buildCashReport t sd ed 
          in 
            do 
              bsReport <- Rpt.buildBalanceSheet t ed
              let newlog = FinancialReport sd ed bsReport cashReport
              run t poolFlowMap (Just ads) rates calls rAssump $ DL.snoc log newlog -- `debug` ("new log"++ show ed++ show newlog)

        FireTrigger d cyc n -> 
          let 
            triggerFired = case mTrgMap of 
                               Nothing -> error "trigger is empty for override" 
                               Just tm -> Map.adjust (Map.adjust (set trgStatusLens True) n) cyc tm
            triggerEffects = do
                                tm <- mTrgMap
                                cycM <- Map.lookup cyc tm
                                trg <- Map.lookup n cycM
                                return $ trgEffects trg
            
            runContext = RunContext poolFlowMap rAssump rates
          in 
            do 
              (newT, rc@(RunContext newPool _ _), adsFromTrigger, newLogsFromTrigger) 
                <- case triggerEffects of 
                    Nothing -> return (t, runContext, ads, DL.empty) -- `debug` "Nothing found on effects"
                    Just efs -> runEffects (t, runContext, ads, DL.empty) d efs
              let (oldStatus,newStatus) = (status t,status newT)
              let stChangeLogs = DL.fromList [DealStatusChangeTo d oldStatus newStatus "by Manual fireTrigger" |  oldStatus /= newStatus] 
              run newT {triggers = Just triggerFired} newPool (Just ads) rates calls rAssump $ DL.concat [log,stChangeLogs,newLogsFromTrigger]
        
        MakeWhole d spd walTbl -> 
            let 
              schedulePoolFlowMap = case pt of 
				      MultiPool pMap -> Map.map (view (P.poolFutureScheduleCf._Just._1) ) pMap 
				      ResecDeal uDealMap -> Map.map (view (uDealFutureScheduleCf . _Just)) uDealMap
            in 
              do 
                factor <- liftA2
                            (/)
                            (queryCompound t d (FutureCurrentPoolBegBalance Nothing)) 
                            (queryCompound t d (FutureCurrentSchedulePoolBegBalance Nothing))
                let reduceCfs = Map.map (\f -> (over CF.cashflowTxn (\xs -> CF.scaleTsRow factor <$> xs) f, Nothing ) ) schedulePoolFlowMap -- need to apply with factor and trucate with date
                (runDealWithSchedule,_,_) <- run t reduceCfs (Just ads) rates calls rAssump log
                let bondWal = Map.map (L.calcWalBond d) (bonds runDealWithSchedule) -- `debug` ("Bond schedule flow"++ show (bonds runDealWithSchedule))
                let bondSprd = Map.map 
                                 (\x -> (spd + (fromMaybe 0 (lookupTable walTbl Up (fromRational x >)))))
                                 bondWal 
                let bondPricingCurve = Map.map 
                                         (\x -> IRateCurve [ TsPoint d x,TsPoint (getDate (last ads)) x])
                                         bondSprd 
                bondPricingResult <- sequenceA $ Map.intersectionWith (flip (L.priceBond d)) (bonds runDealWithSchedule) bondPricingCurve 
                let depositBondFlow = Map.intersectionWith
                                        (\bnd (PriceResult pv _ _ _ _ _ _) -> 
                                          let 
                                            ostBal = L.getCurBalance bnd
                                            prinToPay = min pv ostBal
                                            intToPay = max 0 (pv - prinToPay)
                                            bnd1 = L.payPrin d prinToPay bnd
                                          in 
                                            L.payYield d intToPay bnd1)
                                        (bonds t)
                                        bondPricingResult
                run t {bonds = depositBondFlow, status = Ended (Just d)} Map.empty (Just []) rates calls rAssump $ DL.snoc log (EndRun (Just d) "MakeWhole call")
        
        FundBond d Nothing bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              let bndFunded = L.fundWith d fundAmt $ bndMap Map.! bName
              run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                  poolFlowMap (Just ads) rates calls rAssump log

        FundBond d (Just p) bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              flag <- testPre d t p
              case flag of
                False -> run t poolFlowMap (Just ads) rates calls rAssump (DL.snoc log (WarningMsg ("Failed to fund bond"++ bName++ ":" ++show p)))
                True -> 
                  do
                    let bndFunded = L.fundWith d fundAmt $ bndMap Map.! bName
                    run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                        poolFlowMap (Just ads) rates calls rAssump log
          

        IssueBond d Nothing bGroupName accName bnd mBal mRate -> 
           run t poolFlowMap (Just ((IssueBond d (Just (Always True)) bGroupName accName bnd mBal mRate):ads)) rates calls rAssump log
        
        IssueBond d (Just p) bGroupName accName bnd mBal mRate ->
            do 
              flag <- testPre d t p
              case flag of
                False -> run t poolFlowMap (Just ads) rates calls rAssump (DL.snoc log (WarningMsg ("Failed to issue to bond group"++ bGroupName++ ":" ++show p)))
                True -> let 
                          newBndName = L.bndName bnd
                        in
                           do
                             newBalance <- case mBal of
                                             Just _q -> queryCompound t d (patchDateToStats d _q)  
                                             Nothing -> Right . toRational $ L.originBalance (L.bndOriginInfo bnd)
                             newRate <- case mRate of 
                                         Just _q -> queryCompound t d (patchDateToStats d _q)
                                         Nothing -> return $ L.originRate (L.bndOriginInfo bnd)
                             let newBonds = case Map.lookup bGroupName bndMap of
                                              Nothing -> bndMap
                                              Just L.Bond {} -> bndMap
                                              Just (L.BondGroup bndGrpMap pt) -> let
                                                                                bndOInfo = (L.bndOriginInfo bnd) {L.originDate = d, L.originRate = newRate, L.originBalance = fromRational newBalance }
                                                                                bndToInsert = bnd {L.bndOriginInfo = bndOInfo,
                                                                                                   L.bndDueIntDate = Just d,
                                                                                                   L.bndLastIntPay = Just d, 
                                                                                                   L.bndLastPrinPay = Just d,
                                                                                                   L.bndRate = fromRational newRate,
                                                                                                   L.bndBalance = fromRational newBalance}
                                                                              in 
                                                                                Map.insert bGroupName 
                                                                                           (L.BondGroup (Map.insert newBndName bndToInsert bndGrpMap) pt)
                                                                                           bndMap

                             let issuanceProceeds = fromRational newBalance
                             let newAcc = Map.adjust (A.deposit issuanceProceeds d (IssuanceProceeds newBndName))
                                                     accName
                                                     accMap
                             run t{bonds = newBonds, accounts = newAcc} poolFlowMap (Just ads) rates calls rAssump log
        RefiBondRate d accName bName iInfo ->
           let
             lstDate = getDate (last ads)
             isResetActionEvent (ResetBondRate _ bName ) = False 
             isResetActionEvent _ = True
             filteredAds = filter isResetActionEvent ads
             newRate = L.getBeginRate iInfo
          in 
             do 
               nBnd <- calcDueInt t d $ bndMap Map.! bName
               let dueIntToPay = L.getTotalDueInt nBnd
               let acc = accMap Map.! accName
               let actualPayout = min (A.accBalance acc) dueIntToPay
               let newBnd = set L.bndIntLens iInfo $ L.payInt d actualPayout nBnd
               let resetDates = L.buildRateResetDates newBnd d lstDate 
               let bResetActions = [ ResetBondRate d' bName | d' <- resetDates ]
               newAccMap <- adjustM (draw d actualPayout (PayInt [bName])) accName accMap
               let newBndMap = Map.insert bName (newBnd {L.bndRate = newRate, L.bndDueIntDate = Just d ,L.bndLastIntPay = Just d}) bndMap
               let newAds = sortBy sortActionOnDate $ filteredAds ++ bResetActions
               run t{bonds = newBndMap, accounts = newAccMap} poolFlowMap (Just newAds) rates calls rAssump log
           
        RefiBond d accName bnd -> Left "Undefined action: RefiBond"

        TestCall d ->
          let 
            timeBasedTests::[Pre] = snd (fromMaybe ([],[]) calls)
          in
            do 
              flags::[Bool] <- traverse (testPre d t) timeBasedTests
              case any id flags of
                True -> 
                  let 
                    runContext = RunContext poolFlowMap rAssump rates
                    newStLogs 
		              | null cleanUpActions = DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call"]
                      | otherwise = DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call", RunningWaterfall d W.CleanUp]
                  in  
                    do 
                      (dealAfterCleanUp, rc_, newLogWaterfall_ ) <- foldM (performActionWrap d) (t, runContext, log) cleanUpActions
                      endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp d newLogWaterfall_
                      return (dealAfterCleanUp
                              , DL.snoc (endingLogs `DL.append` newStLogs) (EndRun (Just d) "Clean Up")
                              , (runPoolFlow rc_))
                _ -> run t poolFlowMap (Just ads) rates calls rAssump log

        StopRunTest d pres -> 
	  do
            flags::[Bool] <- sequenceA $ [ (testPre d t pre) | pre <- pres ]
            case all id flags of
	      True -> return (t, DL.snoc log (EndRun (Just d) ("Stop Run Test by:"++ show (zip pres flags))), poolFlowMap)
	      _ -> run t poolFlowMap (Just ads) rates calls rAssump log


        _ -> Left $ "Failed to match action on Date"++ show ad

       where
         cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))
         remainCollectionNum = Map.elems $ Map.map (\(x,_) -> CF.sizeCashFlowFrame x ) poolFlowMap
         futureCashToCollectFlag = and $ Map.elems $ Map.map (\(pcf,_) -> all CF.isEmptyRow2 (view CF.cashflowTxn pcf)) poolFlowMap

-- run :: Ast.Asset a => TestDeal a -> Map.Map PoolId CF.PoolCashflow -> Maybe [ActionOnDate] -> Maybe [RateAssumption] -> Maybe ([Pre],[Pre])
--         -> Maybe (Map.Map String (RevolvingPool,AP.ApplyAssumptionType)) -> DL.DList ResultComponent 
--         -> Either String (TestDeal a, DL.DList ResultComponent, Map.Map PoolId CF.PoolCashflow)

-- run t empty Nothing Nothing Nothing Nothing log
--   = do
--       (t, ads, pcf, unStressPcf) <- getInits S.empty t Nothing Nothing 
--       run t pcf (Just ads) Nothing Nothing Nothing log  -- `debug` ("Init Done >>Last Action#"++show (length ads)++"F/L"++show (head ads)++show (last ads))

run t empty _ _ _ _ log = return (t, log ,empty) -- `debug` ("End with pool CF is []")
