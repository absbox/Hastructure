{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealRun ( run ) where

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
import qualified Expense as F
import qualified Liability as L
import qualified Reports as Rpt
import qualified Pool as P
import qualified Assumptions as AP
import qualified Hedge as HE
import qualified CreditEnhancement as CE
import qualified InterestRate as IR
import Triggers
import Interface

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

import Debug.Trace
debug = flip trace

-- ^ execute effects of trigger: making changes to deal
-- TODO seems position of arugments can be changed : f :: a -> b -> m a  => f:: b -> a -> m a
runEffects :: Ast.Asset a => (TestDeal a, RunContext, [ActionOnDate], DL.DList ResultComponent) -> Date -> TriggerEffect 
            -> Either String (TestDeal a, RunContext, [ActionOnDate], DL.DList ResultComponent)
runEffects (t@TestDeal{accounts = accMap, fees = feeMap ,status=st, bonds = bondMap, pool=pt
                      ,collects = collRules}, rc, actions, logs) d te
  = case te of 
      DealStatusTo _ds -> return (t {status = _ds}, rc, actions, logs)
      DoAccrueFee fns -> do
                            fees <- lookupVs fns feeMap
                            newFeeList <- traverse (calcDueFee t rc d) fees
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
                        Just bnd -> [ ResetBondRate _d bName | _d <- L.buildRateResetDates bnd d (case actions of [] -> d; as -> getDate (last as))]
                        Nothing -> []
        in 
          return (t' , rc, sortBy sortActionOnDate (newActions++actions), logs) 

      DoNothing -> return (t, rc, actions, DL.empty)
      _ -> Left $ "Date:"++ show d++" Failed to match trigger effects: "++show te

setBondStepUpRate :: Date -> [RateAssumption] -> L.Bond -> Either ErrorRep L.Bond
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
setBondNewRate :: Ast.Asset a => TestDeal a -> RunContext -> Date -> [RateAssumption] -> L.Bond -> Either ErrorRep L.Bond
setBondNewRate t rc d ras b@(L.Bond _ _ L.OriginalInfo{ L.originDate = od} ii _ bal currentRate _ dueInt _ Nothing _ _ _)
  = setBondNewRate t rc d ras b {L.bndDueIntDate = Just od}

-- ^ Floater rate
setBondNewRate t rc d ras b@(L.Bond _ _ _ ii@(L.Floater br idx _spd rset dc mf mc) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = return $ (L.accrueInt d b){ L.bndRate = AP.applyFloatRate ii d ras }

-- ^ Fix rate, do nothing
setBondNewRate t rc d ras b@(L.Bond _ _ _ L.Fix {} _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _)
  = return b

-- ^ Ref rate
setBondNewRate t rc d ras b@(L.Bond _ _ _ (L.RefRate sr ds factor _) _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = do
      let b' = L.accrueInt d b
      rate <- queryCompound t rc d (patchDateToStats d ds)
      return b' {L.bndRate = fromRational (rate * toRational factor) }

-- ^ cap & floor & IoI
setBondNewRate t rc d ras b@(L.Bond _ _ _ ii _ bal currentRate _ dueInt _ (Just dueIntDate) _ _ _) 
  = return $ (L.accrueInt d b) { L.bndRate = AP.applyFloatRate ii d ras}

-- ^ bond group
setBondNewRate t rc d ras bg@(L.BondGroup bMap pt)
  = do 
      m <- mapM (setBondNewRate t rc d ras) bMap
      return $ L.BondGroup m pt

-- ^ apply all rates for multi-int bond
setBondNewRate t rc d ras b@(L.MultiIntBond bn _ _ iis _ bal currentRates _ dueInts dueIoIs _ _ _ _)
  = let 
      newRates = AP.applyFloatRate <$> iis <*> pure d <*> pure ras
      b' = L.accrueInt d b -- `debug` ("accrue due to new rate "++ bn)
    in
      return $ b' { L.bndRates = newRates } 

updateRateSwapBal :: Ast.Asset a => TestDeal a -> RunContext -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapBal t rc d rs@HE.RateSwap{ HE.rsNotional = base }
  =  case base of 
        HE.Fixed _ -> return rs  
        HE.Schedule ts -> return $ rs { HE.rsRefBalance = fromRational (getValByDate ts Inc d) }
        HE.Base ds -> 
            do 
              v <- queryCompound t rc d (patchDateToStats d ds) 
              return rs { HE.rsRefBalance = fromRational v} -- `debug` ("query Result"++ show (patchDateToStats d ds) )

updateRateSwapRate :: Ast.Asset a => TestDeal a -> RunContext -> Maybe [RateAssumption] -> Date -> HE.RateSwap -> Either String HE.RateSwap
updateRateSwapRate t _ Nothing _ _ = Left "Failed to update rate swap: No rate input assumption"
updateRateSwapRate t rc (Just rAssumps) d rs@HE.RateSwap{ HE.rsType = rt } 
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
                              _r <- queryCompound t rc d (patchDateToStats d ds)
                              r <- getRate flter
                              return (fromRational _r, r)
                          HE.FloatingToFormula flter ds -> 
                            do 
                              r <- getRate flter
                              _r <- queryCompound t rc d (patchDateToStats d ds)
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

runTriggers :: Ast.Asset a => (TestDeal a, RunContext, [ActionOnDate]) -> Date -> DealCycle -> Either String (TestDeal a, RunContext, [ActionOnDate], DL.DList ResultComponent)
runTriggers (t@TestDeal{status=oldStatus, triggers = Nothing},rc, actions) d dcycle = return (t, rc, actions, DL.empty)
runTriggers (t@TestDeal{status=oldStatus, triggers = Just trgM},rc, actions) d dcycle = 
  do
    let trgsMap = Map.findWithDefault Map.empty dcycle trgM
    let trgsToTest = Map.filter   
                          (\trg -> (not (trgStatus trg) || trgStatus trg && trgCurable trg))
                          trgsMap
    triggeredTrgs <- mapM (testTrigger t rc d) trgsToTest
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


run :: Ast.Asset a => TestDeal a 
        -> RunContext
        -> Maybe [ActionOnDate]  
        -> Maybe ([Pre],[Pre])
        -> DL.DList ResultComponent 
        -> Either ErrorRep (TestDeal a, DL.DList ResultComponent, Map.Map PoolId CF.PoolCashflow)
-- ^ Ended by Status
run t@TestDeal{status=(Ended endedDate)} (RunContext pCfM _ _) ads _ log  = return (t,DL.snoc log (EndRun endedDate "By Status:Ended"), pCfM)
-- ^ Ended by No more Actions
run t (RunContext pCfM _ _) (Just []) _ log  = return (t,DL.snoc log (EndRun Nothing "No Actions"), pCfM)
-- ^ Ended by Stated Maturity
run t (RunContext pCfM _ _) (Just [HitStatedMaturity d]) _ log  = return (t, DL.snoc log (EndRun (Just d) "Stop: Stated Maturity"), pCfM)
-- ^ Ended by Stop Run Flag
run t (RunContext pCfM _ _) (Just (StopRunFlag d:_)) _ log  = return (t, DL.snoc log (EndRun (Just d) "Stop Run Flag"), pCfM)

run t@TestDeal{accounts=accMap,fees=feeMap,triggers=mTrgMap,bonds=bndMap,status=dStatus
              ,waterfall=waterfallM,name=dealName,pool=pt,stats=_stat}
    rc@(RunContext poolFlowMap rAssump rates) 
    (Just (ad:ads)) calls log
    -- Ended by No Pool Cashflow/All Account is zero/Not revolving
  | futureCashToCollectFlag && (queryCompound t rc (getDate ad) AllAccBalance == Right 0) && (dStatus /= Revolving) && (dStatus /= Warehousing Nothing) --TODO need to use prsim here to cover all warehouse status
    = let 
          endingLog = EndRun (Just (getDate ad)) "No Pool Cashflow/All Account is zero/Not revolving"
          endingDate = getDate ad
        in 
          if Map.member W.CleanUp waterfallM then
            do 
              (finalDeal,RunContext newPoolFlowMap _ _,newLogs) <- foldM (performActionWrap endingDate) (t,rc,log) cleanUpActions 
              return (finalDeal, DL.concat [newLogs, DL.fromList [RunningWaterfall endingDate W.CleanUp, endingLog] ] , newPoolFlowMap)
          else
            return (t , DL.snoc log endingLog, poolFlowMap)
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
              cutFutureCf = cutBy Exc Future d
              -- deposit cashflow to SPV from external pool cf               
            in 
              do 
                -- depsoit collected cashflow to accounts
                accs <- depositPoolFlow (collects t) d collectedFlow accMap 
                -- new deal = update accounts and pool collected cashflow
                let dAfterDeposit = (appendCollectedCF d t collectedFlow) {accounts=accs}
                let newPt = case pool dAfterDeposit of 
                              MultiPool pm -> 
                                MultiPool $ (over (mapped . P.poolFutureScheduleCf . _Just . _1 . CF.cashflowTxn) cutFutureCf) pm 
                              ResecDeal dMap -> 
                                ResecDeal $ (over (mapped . uDealFutureScheduleCf . _Just . CF.cashflowTxn) cutFutureCf) dMap
                let runContext = RunContext outstandingFlow rAssump rates  
                (dRunWithTrigger0, rc1, ads2, newLogs0) <- runTriggers (dAfterDeposit {pool = newPt}, runContext, ads) d EndCollection 
                let eopActionsLog = DL.fromList [ RunningWaterfall d W.EndOfPoolCollection | Map.member W.EndOfPoolCollection waterfallM ] 
                let waterfallToExe = Map.findWithDefault [] W.EndOfPoolCollection waterfallM 
                (dAfterAction,rc2,newLogs) <- foldM (performActionWrap d) (dRunWithTrigger0 ,rc1 ,log ) waterfallToExe 
                (dRunWithTrigger1, rc3, ads3, newLogs1) <- runTriggers (dAfterAction, rc2, ads2) d EndCollectionWF 
                run (increasePoolCollectedPeriod dRunWithTrigger1 )
                    rc3
                    (Just ads3) 
                    calls 
                    (DL.concat [newLogs0,newLogs,eopActionsLog,newLogs1]) 
          else
            run t rc (Just ads) calls log 

        AccruePoolCollection d x -> 
          do 
            t' <- (accrueDeal d (fromMaybe [] rates) t rc)
            run t' rc (Just (PoolCollection d x:ads)) calls log

        AccrueRunWaterfall d x -> 
          do 
            t' <- (accrueDeal d (fromMaybe [] rates) t rc)
            run t' rc (Just (RunWaterfall d x:ads)) calls log

        RunWaterfall d "" -> 
          let
            waterfallKey 
              | Map.member (W.DistributionDay dStatus) waterfallM = W.DistributionDay dStatus
              | otherwise = W.DefaultDistribution

            waterfallToExe = Map.findWithDefault [] waterfallKey waterfallM
            callTest = fst $ fromMaybe ([]::[Pre],[]::[Pre]) calls
          in 
            do 
              -- Run triggers before waterfall distribution
              (dRunWithTrigger0, rc1, ads1, newLogs0) <- runTriggers (t, rc, ads) d BeginDistributionWF 
              let logsBeforeDist
                    | Map.notMember waterfallKey waterfallM 
                        = DL.snoc newLogs0 
                                  (WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey++"from"++ (show (Map.keys waterfallM))))
                    | otherwise = newLogs0
              flag <- anyM (testPre d dRunWithTrigger0 rc1) callTest 
              if flag then
                -- Clean Up Waterfall Actions
                do
                  let newStLogs
                        | null cleanUpActions = [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution"]
                        | otherwise = [DealStatusChangeTo d dStatus Called "Call by triggers before waterfall distribution", RunningWaterfall d W.CleanUp] 
                  (dealAfterCleanUp, rc_, newLogWaterfall_) <- foldM (performActionWrap d) (dRunWithTrigger0, rc1,log) cleanUpActions 
                  endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp rc_ d newLogWaterfall_
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
                      rc3 
                      (Just ads2) 
                      calls 
                      (DL.concat [newLogsWaterfall, newLogs2 ,logsBeforeDist,DL.fromList [RunningWaterfall d waterfallKey]])

        -- Custom waterfall execution action from custom dates
        RunWaterfall d wName -> 
          let
            waterfallKey = W.CustomWaterfall wName
          in 
            do
              waterfallToExe <- lookupM waterfallKey waterfallM
              let logsBeforeDist =[ WarningMsg (" No waterfall distribution found on date "++show d++" with waterfall key "++show waterfallKey) 
                                    | Map.notMember waterfallKey waterfallM ]  
              (dAfterWaterfall, rc2, newLogsWaterfall) <- foldM (performActionWrap d) (t,rc,log) waterfallToExe 
              run dAfterWaterfall  rc2 (Just ads) calls  
                  (DL.concat [newLogsWaterfall,DL.fromList (logsBeforeDist ++ [RunningWaterfall d waterfallKey])]) 

        EarnAccInt d accName ->
          let 
            newAcc = Map.adjust (A.depositInt d) accName accMap
          in 
            run (t {accounts = newAcc}) rc (Just ads) calls log

        AccrueFee d feeName -> 
          do 
            fToAcc <- maybeToEither ("Failed to find fee "++feeName) (Map.lookup feeName feeMap)
            newF <- calcDueFee t rc d fToAcc
            let newFeeMap = Map.fromList [(feeName,newF)] <> feeMap
            run (t{fees=newFeeMap}) rc (Just ads) calls log

        ResetLiqProvider d liqName -> 
          case liqProvider t of 
            Nothing -> run t rc (Just ads) calls log
            (Just mLiqProvider) 
              -> let -- update credit 
                    newLiqMap = Map.adjust (updateLiqProvider t rc d) liqName mLiqProvider
                  in
                    run (t{liqProvider = Just newLiqMap}) rc (Just ads) calls log
        ResetLiqProviderRate d liqName -> 
          case liqProvider t of 
            Nothing -> run t rc (Just ads) calls log
            (Just mLiqProvider) 
              -> let -- update rate 
                  newLiqMap = Map.adjust (updateLiqProviderRate t d (fromMaybe [] rates)) liqName mLiqProvider
                in
                  run (t{liqProvider = Just newLiqMap}) rc (Just ads) calls log
        
        DealClosed d ->
          let
            w = Map.findWithDefault [] W.OnClosingDay (waterfall t)
            -- rc = RunContext poolFlowMap rAssump rates  
            logForClosed =  [RunningWaterfall d W.OnClosingDay| not (null w)]
          in 
            do
              newSt <- case dStatus of
                        (PreClosing st) -> return st
                        _ -> Left $ "DealClosed action is not in PreClosing status but got"++ show dStatus
              (newDeal, newRc, newLog) <- foldM (performActionWrap d) (t, rc, log) w  -- `debug` ("ClosingDay Action:"++show w)
              run newDeal{status=newSt} newRc (Just ads) calls  
                  (DL.concat [newLog, DL.fromList ([DealStatusChangeTo d (PreClosing newSt) newSt "By Deal Close"]++logForClosed)]) -- `debug` ("new st at closing"++ show newSt)

        ChangeDealStatusTo d s -> run (t{status=s}) rc (Just ads) calls log

        CalcIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                newRateSwap_rate <- adjustM (updateRateSwapRate t rc rates d) sn rSwap
                newRateSwap_bal <- adjustM (updateRateSwapBal t rc d) sn newRateSwap_rate 
                let newRateSwap_acc = Map.adjust (HE.accrueIRS d) sn newRateSwap_bal
                run (t{rateSwap = Just newRateSwap_acc}) rc (Just ads) calls log

        SettleIRSwap d sn -> 
          case rateSwap t of 
            Nothing -> Left $ " No rate swaps modeled when looking for "++ sn
            Just rSwap ->
              do
                rs0 <- lookupM sn rSwap
                acc <- case HE.rsSettleDates rs0 of 
                          Nothing -> Left $ "No settle date found for "++ sn
                          Just (_, _accName) -> lookupM _accName accMap
                let accBal = A.accBalance acc
                let rs = rs0
                let settleAmt = HE.rsNetCash rs
                let accName = A.accName acc
                case (settleAmt <0, accBal < abs settleAmt) of 
                  (True, True) ->
                    do
                      newAcc <- adjustM (A.draw d accBal (SwapOutSettle sn)) accName accMap
                      let newRsMap = Just $ Map.adjust (HE.payoutIRS d accBal) sn rSwap
                      run (t {accounts = newAcc, rateSwap = newRsMap})
                          rc (Just ads) calls log
                      -- Left $ "Settle Rate Swap Error: "++ show d ++" Insufficient balance to settle "++ sn
                  (True, False) -> 
                    do
                      newAcc <- adjustM (A.draw d (abs settleAmt) (SwapOutSettle sn)) accName  accMap
                      let newRsMap = Just $ Map.adjust (HE.payoutIRS d settleAmt) sn rSwap 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) rc (Just ads) calls log
                  (False, _) -> 
                    let 
                      newAcc = Map.adjust (A.deposit settleAmt d (SwapInSettle sn)) accName accMap
                      newRsMap = Just $ Map.adjust (HE.receiveIRS d) sn rSwap 
                    in 
                      run (t{accounts = newAcc, rateSwap = newRsMap}) rc (Just ads) calls log

        AccrueCapRate d cn -> 
          case rateCap t of 
            Nothing -> Left $ " No rate cap found for "++ cn
            Just rCap ->
              let
                _rates = fromMaybe [] rates
              in 
                do 
                  newRateCap <- adjustM (accrueRC t rc d _rates) cn rCap
                  run (t{rateCap = Just newRateCap}) rc (Just ads) calls log

        InspectDS d dss -> 
          do
            newlog <- inspectListVars t rc d dss 
            run t rc (Just ads) calls $ DL.append log (DL.fromList newlog)
        
        ResetBondRate d bn  -> 
          do 
            bnd <- lookupM bn bndMap
            newBnd <- setBondNewRate t rc d (fromMaybe [] rates) bnd 
            run t{bonds = Map.fromList [(bn,newBnd)] <> bndMap} rc (Just ads) calls log
        
        StepUpBondRate d bn -> 
          do 
            _ <- lookupM bn bndMap
            newBndMap <- adjustM (setBondStepUpRate d (fromMaybe [] rates)) bn bndMap
            run t{bonds = newBndMap } rc (Just ads) calls log
        
        ResetAccRate d accName -> 
          do
            newAccMap <- adjustM 
                          (\a@(A.Account _ _ (Just (A.InvestmentAccount idx spd dp dp1 lastDay _)) _ _)
                            -> do
                                  newRate <- AP.lookupRate (fromMaybe [] rates) (idx,spd) d 
                                  let accWithNewInt = A.depositInt d a
                                  return accWithNewInt { A.accInterest = Just (A.InvestmentAccount idx spd dp dp1 lastDay newRate)})
                          accName accMap
            run t{accounts = newAccMap} rc (Just ads) calls log

        BuildReport sd ed ->
          let 
            cashReport = Rpt.buildCashReport t sd ed 
          in 
            do 
              bsReport <- Rpt.buildBalanceSheet t rc ed
              let newlog = FinancialReport sd ed bsReport cashReport
              run t rc (Just ads) calls log -- `debug` ("new log"++ show ed++ show newlog)
        FireTrigger d cyc n ->  
          do 
            theTrigger <- case (Map.lookup cyc =<< mTrgMap) >>= Map.lookup n of 
                            Nothing -> Left $ "Failed to find trigger "++ n ++" at "++ show cyc ++" for manual fireTrigger"
                            Just trg -> return trg
            (newT, rc@(RunContext newPool newRAssump _), adsFromTrigger, newLogsFromTrigger) <- runEffects (t, rc, ads, DL.empty) d (trgEffects theTrigger)
            let (oldStatus,newStatus) = (status t,status newT)
            let stChangeLogs = DL.fromList [DealStatusChangeTo d oldStatus newStatus "by Manual fireTrigger" |  oldStatus /= newStatus]
            let triggerFired = case mTrgMap of 
                                Nothing -> error "trigger is empty for override" 
                                Just tm -> Map.adjust (Map.adjust (set trgStatusLens True) n) cyc tm
            run newT {triggers = Just triggerFired} (RunContext newPool newRAssump rates) (Just ads) calls log
      
        MakeWhole d spd walTbl -> 
            let 
              schedulePoolFlowMap = 
                case pt of 
                  MultiPool pMap -> Map.map (view (P.poolFutureScheduleCf._Just._1) ) pMap 
                  ResecDeal uDealMap -> Map.map (view (uDealFutureScheduleCf . _Just)) uDealMap
            in 
              do 
                factor <- liftA2
                            (/)
                            (queryCompound t rc d (FutureCurrentPoolBegBalance Nothing)) 
                            (queryCompound t rc d (FutureCurrentSchedulePoolBegBalance Nothing))
                let reduceCfs = Map.map (\f -> (over CF.cashflowTxn (\xs -> CF.scaleTsRow factor <$> xs) f, Nothing ) ) schedulePoolFlowMap -- need to apply with factor and trucate with date
                (runDealWithSchedule,_,_) <- run t (RunContext reduceCfs rAssump rates) (Just ads) calls log
                let bondWal = Map.map (L.calcWalBond d) (bonds runDealWithSchedule) -- `debug` ("Bond schedule flow"++ show (bonds runDealWithSchedule))
                let bondSprd = Map.map 
                                (\x -> (spd + (fromMaybe 0 (lookupTable walTbl Up (fromRational x >)))))
                                bondWal 
                let bondPricingCurve = Map.map 
                                        (\x -> IRateCurve [ TsPoint d x,TsPoint (case ads of [] -> d; as -> getDate (last as)) x])
                                        bondSprd 
                bondPricingResult <- sequenceA $ Map.intersectionWith (flip (L.priceBond d)) (bonds runDealWithSchedule) bondPricingCurve 
                depositBondFlow <- sequenceA $ 
                                    Map.intersectionWith
                                      (\bnd (PriceResult pv _ _ _ _ _ _) -> 
                                        let 
                                          ostBal = L.getCurBalance bnd
                                          prinToPay = min pv ostBal
                                          intToPay = max 0 (pv - prinToPay)
                                        in 
                                          (pay d DuePrincipal prinToPay) =<< (pay d DueResidual intToPay bnd))
                                      bndMap
                                      bondPricingResult
                run t {bonds = depositBondFlow, status = Ended (Just d)} (RunContext Map.empty rAssump rates) (Just []) calls log
        
        FundBond d Nothing bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              bnd0 <- lookupM bName bndMap
              bndFunded <- draw d fundAmt (FundWith bName fundAmt) bnd0
              run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                  rc (Just ads) calls log

        FundBond d (Just p) bName accName fundAmt ->
          let 
            newAcc = Map.adjust (A.deposit fundAmt d (FundWith bName fundAmt)) accName accMap
          in 
            do
              flag <- testPre d t rc p
              case flag of
                False -> run t rc (Just ads) calls log
                True -> 
                  do
                    bnd0 <- lookupM bName bndMap
                    bndFunded <- draw d fundAmt (FundWith bName fundAmt) bnd0
                    run t{accounts = newAcc, bonds = Map.insert bName bndFunded bndMap}
                        rc (Just ads) calls log
          

        IssueBond d Nothing bGroupName accName bnd mBal mRate -> 
          run t rc (Just ((IssueBond d (Just (Always True)) bGroupName accName bnd mBal mRate):ads)) calls log
        
        IssueBond d (Just p) bGroupName accName bnd mBal mRate ->
            do 
              flag <- testPre d t rc p
              case flag of
                False -> run t rc (Just ads) calls log
                True -> let 
                          newBndName = L.bndName bnd
                        in
                          do
                            newBalance <- case mBal of
                                            Just _q -> queryCompound t rc d (patchDateToStats d _q)  
                                            Nothing -> Right . toRational $ L.originBalance (L.bndOriginInfo bnd)
                            newRate <- case mRate of 
                                        Just _q -> queryCompound t rc d (patchDateToStats d _q)
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
                            let newAcc = Map.adjust 
                                          (A.deposit issuanceProceeds d (IssuanceProceeds newBndName))
                                          accName
                                          accMap
                            run t{bonds = newBonds, accounts = newAcc} rc (Just ads) calls log
        RefiBondRate d accName bName iInfo ->
          let
              lstDate = case ads of [] -> d; as -> getDate (last as)
              isResetActionEvent (ResetBondRate _ bName ) = False 
              isResetActionEvent _ = True
              filteredAds = filter isResetActionEvent ads
              newRate = L.getBeginRate iInfo
          in 
              do 
                bnd0 <- lookupM bName bndMap
                nBnd <- calcDueInt t rc d bnd0
                let dueIntToPay = L.getTotalDueInt nBnd
                acc <- lookupM accName accMap
                let actualPayout = min (A.accBalance acc) dueIntToPay
                bnd1 <- pay d (DueTotalOf [DueInterest Nothing, DueArrears]) actualPayout nBnd
                let newBnd = set L.bndIntLens iInfo bnd1 
                let resetDates = L.buildRateResetDates newBnd d lstDate 
                let bResetActions = [ ResetBondRate d' bName | d' <- resetDates ]
                newAccMap <- adjustM (draw d actualPayout (PayInt [bName])) accName accMap
                let newBndMap = Map.insert bName (newBnd {L.bndRate = newRate, L.bndDueIntDate = Just d ,L.bndLastIntPay = Just d}) bndMap
                let newAds = sortBy sortActionOnDate $ filteredAds ++ bResetActions
                run t{bonds = newBndMap, accounts = newAccMap} rc (Just newAds) calls log
            
        RefiBond d accName bnd -> Left "Undefined action: RefiBond"

        TestCall d ->
          let 
            timeBasedTests::[Pre] = snd (fromMaybe ([],[]) calls)
          in
            do 
              flags::[Bool] <- traverse (testPre d t rc) timeBasedTests
              case any id flags of
                True -> 
                  let 
                    newStLogs
		                  | null cleanUpActions = DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call"]
                      | otherwise = DL.fromList [DealStatusChangeTo d dStatus Called "by Date-Based Call", RunningWaterfall d W.CleanUp]
                  in  
                    do 
                      (dealAfterCleanUp, rc_, newLogWaterfall_ ) <- foldM (performActionWrap d) (t, rc, log) cleanUpActions
                      endingLogs <- Rpt.patchFinancialReports dealAfterCleanUp rc_ d newLogWaterfall_
                      return (dealAfterCleanUp
                              , DL.snoc (endingLogs `DL.append` newStLogs) (EndRun (Just d) "Clean Up")
                              , (runPoolFlow rc_))
                _ -> run t rc (Just ads) calls log

        StopRunTest d pres -> 
          do
              flags::[Bool] <- sequenceA $ [ (testPre d t rc pre) | pre <- pres ]
              case all id flags of
                True -> return (t, DL.snoc log (EndRun (Just d) ("Stop Run Test by:"++ show (zip pres flags))), poolFlowMap)
                _ -> run t rc (Just ads) calls log


        _ -> Left $ "Failed to match action on Date"++ show ad

       where
         cleanUpActions = Map.findWithDefault [] W.CleanUp (waterfall t) -- `debug` ("Running AD"++show(ad))
         remainCollectionNum = Map.elems $ Map.map (\(x,_) -> CF.sizeCashFlowFrame x ) poolFlowMap
         futureCashToCollectFlag = and $ Map.elems $ Map.map (\(pcf,_) -> all CF.isEmptyRow2 (view CF.cashflowTxn pcf)) poolFlowMap

run t (RunContext empty _ _) _ _ log = return (t, log ,empty) -- `debug` ("End with pool CF is []")
