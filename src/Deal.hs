{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Deal (run,getInits,runDeal,ExpectReturn(..)
            ,performAction
            ,populateDealDates
            ,calcTargetAmount,updateLiqProvider
            ,projAssetUnion,priceAssetUnion
            ,runPoolType,PoolType
            ,ActionOnDate(..),DateDesp(..)
            ,changeDealStatus
            ) where

import Control.Parallel.Strategies
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Analytics
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Reports as Rpt
import qualified AssetClass.AssetBase as ACM
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan
import AssetClass.Installment
import AssetClass.MixedAsset

import qualified Call as C
import qualified InterestRate as IR
import Deal.DealBase
import Deal.DealQuery
import Deal.DealAction
import Deal.DealCollection
import Deal.DealRun
import qualified Deal.DealValidation as V
import Stmt
import Lib
import Util
import DateUtil
import Types
import Revolving
import Triggers

import qualified Data.Map as Map hiding (mapEither)
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import qualified Data.DList as DL
import Data.Fixed
import Data.Time.Clock
import Data.Maybe
import Data.Either
import Data.Aeson hiding (json)
-- import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Loops (allM,anyM)
import Control.Applicative (liftA2)

import Debug.Trace
import Cashflow (buildBegTsRow)
import Assumptions (NonPerfAssumption(NonPerfAssumption),lookupRate0)
import Asset ()
import Pool (issuanceStat)
import qualified Types as P
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Either.Utils
import InterestRate (calcInt)
import Liability (getDayCountFromInfo,getTxnRate)
import Hedge (RateCap(..),RateSwapBase(..),RateSwap(rsRefBalance))
import qualified Hedge as HE

debug = flip trace


updateSrtRate :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> HE.SRT -> Either String HE.SRT
updateSrtRate t d ras srt@HE.SRT{HE.srtPremiumType = rt} 
    = do 
        r <- AP.applyFloatRate2 rt d ras 
        return srt { HE.srtPremiumRate = r }


accrueSrt :: Ast.Asset a => TestDeal a -> Date -> HE.SRT -> Either String HE.SRT
accrueSrt t d srt@HE.SRT{ HE.srtDuePremium = duePrem, HE.srtRefBalance = bal, HE.srtPremiumRate = rate
                        , HE.srtDuePremiumDate = mDueDate,  HE.srtType = st
                        , HE.srtStart = sd } 
  = do 
      newBal <- case st of
                  HE.SrtByEndDay ds dp -> queryCompound t d (patchDateToStats d ds)
      let newPremium = duePrem +  calcInt (fromRational newBal) (fromMaybe sd mDueDate) d rate DC_ACT_365F
      let accrueInt = calcInt (HE.srtRefBalance srt + duePrem) (fromMaybe d (HE.srtDuePremiumDate srt)) d (HE.srtPremiumRate srt) DC_ACT_365F
      return srt { HE.srtRefBalance = fromRational newBal, HE.srtDuePremium = newPremium, HE.srtDuePremiumDate = Just d}


-- ^ test if a clean up call should be fired
testCall :: Ast.Asset a => TestDeal a -> Date -> C.CallOption -> Either String Bool 
testCall t d opt = 
    case opt of 
       C.PoolBalance x -> (< x) . fromRational <$> queryCompound t d (FutureCurrentPoolBalance Nothing)
       C.BondBalance x -> (< x) . fromRational <$> queryCompound t d CurrentBondBalance
       C.PoolFactor x ->  (< x) <$> queryCompound t d (FutureCurrentPoolFactor d Nothing)
       C.BondFactor x ->  (< x) <$> queryCompound t d BondFactor
       C.OnDate x -> Right $ x == d 
       C.AfterDate x -> Right $ d > x
       C.And xs -> allM (testCall t d) xs
       C.Or xs -> anyM (testCall t d) xs
       C.Pre pre -> testPre d t pre
       _ -> Left ("failed to find call options"++ show opt)


queryTrigger :: Ast.Asset a => TestDeal a -> DealCycle -> [Trigger]
queryTrigger t@TestDeal{ triggers = trgs } wt 
  = case trgs of 
      Nothing -> []
      Just _trgs -> maybe [] Map.elems $ Map.lookup wt _trgs

-- ^ test triggers in the deal and add a log if deal status changed
changeDealStatus:: Ast.Asset a => (Date,String)-> DealStatus -> TestDeal a -> (Maybe ResultComponent, TestDeal a)
changeDealStatus _ _ t@TestDeal{status=Ended _} = (Nothing, t) 
changeDealStatus (d,why) newSt t@TestDeal{status=oldSt} 
  | newSt /= oldSt = (Just (DealStatusChangeTo d oldSt newSt why), t {status=newSt})
  | otherwise = (Just (DealStatusChangeTo d oldSt newSt ("Duplicate status change: "++why)), t) 


-- reserved for future used
data ExpectReturn = DealLogs
                  | AssetLevelFlow
                  deriving (Show,Generic,Ord,Eq)


priceBondIrr :: AP.IrrType -> [Txn] -> Either String (Rate, [Txn])
-- No projected transaction, use history cashflow only
priceBondIrr AP.BuyBond {} [] = Left "No transaction to buy the bond" 
priceBondIrr (AP.HoldingBond historyCash _ _) [] 
  = let 
      (ds,vs) = unzip historyCash
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
    in 
      do 
        irr <- Analytics.calcIRR ds vs
        return (irr, txns')
-- Projected transaction and hold to maturity
priceBondIrr (AP.HoldingBond historyCash holding Nothing) (txn:txns)
  = let 
      begBal = getTxnBegBalance txn
      holdingPct = divideBB holding begBal
      bProjectedTxn = scaleTxn holdingPct <$> (txn:txns) 
      (ds,vs) = unzip historyCash
      (ds2,vs2) = (getDate <$> bProjectedTxn, getTxnAmt <$> bProjectedTxn) -- `debug` ("projected txn position"++ show bProjectedTxn)
      
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
    in 
      do 
        irr <- Analytics.calcIRR (ds++ds2) (vs++vs2) -- `debug` ("projected holding"++ show (ds2,vs2))
        return (irr, txns' ++ bProjectedTxn)

-- TODO: need to use DC from bond
-- Projected transaction and sell at a Date
priceBondIrr (AP.HoldingBond historyCash holding (Just (sellDate, sellPricingMethod))) (txn:txns)
  = let 
      -- history cash
      (ds,vs) = unzip historyCash
      txns' = [ BondTxn d 0 0 0 0 v 0 0 Nothing Types.Empty | (d,v) <- historyCash ]
      
      begBal = getTxnBegBalance txn
      holdingPct = toRational $ holding / begBal
      -- assume cashflow of sell date belongs to seller(owner)
      (bProjectedTxn',futureFlow') = splitByDate (txn:txns) sellDate EqToLeft
      (bProjectedTxn,futureFlow) = ((scaleTxn holdingPct) <$> bProjectedTxn',(scaleTxn holdingPct) <$> futureFlow')
      -- projected cash
      (ds2,vs2) = (getDate <$> bProjectedTxn, getTxnAmt <$> bProjectedTxn)
      -- accrued interest
      accruedInt = L.backoutAccruedInt sellDate epocDate (bProjectedTxn++futureFlow)
      (ds3,vs3) = (sellDate, accruedInt)  -- `debug` ("accrued interest"++ show (accruedInt,sellDate))
      -- sell price 
      sellPrice = case sellPricingMethod of 
                    BondBalanceFactor f -> case bProjectedTxn of 
                                            [] -> mulBR begBal (f * holdingPct) 
                                            _txns -> mulBR (getTxnBalance (last _txns)) f
      (ds4,vs4) = (sellDate,  sellPrice)  -- `debug` ("sale price, date"++ show (sellPrice,sellDate))
    in 
      do 
        irr <- Analytics.calcIRR (ds++ds2++[ds3]++[ds4]) (vs++vs2++[vs3]++[vs4])
        return (irr, txns'++ bProjectedTxn++ [BondTxn sellDate 0 vs3 sellPrice 0 (sellPrice+vs3) 0 0 Nothing Types.Empty]) 

-- Buy and hold to maturity
priceBondIrr (AP.BuyBond dateToBuy bPricingMethod (AP.ByCash cash) Nothing) txns
  | null futureFlow' = Left "No transaction to buy bond"
  | otherwise
    = let 
      -- balance of bond on buy date
      nextTxn = head futureFlow'
      balAsBuyDate = getTxnBegBalance nextTxn
      buyPrice = case bPricingMethod of 
                    BondBalanceFactor f -> mulBR balAsBuyDate f 
      buyPaidOut = min buyPrice cash
      buyPct = divideBB buyPaidOut buyPrice
      boughtTxns = scaleTxn buyPct <$> futureFlow'
      -- buy price (including accrued interest)

      accuredInt = let
                     --TODO what about interest over interest
                     accruedInt' = calcInt balAsBuyDate dateToBuy (getDate nextTxn) (getTxnRate nextTxn) DC_ACT_365F
                     x = nextTxn
                     totalInt' = (fromMaybe 0) <$> [(preview (_BondTxn . _3 ) x), (preview (_BondTxn . _7 ) x), (preview (_BondTxn . _8 ) x)]
                   in
                     sum(totalInt') - accruedInt'

      (ds1, vs1) = (dateToBuy, negate (buyPaidOut + accuredInt))
      (ds2, vs2) = (getDate <$> futureFlow', getTxnAmt <$> boughtTxns)
    in 
      do 
        irr <- Analytics.calcIRR (ds1:ds2) (vs1:vs2)
        return (irr, (BondTxn dateToBuy 0 (negate accuredInt) (negate buyPaidOut) 0 vs1 0 0 Nothing Types.Empty):boughtTxns)
  where 
    -- assume cashflow of buy date belongs to seller(owner)
    (bProjectedTxn',futureFlow') = splitByDate txns dateToBuy EqToLeft


priceBonds :: Ast.Asset a => TestDeal a -> AP.BondPricingInput -> Either String (Map.Map String PriceResult)
-- Price bond via discount future cashflow
priceBonds t (AP.DiscountCurve d dc) = Right $ Map.map (L.priceBond d dc) (viewBondsInMap t)
-- Run Z-Spread
priceBonds t@TestDeal {bonds = bndMap} (AP.RunZSpread curve bondPrices) 
  = sequenceA $ 
      Map.mapWithKey 
        (\bn (pd,price)-> ZSpread <$> L.calcZspread (price,pd) (bndMap Map.! bn) curve)
        bondPrices
-- Calc Irr of bonds 
priceBonds t@TestDeal {bonds = bndMap} (AP.IrrInput bMapInput) 
  = let
      -- Date 
      d = getNextBondPayDate t
      -- get projected bond txn
      projectedTxns xs = snd $ splitByDate xs d EqToRight 
      -- (Maybe Bond,IrrType)
      bndMap' = Map.mapWithKey (\k v -> (getBondByName t True k, v)) bMapInput
      -- (Rate, [(date, cash)])
      bndMap'' = Map.mapWithKey (\bName (Just b, v) -> 
                                  do 
                                    let _irrTxns = projectedTxns (getAllTxns b)
                                    (_irr, flows) <- priceBondIrr v _irrTxns
                                    return (IrrResult (fromRational _irr) flows))
                                bndMap'
    in 
      sequenceA bndMap''



-- <Legacy Test>, <Test on dates>
runDeal :: Ast.Asset a => TestDeal a -> S.Set ExpectReturn -> Maybe AP.ApplyAssumptionType-> AP.NonPerfAssumption
        -> Either String (TestDeal a
                         , Map.Map PoolId CF.CashFlowFrame
                         , [ResultComponent]
                         , Map.Map String PriceResult
                         , Map.Map PoolId CF.PoolCashflow)
runDeal t er perfAssumps nonPerfAssumps@AP.NonPerfAssumption{AP.callWhen = opts ,AP.pricing = mPricing ,AP.revolving = mRevolving ,AP.interest = mInterest} 
  | not runFlag = Left $ intercalate ";" $ show <$> valLogs 
  | otherwise 
    = do 
        (newT, ads, pcf, unStressPcf) <- getInits er t perfAssumps (Just nonPerfAssumps)  
        (_finalDeal, logs, osPoolFlow) <- run newT 
                                              pcf
                                              (Just ads) 
                                              mInterest
                                              (AP.readCallOptions <$> opts)
                                              mRevolvingCtx
                                              DL.empty
    -- prepare deal with expected return
        let finalDeal = prepareDeal er _finalDeal
    -- extract pool cash collected to deal
        let poolFlowUsedNoEmpty = Map.map 
                                (over CF.cashflowTxn CF.dropTailEmptyTxns) 
                                (getAllCollectedFrame finalDeal Nothing)
        let poolFlowUnUsed = osPoolFlow & mapped . _1 . CF.cashflowTxn %~ CF.dropTailEmptyTxns
                                & mapped . _2 . _Just . each . CF.cashflowTxn %~ CF.dropTailEmptyTxns
        bndPricing <- case mPricing of 
                        (Just p) -> priceBonds finalDeal p 
                        Nothing -> Right Map.empty
        return (finalDeal
                 , poolFlowUsedNoEmpty
                 , getRunResult finalDeal ++ V.validateRun finalDeal ++ DL.toList (DL.append logs (unCollectedPoolFlowWarning poolFlowUnUsed))
                 , bndPricing
                 , poolFlowUnUsed
               )
    where
      (runFlag, valLogs) = V.validateReq t nonPerfAssumps 
      -- getinits() will get (new deal snapshot, actions, pool cashflows, unstressed pool cashflow)
      -- extract Revolving Assumption
      mRevolvingCtx = case mRevolving of
                        Nothing -> Nothing
                        Just (AP.AvailableAssets rp rperf) -> Just (Map.fromList [("Consol", (rp, rperf))])
                        Just (AP.AvailableAssetsBy rMap) -> Just rMap
      unCollectedPoolFlowWarning pMap = let
                                           countMap = Map.map (CF.sizeCashFlowFrame . view _1) pMap 
                                        in 
                      if sum (Map.elems countMap) > 0 then 
                                          DL.singleton $ WarningMsg $ "Oustanding pool cashflow hasn't been collected yet"++ show countMap
                                        else
                      DL.empty

      -- run() is a recusive function loop over all actions till deal end conditions are met
      
-- | get bond principal and interest shortfalls from a deal
getRunResult :: Ast.Asset a => TestDeal a -> [ResultComponent]
getRunResult t = let 
                   bs = viewDealAllBonds t  
                   os_bn_b = [ BondOutstanding (L.bndName _b) (L.getCurBalance _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ] 
                   os_bn_i = [ BondOutstandingInt (L.bndName _b) (L.getTotalDueInt _b) (getBondBegBal t (L.bndName _b)) | _b <- bs ]
                 in 
                   os_bn_i ++ os_bn_b 


-- | consolidate pool cashflow 
-- consolidate bond cashflow and patch factor
prepareDeal :: Ast.Asset a => S.Set ExpectReturn -> TestDeal a -> TestDeal a
prepareDeal er t@TestDeal {bonds = bndMap ,pool = poolType } 
  = let 
      consolePoolFlowFn = over CF.cashflowTxn CF.dropTailEmptyTxns
      rmAssetLevelFn xs 
        | S.member AssetLevelFlow er = xs
        | otherwise = []
    in 
      t {bonds = Map.map (L.patchBondFactor . L.consolStmt) bndMap
         ,pool = poolType & over (_MultiPool . mapped . P.poolFutureCf . _Just ._1) consolePoolFlowFn 
                          & over (_ResecDeal . mapped . uDealFutureCf . _Just) consolePoolFlowFn
                          & over (_MultiPool . mapped . P.poolFutureCf . _Just . _2 . _Just) rmAssetLevelFn 
    }

runPoolType :: Ast.Asset a => Bool -> PoolType a -> Maybe AP.ApplyAssumptionType 
            -> Maybe AP.NonPerfAssumption -> Either String (Map.Map PoolId CF.PoolCashflow)

runPoolType flag (MultiPool pm) (Just poolAssumpType) mNonPerfAssump
  = let 
      rateAssump = AP.interest =<< mNonPerfAssump
      calcPoolCashflow (AP.ByName assumpMap) pid v = P.runPool v (AP.PoolLevel <$> Map.lookup pid assumpMap) rateAssump     
      calcPoolCashflow (AP.ByPoolId assumpMap) pid v = P.runPool v (Map.lookup pid assumpMap) rateAssump
      calcPoolCashflow poolAssump pid v = P.runPool v (Just poolAssump) rateAssump
    in
      sequenceA $
        Map.mapWithKey 
          (\k v -> 
            let 
              poolBegStats = P.issuanceStat v
            in
              do 
                assetCfs <- calcPoolCashflow poolAssumpType k v
                let (poolCf,_) = P.aggPool poolBegStats assetCfs
                return (poolCf, if flag then 
                                  Just $ fst <$> assetCfs
                                else
                                  Nothing))
        pm

runPoolType flag (MultiPool pm) mAssumps mNonPerfAssump
  = sequenceA $ 
      Map.map (\p -> 
                do
                  assetFlows <- P.runPool p mAssumps (AP.interest =<< mNonPerfAssump)
                  let (poolCf, poolStatMap) = P.aggPool (P.issuanceStat p) assetFlows
                  return (poolCf, if flag then 
                                    Just $ fst <$> assetFlows
                                  else
                                    Nothing))
              pm

runPoolType flag (ResecDeal dm) mAssumps mNonPerfAssump
  = 
    let 
      assumpMap =  Map.mapWithKey (\_ (UnderlyingDeal uDeal _ _ _) -> 
                              let 
                                 dName = name uDeal -- `debug` ("Getting name of underlying deal:"++ (name uDeal))
                                 mAssump = case mAssumps of 
                                             Just (AP.ByDealName assumpMap) -> Map.lookup dName assumpMap
                                             _ -> Nothing
                               in 
                                 (uDeal, mAssump))
                             dm
      ranMap =   Map.mapWithKey (\(DealBondFlow dn bn sd pct) (uDeal, mAssump) -> 
                                  let
                                    (poolAssump,dealAssump) = case mAssump of 
                                                                Nothing -> (Nothing, AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                                                                Just (_poolAssump, _dealAssump) -> (Just _poolAssump, _dealAssump)
                                  in
                                    do 
                                      (dealRunned, _, _, _,_) <- runDeal uDeal (S.fromList []) poolAssump dealAssump
                                      let bondFlow = cutBy Inc Future sd $ concat $ Map.elems $ Map.map (DL.toList . Stmt.getTxns) $ getBondStmtByName dealRunned (Just [bn]) 
                                      let bondFlowRated = (\(BondTxn d b i p r c di dioi f t) -> CF.BondFlow d b p i) <$> Stmt.scaleByFactor pct bondFlow 
                                      return (CF.CashFlowFrame (0,sd,Nothing) bondFlowRated, Nothing))
                                 assumpMap
    in
      sequenceA ranMap
 

-- ^ patch issuance balance for PreClosing Deal
patchIssuanceBalance :: Ast.Asset a => DealStatus -> Map.Map PoolId Balance -> PoolType a -> PoolType a
-- patchIssuanceBalance (Warehousing _) balM pt = patchIssuanceBalance (PreClosing Amortizing) balM pt
patchIssuanceBalance (PreClosing _ ) balM pt =
  case pt of 
    MultiPool pM -> MultiPool $ Map.mapWithKey 
                      (\k v -> over P.poolIssuanceStat (Map.insert IssuanceBalance (Map.findWithDefault 0.0 k balM)) v)
                  pM
    ResecDeal pM -> ResecDeal pM  --TODO patch balance for resec deal
    
patchIssuanceBalance _ bal p = p -- `debug` ("NO patching ?")


patchScheduleFlow :: Ast.Asset a => Map.Map PoolId CF.PoolCashflow -> PoolType a -> PoolType a
patchScheduleFlow flowM pt = 
  case pt of
    MultiPool pM -> MultiPool $ Map.intersectionWith (set (P.poolFutureScheduleCf . _Just)) flowM pM
    ResecDeal pM -> ResecDeal pM

patchRuntimeBal :: Ast.Asset a => Map.Map PoolId Balance -> PoolType a -> PoolType a
patchRuntimeBal balMap (MultiPool pM) 
  = MultiPool $
      Map.mapWithKey
        (\k p -> over P.poolIssuanceStat 
                      (Map.insert RuntimeCurrentPoolBalance (Map.findWithDefault 0.0 k balMap)) 
                      p)
        pM

patchRuntimeBal balMap pt = pt


getInits :: Ast.Asset a => S.Set ExpectReturn -> TestDeal a -> Maybe AP.ApplyAssumptionType -> Maybe AP.NonPerfAssumption 
         -> Either String (TestDeal a,[ActionOnDate], Map.Map PoolId CF.PoolCashflow, Map.Map PoolId CF.PoolCashflow)
getInits er t@TestDeal{fees=feeMap,pool=thePool,status=status,bonds=bndMap,stats=_stats} mAssumps mNonPerfAssump =
  let 
    expandInspect sd ed (AP.InspectPt dp ds) = [ InspectDS _d [ds] | _d <- genSerialDatesTill2 II sd dp ed ]
    expandInspect sd ed (AP.InspectRpt dp dss) = [ InspectDS _d dss | _d <- genSerialDatesTill2 II sd dp ed ] 
  in 
    do 
      (startDate,closingDate,firstPayDate,pActionDates,bActionDates,endDate,custWdates) <- populateDealDates (dates t) status

      let intEarnDates = A.buildEarnIntAction (Map.elems (accounts t)) endDate [] 
      let intAccRateResetDates = (A.buildRateResetDates endDate) <$> (Map.elems (accounts t))
      let iAccIntDates = [ EarnAccInt _d accName | (accName,accIntDates) <- intEarnDates , _d <- accIntDates ] 
      let iAccRateResetDates = concat [ [ResetAccRate _d accName | _d <- _ds] | rst@(Just (accName, _ds)) <- intAccRateResetDates, isJust rst ]
    
      --fee accrue dates 
      let _feeAccrueDates = F.buildFeeAccrueAction (Map.elems feeMap) endDate [] 
      let feeAccrueDates = [ AccrueFee _d _feeName | (_feeName,feeAccureDates) <- _feeAccrueDates , _d <- feeAccureDates ]
    --liquidation facility
      let liqResetDates = case liqProvider t of 
                            Nothing -> []
                            Just mLiqProvider -> 
                                let 
                                  _liqResetDates = CE.buildLiqResetAction (Map.elems mLiqProvider) endDate []
                                  _liqRateResetDates = CE.buildLiqRateResetAction (Map.elems mLiqProvider) endDate []
                                in 
                                  [ ResetLiqProvider _d _liqName |(_liqName,__liqResetDates) <- _liqResetDates , _d <- __liqResetDates ]
                                  ++ 
                                  [ ResetLiqProviderRate _d _liqName |(_liqName,__liqResetDates) <- _liqRateResetDates , _d <- __liqResetDates ]                            
    --inspect dates 
      let inspectDates = case mNonPerfAssump of
                          Just AP.NonPerfAssumption{AP.inspectOn = Just inspectList } -> concatMap  (expandInspect startDate endDate) inspectList
                          _ -> []
    
      let financialRptDates = case mNonPerfAssump of 
                                Just AP.NonPerfAssumption{AP.buildFinancialReport= Just dp } 
                                  -> case genSerialDatesTill2 II startDate dp endDate of
				       (s:_ds) -> [ BuildReport _sd _ed  | (_sd,_ed) <- zip (s:_ds) _ds ]
				       [] -> []
                                _ -> []

      let irUpdateSwapDates = case rateSwap t of
                                Nothing -> []
                                Just rsm -> Map.elems $ Map.mapWithKey 
                                                         (\k x -> let 
                                                                   resetDs = genSerialDatesTill2 EE (HE.rsStartDate x) (HE.rsUpdateDates x) endDate
                                                                  in 
                                                                   flip CalcIRSwap k <$> resetDs)
                                                         rsm
      let irSettleSwapDates = case rateSwap t of
                                Nothing -> []
                                Just rsm -> Map.elems $ Map.mapWithKey 
                                                          (\k x@HE.RateSwap{ HE.rsSettleDates = sDates} ->
                                                            case sDates of 
                                                              Nothing -> []
                                                              Just (sdp,_) ->
                                                                let 
                                                                  resetDs = genSerialDatesTill2 EE (HE.rsStartDate x) sdp endDate
                                                                in 
                                                                  flip SettleIRSwap k <$> resetDs)
                                                          rsm
      let rateCapSettleDates = case rateCap t of 
                                 Nothing -> []
                                 Just rcM -> Map.elems $ Map.mapWithKey 
                                                           (\k x -> let 
                                                                      resetDs = genSerialDatesTill2 EE (HE.rcStartDate x) (HE.rcSettleDates x) endDate
                                                                    in 
                                                                      flip AccrueCapRate k <$> resetDs)
                                                           rcM
    -- bond rate resets 
      let bndRateResets = let 
                            bndWithDate = Map.toList $ Map.map 
                                                      (\b -> L.buildRateResetDates b closingDate endDate) 
                                                      bndMap
                          in 
                            [ ResetBondRate bdate bn | (bn, bdates) <- bndWithDate , bdate <- bdates ] 

    -- bond step ups events
      let bndStepUpDates = let 
                             bndWithDate = Map.toList $ Map.map 
                                                       (\b -> L.buildStepUpDates b closingDate endDate) 
                                                       bndMap
                           in
                             [ StepUpBondRate bdate bn  | (bn, bdates) <- bndWithDate , bdate <- bdates ] 

    -- mannual triggers 
      let mannualTrigger = case mNonPerfAssump of 
                            Just AP.NonPerfAssumption{AP.fireTrigger = Just evts} -> [ FireTrigger d cycle n | (d,cycle,n) <- evts]
                            _ -> []

    -- make whole assumption
      let makeWholeDate = case mNonPerfAssump of
                            Just AP.NonPerfAssumption{AP.makeWholeWhen = Just (_d,_s,_t)} -> [MakeWhole _d _s _t]
                            _ -> [] 

    -- issue bonds in the future 
      let bondIssuePlan = case mNonPerfAssump of 
                            Just AP.NonPerfAssumption{AP.issueBondSchedule = Just bndPlan} 
                              -> [ IssueBond _d mPre bGroupName accName b mBal mRate | TsPoint _d (AP.IssueBondEvent mPre bGroupName accName b mBal mRate) <- bndPlan]
                                  ++ [FundBond _d mPre bName accName amount | TsPoint _d (AP.FundingBondEvent mPre bName accName amount) <- bndPlan]
                            _ -> []

    -- refinance bonds in the future 
      let bondRefiPlan = case mNonPerfAssump of 
                        Just AP.NonPerfAssumption{AP.refinance = Just bndPlan} 
                          -> [ RefiBondRate _d accName bName iInfo | TsPoint _d (AP.RefiRate accName bName iInfo) <- bndPlan]
                            ++ [ RefiBond _d accName bnd | TsPoint _d (AP.RefiBond accName bnd) <- bndPlan] 
                             
                        _ -> []

      let extractTestDates (AP.CallOnDates dp _) = [TestCall x | x <- genSerialDatesTill2 EE startDate dp endDate ]
      let extractTestDates _ = []
    -- extractTestDates (AP.CallOptions opts) = concat [ extractTestDates opt | opt <- opts ]
    -- call test dates 
      let callDates = case mNonPerfAssump of
                        Just AP.NonPerfAssumption{AP.callWhen = Just callOpts}
                          -> concat [ extractTestDates callOpt | callOpt <- callOpts ]
                        _ -> []
      let stopTestDates = case mNonPerfAssump of
                            Just AP.NonPerfAssumption{AP.stopRunBy = Just (AP.StopByPre dp pres)} 
                              -> [StopRunTest d pres | d <- genSerialDatesTill2 EI startDate dp endDate]
                            _ -> []
      let allActionDates = let 
                         __actionDates = let 
                                          a = concat [bActionDates,pActionDates,custWdates,iAccIntDates,makeWholeDate
                                                     ,feeAccrueDates,liqResetDates,mannualTrigger,concat rateCapSettleDates
                                                     ,concat irUpdateSwapDates, concat irSettleSwapDates ,inspectDates, bndRateResets,financialRptDates, stopTestDates
                                                     ,bondIssuePlan,bondRefiPlan,callDates, iAccRateResetDates 
                                                     ,bndStepUpDates] 
                                        in
                                          case (dates t,status) of 
                                            (PreClosingDates {}, PreClosing _) -> sortBy sortActionOnDate $ DealClosed closingDate:a 
                                            _ -> sortBy sortActionOnDate a
                         _actionDates = __actionDates++[HitStatedMaturity endDate]
                       in 
                         case mNonPerfAssump of
                           Just AP.NonPerfAssumption{AP.stopRunBy = Just (AP.StopByDate d)} -> cutBy Exc Past d __actionDates ++ [StopRunFlag d]
                           _ -> _actionDates  
     
      let newFeeMap = case mNonPerfAssump of
                        Nothing -> feeMap
                        Just AP.NonPerfAssumption{AP.projectedExpense = Nothing } -> feeMap
                        Just AP.NonPerfAssumption{AP.projectedExpense = Just pairs } 
                          ->   foldr  (\(feeName,feeFlow) accM -> Map.adjust (\v -> v {F.feeType = F.FeeFlow feeFlow}) feeName accM)  feeMap pairs
      pCfM <- runPoolType True thePool mAssumps mNonPerfAssump
      pScheduleCfM <- runPoolType True thePool Nothing mNonPerfAssump
      let aggDates = getDates pActionDates
      let pCollectionCfAfterCutoff = Map.map 
                                       (\(pCf, mAssetFlow) -> 
                                           (CF.cutoffCashflow startDate aggDates pCf
	                                   ,(\xs -> [ CF.cutoffCashflow startDate aggDates x | x <- xs ] ) <$> mAssetFlow)
	                               )
                                       pCfM

      let pUnstressedAfterCutoff = Map.map 
                                       (\(pCf, mAssetFlow) -> 
                                           (CF.cutoffCashflow startDate aggDates pCf
                                           ,(\xs -> [ CF.cutoffCashflow startDate aggDates x | x <- xs ]) <$> mAssetFlow)
                                       )
                                       pScheduleCfM

      let poolWithSchedule = patchScheduleFlow pUnstressedAfterCutoff thePool
      let poolWithIssuanceBalance = patchIssuanceBalance 
                                      status 
                                      ((\(_pflow,_) -> CF.getBegBalCashFlowFrame _pflow) <$> pCollectionCfAfterCutoff)
                                      poolWithSchedule
      let poolWithRunPoolBalance = patchRuntimeBal 
                                     (Map.map (\(CF.CashFlowFrame (b,_,_) _,_) -> b) pCollectionCfAfterCutoff) 
                                     poolWithIssuanceBalance

      let newStat 
            | (isPreClosing t) =  _stats & (over _4) (`Map.union` (Map.fromList [(BondPaidPeriod,0),(PoolCollectedPeriod,0)]))
            | otherwise = _stats

      return (t {fees = newFeeMap , pool = poolWithRunPoolBalance , stats = newStat}
             , allActionDates
             , pCollectionCfAfterCutoff
             , pUnstressedAfterCutoff)

$(deriveJSON defaultOptions ''ExpectReturn)
