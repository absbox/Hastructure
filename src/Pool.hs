{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Pool (Pool(..),aggPool
       ,getIssuanceField ,poolIssuanceStat ,poolFutureScheduleCf
       ,poolBegStats,calcLiquidationAmount,pricingPoolFlow
       ,futureScheduleCfLens,futureCfLens, poolFutureCf
       ,runPool
) where


import Lib (Period(..)
           ,Ts(..),periodRateFromAnnualRate,toDate
           ,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mkRateTs,daysBetween, )

import Control.Parallel.Strategies
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified Analytics as AN
import qualified AssetClass.AssetBase as ACM 
import AssetClass.Mortgage
import AssetClass.AssetCashflow
import Asset (Asset(..))
import qualified Data.Map as Map

import Data.Ratio
import qualified Data.Set as S
import Data.List
import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current)

import Data.Maybe
import Control.Lens
import Control.Lens.TH
import Assumptions (ApplyAssumptionType)

import Util
import Cashflow (CashFlowFrame)
import qualified Stmt as CF
import Stmt
import Debug.Trace
debug = flip trace


data Pool a = Pool {assets :: [a]                                           -- ^ a list of assets in the pool
                   ,futureCf :: Maybe CF.PoolCashflow                       -- ^ collected cashflow from the assets in the pool
                   ,futureScheduleCf :: Maybe CF.PoolCashflow               -- ^ collected un-stressed cashflow
                   ,asOfDate :: Date                                        -- ^ include cashflow after this date 
                   ,issuanceStat :: Maybe (Map.Map CutoffFields Balance)    -- ^ cutoff balance of pool
                   ,extendPeriods :: Maybe DatePattern                      -- ^ dates for extend pool collection
                   } deriving (Show, Generic, Ord, Eq)

makeLensesFor [("futureCf","futureCfLens"),("futureScheduleCf","futureScheduleCfLens")] ''Pool

poolFutureCf :: Asset a => Lens' (Pool a) (Maybe CF.PoolCashflow)
poolFutureCf = lens getter setter 
  where 
    getter = futureCf
    setter p mNewCf = p {futureCf = mNewCf}

poolFutureScheduleCf :: Asset a => Lens' (Pool a) (Maybe CF.PoolCashflow)
poolFutureScheduleCf = lens getter setter
  where 
    getter = futureScheduleCf
    setter p mNewCf = p {futureScheduleCf = mNewCf}

poolIssuanceStat :: Asset a => Lens' (Pool a) (Map.Map CutoffFields Balance)
poolIssuanceStat = lens getter setter
  where 
    getter p =  fromMaybe Map.empty $ issuanceStat p
    setter p m = case issuanceStat p of
                    Nothing -> p {issuanceStat = Just m}
                    Just _ -> p {issuanceStat = Just m}


-- | get stats of pool 
getIssuanceField :: Pool a -> CutoffFields -> Either String Balance
getIssuanceField p@Pool{issuanceStat = Just m} s
  = case Map.lookup s m of
      Just r -> Right r
      Nothing -> Left $ "Faile dto find field "++ show s ++ "in pool issuance " ++ show m
getIssuanceField Pool{issuanceStat = Nothing} s 
  = Left $ "There is no pool stats to lookup:" ++ show s

poolBegStats :: Pool a -> (Balance,Balance,Balance,Balance,Balance,Balance)
poolBegStats p = 
  let 
    m = issuanceStat p
    stats = case m of
              Nothing -> (0,0,0,0,0,0)
              Just m -> (Map.findWithDefault 0 HistoryPrincipal m
                        ,Map.findWithDefault 0 HistoryPrepayment m
                        ,Map.findWithDefault 0 HistoryDelinquency m
                        ,Map.findWithDefault 0 HistoryDefaults m
                        ,Map.findWithDefault 0 HistoryRecoveries m
                        ,Map.findWithDefault 0 HistoryLoss m)
  in
    stats


-- | Aggregate all cashflow into a single cashflow frame
-- patch with pool level cumulative defaults/loss etc
aggPool :: Maybe (Map.Map CutoffFields Balance) -> [(CF.CashFlowFrame, Map.Map CutoffFields Balance)] -> (CF.CashFlowFrame, Map.Map CutoffFields Balance)
aggPool Nothing [] = (CF.CashFlowFrame (0,toDate "19000101",Nothing) [],Map.empty)
aggPool (Just m) [] = (CF.CashFlowFrame (0,toDate "19000101",Nothing) [], m)
aggPool mStat xs 
  = let
      cfs = fst <$> xs
      CF.CashFlowFrame st _txns = foldr1 CF.combine cfs 
      -- total stats with begin stats + stats from each cfs
      stats = foldr1 (Map.unionWith (+)) $  fromMaybe Map.empty mStat:(snd <$> xs)
      -- patch cumulative statistics
      cumulativeStatAtCutoff = case mStat of
                                 Nothing -> (0,0,0,0,0,0)
                                 Just m -> (Map.findWithDefault 0 HistoryPrincipal m
                                           ,Map.findWithDefault 0 HistoryPrepayment m
                                           ,Map.findWithDefault 0 HistoryDelinquency m
                                           ,Map.findWithDefault 0 HistoryDefaults m
                                           ,Map.findWithDefault 0 HistoryRecoveries m
                                           ,Map.findWithDefault 0 HistoryLoss m)
      -- (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)
      txns = CF.patchCumulative cumulativeStatAtCutoff [] _txns 
      -- txns = CF.patchCumulativeAtInit (Just cumulativeStatAtCutoff) _txns 
    in
      case Map.lookup AccruedInterest =<< mStat of
        Nothing -> (CF.CashFlowFrame st txns, stats) 
        Just accruedIntAmt -> (CF.CashFlowFrame st (CF.clawbackInt accruedIntAmt txns), stats)


calcLiquidationAmount :: Asset a => PricingMethod -> Pool a -> Date -> Amount
calcLiquidationAmount (BalanceFactor currentFactor defaultFactor ) pool d 
  = case futureCf pool of 
      Just (CF.CashFlowFrame _ [],_) -> 0
      Just _futureCf@(CF.CashFlowFrame _ trs,_) ->
        let 
          earlierTxns = cutBy Inc Past d trs
          currentCumulativeDefaultBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
        in 
          case earlierTxns of 
            [] -> 0  -- `debug` ("No pool Inflow")
            _ -> (mulBR (view CF.tsRowBalance (last earlierTxns)) currentFactor) + (mulBR currentCumulativeDefaultBal defaultFactor)
            -- TODO need to check if missing last row


-- TODO: check futureCf is future CF or not, seems it is collected CF
-- | pricing via future scheduled cashflow( zero risk adjust)
-- | pricing via user define risk adjust cashflow( own assumption)
-- TODO: in revolving buy future schedule cashflow should be updated as well
calcLiquidationAmount (PV discountRate  recoveryPct) pool d 
  = case futureCf pool of
      Just (CF.CashFlowFrame _ [],_) -> 0 
      Just (CF.CashFlowFrame _ trs,_) ->
          let 
            futureTxns = cutBy Inc Future d trs -- `debug` (" pv date"++show d++ " with rate"++show discountRate)
            earlierTxns = cutBy Exc Past d trs -- `debug` ("Total txn"++show trs)
            pvCf = sum $ map (\x -> AN.pv2  discountRate  d (CF.getDate x) (CF.tsTotalCash x)) futureTxns -- `debug` ("FutureTxns: "++show futureTxns)
            
            currentDefaulBal = sum $ map (\x -> CF.mflowDefault x - CF.mflowRecovery x - CF.mflowLoss x) earlierTxns
          in 
            
            pvCf + mulBR currentDefaulBal recoveryPct

-- ^ price a pool with collected cashflow and future cashflow
pricingPoolFlow :: Asset a =>  Date -> Pool a -> CF.PoolCashflow -> PricingMethod -> Amount
pricingPoolFlow d pool@Pool{ futureCf = Just (mCollectedCf,_), issuanceStat = mStat } (futureCfUncollected,_) pm 
  = let 
      currentCumulativeDefaultBal 
        | CF.emptyCashFlowFrame  mCollectedCf = 0
        | otherwise = let 
                        lastTxn = last $ view CF.cashflowTxn  $ mCollectedCf
                      in 
                        fromMaybe 0 (CF.tsCumDefaultBal lastTxn) - fromMaybe 0 (CF.tsCumRecoveriesBal lastTxn) - fromMaybe 0 (CF.tsCumLossBal lastTxn)

      currentPerformingBal = case mStat of
              Nothing -> 0
              Just stat -> Map.findWithDefault 0 RuntimeCurrentPoolBalance stat

    in 
      case pm of
        BalanceFactor currentFactor defaultFactor -> 
          mulBR currentPerformingBal currentFactor + mulBR currentCumulativeDefaultBal defaultFactor

        PvRate discountRate ->
          let 
            futureTxn = view CF.cashflowTxn futureCfUncollected -- `debug` ("PV with cf"++ show d ++ ">>"++show futureCfUncollected)
            futureCfCash = CF.tsTotalCash <$> futureTxn
            futureDates = getDate <$> futureTxn
          in 
            AN.pv21 discountRate d futureDates futureCfCash

 -- | run a pool of assets ,use asOfDate of Pool to cutoff cashflow yields from assets with assumptions supplied
runPool :: Asset a => Pool a -> Maybe A.ApplyAssumptionType -> Maybe [RateAssumption] 
        -> Either String [(CF.CashFlowFrame, Map.Map CutoffFields Balance)]
-- use interest rate assumption
runPool (Pool as _ _ asof _ _) Nothing mRates 
  = do 
      cf <- sequenceA $ parMap rdeepseq  (\x -> calcCashflow x asof mRates) as 
      return [ (x, Map.empty) | x <- cf ]
-- asset cashflow with credit stress
---- By pool level
runPool (Pool as _ Nothing asof _ _) (Just (A.PoolLevel assumps)) mRates 
  = sequenceA $ parMap rdeepseq (\x -> projCashflow x asof assumps mRates) as  
---- By index
runPool (Pool as _ Nothing  asof _ _) (Just (A.ByIndex idxAssumps)) mRates =
  let
    numAssets = length as
  in
    do 
      _assumps <- traverse (A.lookupAssumptionByIdx idxAssumps) [0..(pred numAssets)] -- `debug` ("Num assets"++ show numAssets)
      sequenceA $ parMap rdeepseq (\(x, a) -> projCashflow x asof a mRates) (zip as _assumps)

---- By Obligor
runPool (Pool as _ Nothing asof _ _) (Just (A.ByObligor obligorRules)) mRates =
  let
    matchAssets []   _ [] = Right [(CF.CashFlowFrame (0,epocDate,Nothing) [], Map.empty)] 
    matchAssets cfs [] [] = sequenceA cfs
    -- matchAssets cfs [] astList = sequenceA $ cfs ++ ((\x -> (\y -> (y, Map.empty)) <$> (Ast.calcCashflow x asof mRates)) <$> astList)
    matchAssets cfs [] astList = let
                                    poolCfs = parMap rdeepseq (\x -> calcCashflow x asof mRates) astList
                                    poolCfs' = (\x -> (, Map.empty) <$> x) <$> poolCfs
                                 in 
                                    sequenceA $ cfs ++ poolCfs'
    matchAssets cfs (rule:rules) astList = 
      case rule of 
        A.ObligorById ids assetPerf 
          -> let 
               idSet = S.fromList ids
               (matchedAsts,unMatchedAsts) = partition 
                                               (\x -> case getObligorId x of 
                                                         Just oid -> S.member oid idSet
                                                         Nothing -> False) 
                                               astList
               matchedCfs = parMap rdeepseq (\x -> projCashflow x asof assetPerf mRates) matchedAsts 
             in 
               matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        A.ObligorByTag tags tagRule assetPerf ->
          let 
            obrTags = S.fromList tags

            matchRuleFn A.TagEq s1 s2 = s1 == s2 
            matchRuleFn A.TagSubset s1 s2 = s1 `S.isSubsetOf` s2
            matchRuleFn A.TagSuperset s1 s2 = s2 `S.isSubsetOf` s1
            matchRuleFn A.TagAny s1 s2 = not $ S.null $ S.intersection s1 s2
            matchRuleFn (A.TagNot tRule) s1 s2 = not $ matchRuleFn tRule s1 s2
            
            (matchedAsts,unMatchedAsts) = partition (\x -> matchRuleFn tagRule (getObligorTags x) obrTags) astList
            matchedCfs = parMap rdeepseq (\x -> projCashflow x asof assetPerf mRates) matchedAsts 
          in 
            matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        
        A.ObligorByField fieldRules assetPerf -> 
          let 
            matchRuleFn (A.FieldIn fv fvals) Nothing = False
            matchRuleFn (A.FieldIn fv fvals) (Just fm) = case Map.lookup fv fm of
                                                    Just (Left v) -> v `elem` fvals
                                                    Nothing -> False
            matchRuleFn (A.FieldCmp fv cmp dv) (Just fm) = case Map.lookup fv fm of
                                                        Just (Right v) -> case cmp of 
                                                                    G -> v > dv
                                                                    L -> v < dv
                                                                    GE -> v >= dv
                                                                    LE -> v <= dv
                                                        Nothing -> False
            matchRuleFn (A.FieldInRange fv rt dv1 dv2) (Just fm) = 
              case Map.lookup fv fm of
                Just (Right v) -> case rt of 
                          II -> v <= dv2 && v >= dv1
                          IE -> v <= dv2 && v > dv1
                          EI -> v < dv2 && v >= dv1
                          EE -> v < dv2 && v > dv1
                          _ -> False
                Nothing -> False
            matchRuleFn (A.FieldNot fRule) fm = not $ matchRuleFn fRule fm

            matchRulesFn fs fm = all (`matchRuleFn` fm) fs

            (matchedAsts,unMatchedAsts) = partition (matchRulesFn fieldRules . getObligorFields) astList            
            matchedCfs = parMap rdeepseq (\x -> projCashflow x asof assetPerf mRates) matchedAsts 
         in 
            matchAssets (cfs ++ matchedCfs) rules unMatchedAsts
        A.ObligorByDefault assetPerf ->
          matchAssets 
            (cfs ++ (parMap rdeepseq (\x -> projCashflow x asof assetPerf mRates) astList))
            []
            []
  in
    matchAssets [] obligorRules as

-- safe net to catch other cases
runPool _a _b _c = Left $ "[Run Pool]: Failed to match" ++ show _a ++ show _b ++ show _c



$(deriveJSON defaultOptions ''Pool)
