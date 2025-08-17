{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Asset ( Asset(..),
       buildAssumptionPpyDefRecRate,buildAssumptionPpyDelinqDefRecRate
       ,calcRecoveriesFromDefault,getCurBalance
       ,priceAsset,applyHaircut,buildPrepayRates,buildDefaultRates,getObligorFields
       ,getObligorTags,getObligorId,getRecoveryLagAndRate,getDefaultDelinqAssump,getOriginInfo
) where

import qualified Data.Time as T
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Lib (Period(..)
           ,Ts(..),periodRateFromAnnualRate,toDate
           ,getIntervalDays,zipWith9,mkTs,periodsBetween
           ,mkRateTs,daysBetween, getIntervalFactors)

import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified AssetClass.AssetBase as ACM 
import AssetClass.AssetCashflow

import qualified Data.Map as Map
import Analytics
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Types hiding (Current)
import Text.Printf
import Data.Fixed
import qualified InterestRate as IR
import qualified Data.Set as Set
import Util

import AssetClass.AssetBase ( OriginalInfo(..), calcPmt, AssetUnion, Obligor(..) )

import Debug.Trace
import Assumptions (ExtraStress(ExtraStress))

import Control.Lens hiding (element)
import Control.Lens.TH
import DateUtil (yearCountFraction)


debug = flip trace

class (Show a,IR.UseRate a) => Asset a where
  -- | project contractual cashflow of an asset with interest assumptions
  calcCashflow :: a -> Date -> Maybe [RateAssumption] -> Either String CF.CashFlowFrame
  -- | Get current balance of an asset
  getCurrentBal :: a -> Balance
  -- | Get original balance of an asset
  getOriginBal :: a -> Balance
  -- | Get original rate of an asset
  getOriginRate :: a -> IRate
  -- | Get current rate of an asset
  getCurrentRate :: a -> IRate
  -- | Get origination date of an asset
  getOriginDate :: a -> Date
  -- | Get origin info of an asset
  getOriginInfo :: a -> OriginalInfo  
  -- | if the asset is defaulted
  isDefaulted :: a -> Bool
  -- | project projected dates of an asset
  getPaymentDates :: a -> Int -> [Date]
  -- | get number of remaining payments
  getRemainTerms :: a -> Int
  -- | get remain payment dates
  getRemainDates :: a -> [Date]
  getRemainDates a = lastN (getRemainTerms a) (getPaymentDates a 0)
  -- | project asset cashflow under credit stress and interest assumptions
  getTotalTerms :: a -> Int 
  getTotalTerms a = ACM.originTerm (getOriginInfo a)

  getPastTerms :: a -> Int
  getPastTerms a = getTotalTerms a - getRemainTerms a

  projCashflow :: a -> Date -> A.AssetPerf -> Maybe [RateAssumption] -> Either String (CF.CashFlowFrame, Map.Map CutoffFields Balance)
  -- | Get possible number of borrower 
  getBorrowerNum :: a -> Int
  -- | Split asset per rates passed in 
  splitWith :: a -> [Rate] -> [a]
  -- | ! Change the origination date of an asset
  updateOriginDate :: a -> Date -> a
  -- | ! Change the current asset state to the date of origination
  resetToOrig :: a -> a
  -- | Get Last Interest Payment date
  getLastInterestPaymentDate :: a -> Maybe Date
  -- | Calculate Accrued Interest 
  calcAccruedInterest :: a -> Date -> Balance
  -- | ! Internal use
  calcAlignDate :: a -> Date -> Date
  calcAlignDate ast d = let 
                          payDates = Asset.getOriginDate ast:getPaymentDates ast 0
                          remainTerms = getRemainTerms ast 
                          benchDate = reverse payDates!! remainTerms  
                          offset = daysBetween benchDate d
                        in 
                          T.addDays offset $ Asset.getOriginDate ast

  getObligor :: a -> Maybe Obligor
  getObligor a = 
      case getOriginInfo a of 
        FixedAssetInfo {} -> Nothing
        MortgageOriginalInfo{obligor = x } -> x
        LoanOriginalInfo{obligor = x } -> x
        LeaseInfo{obligor =  x } -> x
        ReceivableInfo{obligor = x } -> x

  getObligorTags :: a -> Set.Set String
  getObligorTags a = 
      case getOriginInfo a of 
        MortgageOriginalInfo{obligor = Just obr } -> Set.fromList (obligorTag obr)
        LoanOriginalInfo{obligor = Just obr } -> Set.fromList (obligorTag obr)
        LeaseInfo{obligor = Just obr } -> Set.fromList (obligorTag obr)
        ReceivableInfo{obligor = Just obr } -> Set.fromList (obligorTag obr)
        _ -> mempty

  getObligorId :: a -> Maybe String
  getObligorId a = 
      case getOriginInfo a of 
        MortgageOriginalInfo{obligor = Just obr } -> Just (obligorId obr)
        LoanOriginalInfo{obligor = Just obr } -> Just (obligorId obr)
        LeaseInfo{obligor = Just obr } -> Just (obligorId obr)
        ReceivableInfo{obligor = Just obr } -> Just (obligorId obr)
        _ -> Nothing

  getObligorFields :: a -> Maybe (Map.Map String (Either String Double))
  getObligorFields a = 
    let 
      obInfo = getObligor a
    in
      case obInfo of
        Nothing -> Nothing 
        Just ob -> Just (obligorFields ob)

  {-# MINIMAL calcCashflow,getCurrentBal,getOriginBal,getOriginRate #-}



-- | apply ExtraStress on prepayment/default rates
applyExtraStress :: Maybe A.ExtraStress -> [Date] -> [Rate] -> [Rate] -> ([Rate],[Rate])
applyExtraStress Nothing _ ppy def = (ppy,def)
applyExtraStress (Just ExtraStress{A.defaultFactors= mDefFactor
                                  ,A.prepaymentFactors = mPrepayFactor}) ds ppy def =
  case (mPrepayFactor,mDefFactor) of
    (Nothing,Nothing) -> (ppy,def)
    (Nothing,Just defFactor) -> (ppy ,getTsVals $ multiplyTs Exc (zipTs ds def) defFactor)
    (Just ppyFactor,Nothing) -> (getTsVals $ multiplyTs Exc (zipTs ds ppy) ppyFactor, def)
    (Just ppyFactor,Just defFactor) -> (getTsVals $ multiplyTs Exc (zipTs ds ppy) ppyFactor
                                       ,getTsVals $ multiplyTs Exc (zipTs ds def) defFactor)


cpr2smm :: Rate -> Rate
cpr2smm r = toRational $ 1 - (1 - fromRational r :: Double) ** (1/12)

normalPerfVector :: [Rate] -> [Rate]
normalPerfVector = floorWith 0.0 . capWith 1.0

buildPrepayRates :: Asset b => b -> [Date] -> Maybe A.AssetPrepayAssumption -> Either ErrorRep [Rate]
buildPrepayRates _ ds Nothing = return $ replicate ((pred . length) ds) 0.0
buildPrepayRates a ds (Just (A.PrepaymentConstant r)) 
  | r < 0 || r > 1.0  = Left $ "buildPrepayRates: prepayment constant rate should be between 0 and 1, got " ++ show r
  | otherwise = return $ replicate (pred (length ds)) r

buildPrepayRates a ds (Just (A.PrepaymentCPR r)) 
  | r < 0 || r > 1.0 = Left $ "buildPrepayRates: prepayment CPR rate should be between 0 and 1, got " ++ show r
  | otherwise = return $ Util.toPeriodRateByInterval r <$> getIntervalDays ds

buildPrepayRates a ds (Just (A.PrepaymentVec vs))
  | any (> 1.0) vs || any (< 0.0) vs = Left $ "buildPrepayRates: prepayment vector should be between 0 and 1, got " ++ show vs
  | otherwise = return $ zipWith Util.toPeriodRateByInterval
                                 (paddingDefault 0.0 vs (pred (length ds)))
                                 (getIntervalDays ds)
buildPrepayRates a ds (Just (A.PrepaymentVecPadding vs))
  | any (> 1.0) vs || any (< 0.0) vs = Left $ "buildPrepayRates: prepayment vector should be between 0 and 1, got " ++ show vs
  | otherwise = return $ zipWith Util.toPeriodRateByInterval
                                 (paddingDefault (last vs) vs (pred (length ds)))
                                 (getIntervalDays ds)
buildPrepayRates a ds (Just (A.PrepayStressByTs ts x)) 
  | any (< 0.0) (getTsVals ts) = Left $ "buildPrepayRates: prepayment vector by ts should be non-negative, got " ++ show (getTsVals ts)
  | otherwise = do
                  rs <- buildPrepayRates a ds (Just x)
                  return $ getTsVals $ multiplyTs Exc (zipTs (tail ds) rs) ts
buildPrepayRates a ds (Just (A.PrepaymentPSA r))
  | r < 0 = Left $ "buildPrepayRates: PSA rate should be non-negative, got " ++ show r
  | otherwise = let 
                  agedTerm = getPastTerms a
                  remainingTerm = getRemainTerms a
                  ppyVectorInCPR = (* r) <$> [0.002,0.004..0.06] ++ repeat 0.06
                  vectorUsed = take remainingTerm $ drop agedTerm ppyVectorInCPR
                in 
                  case period (getOriginInfo a) of
                    Monthly -> return $ cpr2smm <$> vectorUsed
                    _ -> Left $ "PSA is only supported for monthly payment but got "++ show (period (getOriginInfo a))
buildPrepayRates a ds (Just (A.PrepaymentByTerm rs)) 
  | any (< 0.0) (concat rs) = Left $ "buildPrepayRates: prepayment by term vector should be non-negative, got " ++ show rs
  | any (> 1.0) (concat rs) = Left $ "buildPrepayRates: prepayment by term vector should be between 0 and 1, got " ++ show rs
  | otherwise = let 
                  agedTerm = getPastTerms a
                  oTerm = originTerm (getOriginInfo a)
                in 
                  case find (\x -> oTerm == length x) rs of 
                    Just v -> return $ drop agedTerm v
                    Nothing -> Left "Prepayment by term doesn't match the origin term"

buildDefaultRates :: Asset b => b -> [Date] -> Maybe A.AssetDefaultAssumption -> Either ErrorRep [Rate]
buildDefaultRates _ ds Nothing = return $ replicate ((pred . length) ds) 0.0
buildDefaultRates a [] mDa = Left "buildDefaultRates: empty date list" 
buildDefaultRates a ds (Just (A.DefaultConstant r))
  | r > 1.0 || r < 0.0 = Left $ "buildDefaultRates: default constant rate should be between 0 and 1, got " ++ show r
  | otherwise = return $ replicate (pred (length ds)) r
buildDefaultRates a ds (Just (A.DefaultCDR r))
  | r > 1.0 || r < 0.0 = Left $ "buildDefaultRates: default CDR rate should be between 0 and 1, got " ++ show r
  | otherwise = return $ Util.toPeriodRateByInterval r <$> getIntervalDays ds
buildDefaultRates a ds (Just (A.DefaultVec vs))
  | any (> 1.0) vs || any (< 0.0) vs = Left $ "buildDefaultRates: default vector should be between 0 and 1, got " ++ show vs
  | otherwise = return $ zipWith Util.toPeriodRateByInterval
                                 (paddingDefault 0.0 vs (pred (length ds)))
                                 (getIntervalDays ds)
buildDefaultRates a ds (Just (A.DefaultVecPadding vs))
  | any (> 1.0) vs || any (< 0.0) vs = Left $ "buildDefaultRates: default vector should be between 0 and 1, got " ++ show vs
  | otherwise = return $ zipWith Util.toPeriodRateByInterval
                                 (paddingDefault (last vs) vs (pred (length ds)))
                                 (getIntervalDays ds)
buildDefaultRates a ds (Just (A.DefaultAtEndByRate r rAtEnd))
  | r > 1.0 || r < 0.0 = Left $ "buildDefaultRates: default at end rate should be between 0 and 1, got " ++ show r
  | rAtEnd > 1.0 || rAtEnd < 0.0 = Left $ "buildDefaultRates: default at end rate should be between 0 and 1, got " ++ show rAtEnd
  | otherwise = return $
                  case size of 
                    0 -> []
                    1 -> []
                    _ -> (Util.toPeriodRateByInterval r <$> getIntervalDays (init ds)) ++ (Util.toPeriodRateByInterval rAtEnd <$> getIntervalDays [head ds,last ds])
  where
    size = length ds
buildDefaultRates a ds (Just (A.DefaultStressByTs ts x)) 
  | any (< 0.0) (getTsVals ts) = Left $ "buildDefaultRates: default vector by ts should be non-negative, got " ++ show (getTsVals ts)
  | otherwise = do
                  rs <- buildDefaultRates a ds (Just x)
                  return $ getTsVals $ multiplyTs Inc (zipTs (tail ds) rs) ts

buildDefaultRates a ds (Just (A.DefaultByTerm rs))
  | any (< 0.0) (concat rs) = Left $ "buildDefaultRates: default by term vector should be non-negative, got " ++ show rs
  | any (> 1.0) (concat rs) = Left $ "buildDefaultRates: default by term vector should be between 0 and 1, got " ++ show rs
  | otherwise = 
      let 
        agedTerm = getPastTerms a
        oTerm = originTerm (getOriginInfo a)
      in 
        case find (\x -> oTerm == length x) rs of 
          Just v -> return $ drop agedTerm v
          Nothing -> Left "Default by term doesn't match the origin term"


getRecoveryLagAndRate :: Maybe A.RecoveryAssumption -> (Rate,Int)
getRecoveryLagAndRate Nothing = (0,0)
getRecoveryLagAndRate (Just (A.Recovery (r,lag))) = (r,lag)

-- | build pool assumption rate (prepayment, defaults, recovery rate , recovery lag)
buildAssumptionPpyDefRecRate :: Asset a => a -> [Date] -> A.AssetPerfAssumption -> Either ErrorRep ([Rate],[Rate],Rate,Int)
buildAssumptionPpyDefRecRate a ds (A.LoanAssump mDa mPa mRa mESa) = buildAssumptionPpyDefRecRate a ds (A.MortgageAssump mDa mPa mRa mESa)
buildAssumptionPpyDefRecRate a ds (A.MortgageAssump mDa mPa mRa mESa)
  = let  
      size = length ds
      zeros = replicate size 0.0
      (recoveryRate,recoveryLag) = getRecoveryLagAndRate mRa
    in 
      do 
        prepayRates <- buildPrepayRates a ds mPa
        defaultRates <- buildDefaultRates a ds mDa
        let (prepayRates2,defaultRates2) = applyExtraStress mESa ds prepayRates defaultRates
        return (prepayRates2,defaultRates2,recoveryRate,recoveryLag) 


getDefaultDelinqAssump :: Maybe A.AssetDelinquencyAssumption -> [Date] -> ([Rate],Int,Rate)
getDefaultDelinqAssump Nothing ds = (replicate (length ds) 0.0, 0, 0.0)  
getDefaultDelinqAssump (Just (A.DelinqCDR r (lag,pct))) ds = (map (Util.toPeriodRateByInterval r) (getIntervalDays ds)
                                                    ,lag 
                                                    ,pct)

getDefaultLagAndRate :: Maybe A.RecoveryAssumption -> (Rate,Int)
getDefaultLagAndRate Nothing = (0,0)
getDefaultLagAndRate (Just (A.Recovery (r,lag))) = (r,lag)

-- | build prepayment rates/ delinq rates and (%,lag) convert to default, recovery rate, recovery lag
buildAssumptionPpyDelinqDefRecRate :: Asset a => a -> [Date] -> A.AssetPerfAssumption -> Either String ([Rate],[Rate],(Rate,Lag),Rate,Int)
buildAssumptionPpyDelinqDefRecRate _ ds (A.MortgageDeqAssump mDeqDefault mPa mRa (Just _)) = Left "Delinq assumption doesn't support extra stress"
buildAssumptionPpyDelinqDefRecRate a ds (A.MortgageDeqAssump mDeqDefault mPa mRa Nothing)
  = let 
      (recoveryRate,recoveryLag) = getRecoveryLagAndRate mRa
      zeros = replicate (length ds) 0.0
      (delinqRates,defaultLag,defaultPct) = case mDeqDefault of
                                              Nothing -> (zeros,0,0.0)
                                              Just (A.DelinqCDR r (lag,pct)) -> 
                                                (map (Util.toPeriodRateByInterval r) (getIntervalDays ds)
                                                ,lag 
                                                ,pct)
    in 
      do 
        prepayRates <- buildPrepayRates a ds mPa
        return (prepayRates,delinqRates,(defaultPct,defaultLag),recoveryRate, recoveryLag)


calcRecoveriesFromDefault :: Balance -> Rate -> [Rate] -> [Amount]
calcRecoveriesFromDefault bal recoveryRate recoveryTiming
  = mulBR recoveryAmt <$> recoveryTiming
    where
      recoveryAmt = mulBR bal recoveryRate

priceAsset :: Asset a => a -> Date -> PricingMethod -> A.AssetPerf -> Maybe [RateAssumption] -> CutoffType 
           -> Either String PriceResult
priceAsset m d (PVCurve curve) assumps mRates cType
  = let 
      cr = getCurrentRate m
      pDays = Asset.getOriginDate m:(getPaymentDates m 0)
      cb = getCurrentBal m
    in
      case projCashflow m d assumps mRates of
        Right (CF.CashFlowFrame _ txns,_) ->
          let 
            ds = getDate <$> txns 
            accruedInt = case ds of 
                  [] -> 0 
                  (fstTxnDate:_) -> 
                    let 
                      accStartDate = last $ takeWhile (< fstTxnDate) pDays 
                    in 
                      mulBR (mulBIR cb cr) (yearCountFraction DC_ACT_365F accStartDate d) 
            amts = CF.tsTotalCash <$> (case cType of 
                                    Exc -> CF.clawbackInt accruedInt txns 
                                    Inc -> txns)
            pv = pv3 curve d ds amts -- `debug` ("pricing"++ show d++ show ds++ show amts)
            wal = calcWAL ByYear cb d (zip amts ds)
            duration = fromRational $ calcDuration DC_ACT_365F d (zip ds amts) curve
            convexity = fromRational $ calcConvexity DC_ACT_365F d (zip ds amts) curve
          in
            Right $ AssetPrice pv wal duration convexity accruedInt
        Left x -> Left x

priceAsset m d (BalanceFactor currentFactor defaultedFactor) assumps mRates cType
  = let 
      cb =  getCurrentBal m
      val = if isDefaulted m then 
              mulBR cb defaultedFactor -- `debug` ("Defulat CB"++ show cb)
            else
              mulBR cb currentFactor  -- `debug` ("CB"++ show cb)
    in
       case projCashflow m d assumps mRates of
         Right (CF.CashFlowFrame _ txns,_) ->
           let ds = getDate <$> txns 
               amts = CF.tsTotalCash <$> txns 
               wal = calcWAL ByYear cb d (zip amts ds) -- `debug` ("pricing"++ show d++ show ds++ show amts)
           in  
             Right $ AssetPrice val wal (-1) (-1) (-1)  
         Left x -> Left x
      
priceAsset m d (PvRate r) assumps mRates cType
  = let 
      cb = getCurrentBal m
      pDays = Asset.getOriginDate m:getPaymentDates m 0
      cr = getCurrentRate m
    in 
        case projCashflow m d assumps mRates of
          Right (CF.CashFlowFrame _ txns,_) ->
            let ds = getDate <$> txns 
                accruedInt = case ds of 
                              [] -> 0 
                              (fstTxnDate:_) -> 
                                let 
                                  accStartDate = last $ takeWhile (< fstTxnDate) pDays 
                                in 
                                  mulBR (mulBIR cb cr) (yearCountFraction DC_ACT_365F accStartDate d)  
                amts = CF.tsTotalCash <$> (case cType of 
                                            Exc -> CF.clawbackInt accruedInt txns 
                                            Inc -> txns)
                wal = calcWAL ByYear cb d (zip amts ds) 
                pv = sum $ zipWith (pv2  r d) ds amts
                curve = mkTs $ zip ds (repeat (toRational r))
                duration = fromRational $ calcDuration DC_ACT_365F d (zip ds amts) curve
                convexity = fromRational $ calcConvexity DC_ACT_365F d (zip ds amts) curve
            in
              Right $ AssetPrice pv wal duration convexity accruedInt
          Left x -> Left x
