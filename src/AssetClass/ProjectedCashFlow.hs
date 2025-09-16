{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.ProjectedCashFlow  
  (ProjectedCashFlow(..))
  where

import qualified Data.Time as T
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import DateUtil
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.DList as DL
import qualified Cashflow as CF

import AssetClass.AssetBase
import AssetClass.AssetCashflow

import Cashflow (extendTxns,TsRow(..))

import Debug.Trace
import Control.Lens hiding (element,Index)
import Control.Lens.TH
debug = flip trace


data IntCalcType = ByRate IRate
                 | ByAmount Amount


projectScheduleFlow ::  DL.DList CF.TsRow -> Rate -> (Balance,Date) -> [(Date,Rate,IntCalcType)] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> (DL.DList CF.TsRow)
projectScheduleFlow trs _ last_bal [] _ _ [] [] (_,_) = trs
projectScheduleFlow trs bal_factor (startBal,startDate) ((aDate,aRate,iRate):flows) (defRate:defRates) (ppyRate:ppyRates) recV lossV (recoveryLag,recoveryRate)
  = let
      defAmt = mulBR startBal defRate
      ppyAmt = mulBR (startBal - defAmt) ppyRate 
      afterBal = startBal - defAmt - ppyAmt

      
      interest = case iRate of 
                   ByRate _r -> mulBIR afterBal $ calcIntRate startDate aDate _r DC_ACT_365F
                   ByAmount _a -> mulBR _a bal_factor

      rateUsed = case iRate of 
                   ByRate r -> r
                   ByAmount scheduleInt -> fromRational $  (divideBB scheduleInt afterBal) /  (yearCountFraction DC_ACT_365F startDate aDate)
      
      surviveRate = (1 - defRate) * (1 - ppyRate) * bal_factor
      schedulePrin = mulBR afterBal aRate

      newRec = mulBR defAmt recoveryRate
      newLoss = mulBR defAmt (1 - recoveryRate)
      recVector = replace recV recoveryLag newRec
      lossVector = replace lossV recoveryLag newLoss

      endBal = afterBal - schedulePrin 
      tr = CF.MortgageFlow aDate endBal schedulePrin interest ppyAmt defAmt (head recVector) (head lossVector) rateUsed Nothing Nothing Nothing
    in 
      projectScheduleFlow (DL.snoc trs tr) surviveRate (endBal,aDate) flows defRates ppyRates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate)

projectScheduleFlow trs b_factor (lastBal,lastDate) [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
  = let 
      remain_length = length rs
      flowDate = nextDate lastDate Lib.Monthly
      tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing
    in 
      projectScheduleFlow (DL.snoc trs tr) b_factor (lastBal,flowDate) [] [] [] rs ls (recovery_lag - 1,recovery_rate) 


-- ^ project cashflow with fix rate portion
projFixCfwithAssumption :: ([Balance], [Date], [Rational], [IntCalcType], DatePattern) -> ([Rate],[Rate],Rate,Int) -> Date -> Either ErrorRep CF.CashFlowFrame
projFixCfwithAssumption (bals, ds, amortFactors, rates, dp) (ppyRates,defRates,recoveryRate,recoveryLag) asOfDay
  | all (== 0) bals = return $ CF.CashFlowFrame (0.0,asOfDay,Nothing) []
  | length amortFactors /= length ppyRates || length amortFactors /= length defRates 
    = Left $ "Not even rates amort "++ (showLength amortFactors) ++ "ppy rate " ++ (showLength ppyRates) ++ " def rate" ++ (showLength defRates)
  | otherwise = let
                  curveDatesLength = recoveryLag + length ds -- `debug` ("curveDatesLength " ++ show recoveryLag ++ " " ++ show (length ds))
                  extraDates = genSerialDates dp Exc (last ds) recoveryLag -- `debug` (" curve dates length" ++ show curveDatesLength )
                  cfDates = ds ++ extraDates -- `debug` ("cfDates " ++ show extraDates++ "lag" ++ show recoveryLag)
                  begBal = head bals
                  begDate = head cfDates
                  amortSchedule = zip3 ds (0:amortFactors) rates
                  initRow = CF.MortgageFlow (head cfDates) begBal 0.0 0.0 0.0 0.0 0.0 0.0 0.0 Nothing Nothing Nothing
                  txns = projectScheduleFlow (DL.singleton initRow) 1.0 (begBal,begDate) (tail amortSchedule) defRates ppyRates (replicate curveDatesLength 0.0) (replicate curveDatesLength 0.0) (recoveryLag,recoveryRate)
                in 
                  do
                    let (futureTxns,historyM) = CF.cutoffTrs asOfDay (DL.toList txns) -- `debug` ("txn" ++ show txns)
                    let cb = (CF.mflowBegBalance . head) futureTxns
                    return $ CF.CashFlowFrame (cb,asOfDay,Nothing) futureTxns -- `debug` ("future txns" ++ show futureTxns)

-- ^ project cashflow with fix rate portion
projIndexCashflows :: ([Balance] , [Date] , Index, Spread) -> DatePattern -> ([Rate],[Rate],Rate,Int) -> Maybe [RateAssumption] -> Either String CF.CashFlowFrame
projIndexCashflows (bals, ds, index, spd) dp pAssump Nothing = Left "No rate assumption provided for index cashflow projection" 
projIndexCashflows (bals, ds, index, spd) dp pAssump (Just ras) = 
  do
    indexRates <- traverse (A.lookupRate0 ras index) ds 
    let rates = ByRate <$> (spd +) <$> indexRates
    let floatPrincipalFlow = diffNum bals
    let floatAmortFactor =  zipWith divideBB floatPrincipalFlow (init bals) 

    projFixCfwithAssumption (bals, ds, floatAmortFactor, rates,  dp) pAssump (head ds)
    
-- ^ project cashflow with fix rate portion and floater rate portion
seperateCashflows :: ProjectedCashFlow -> Maybe A.AssetPerfAssumption -> Maybe [RateAssumption] -> Either String (CF.CashFlowFrame, [CF.CashFlowFrame])
seperateCashflows a@(ProjectedByFactor pflow dp (fixPct,fixRate) floaterList)
                  mPassump
                  mRates
  | fixPct + sum (view _1 <$> floaterList) /= 1.0
    = Left $ "Fix portion " ++ show fixPct ++ " + float portion " ++ show (sum (view _1 <$> floaterList)) ++ " is not sum up to 100% "
  | otherwise = let
                  totalBals = (view _2) <$> pflow
                  principalFlows = diffNum totalBals
                  (begDate,begBal) = head pflow
                  ds = (view _1) <$> pflow
                  flowSize = length ds
                  -- fix rate cashflow
                  fixedBals = flip mulBR fixPct <$> totalBals
                  fixedPrincipalFlow = diffNum fixedBals 
                  fixedAmortFactor 
                    | all (== 0) fixedBals = replicate (pred flowSize) 0.0
                    | otherwise =  zipWith divideBB fixedPrincipalFlow (init fixedBals)
                  fixRates = calcIntRates DC_ACT_365F fixRate ds
                  -- float rate cashflow
                  floatPrincipalFlow = zipWith (-) principalFlows fixedPrincipalFlow
                  
                  rs = (\(a,iRate,b,c) -> a) <$> floaterList      -- portion of each floater
                  spds = (\(a,iRate,b,c) -> b) <$> floaterList    -- spreads
                  indexes = (\(a,iRate,b,c) -> c) <$> floaterList -- indexes
                  floaterSize = length rs
                  -- float bal brekdown by index
                  floatBalsBreakDown = (\r -> flip mulBR r <$> totalBals) <$> rs
                  -- float principal flow breakdown by index
                  floatPrincipalFloat = diffNum <$> floatBalsBreakDown 
                  floatPrincipalFactor = (\(floatPrin,fBals) -> zipWith divideBB floatPrin (init fBals)) <$> (zip floatPrincipalFloat floatBalsBreakDown) 
                  recoveryLag = case mPassump of 
                                  Nothing -> 0 
                                  Just passump -> fromMaybe 0 $ getRecoveryLagFromAssumption passump
                  curveDatesLength = flowSize + recoveryLag
                in
                  do
                    assumptionInput <- case mPassump of 
                                        Just pAssump -> buildAssumptionPpyDefRecRate a ds pAssump 
                                        Nothing -> Right (replicate (pred flowSize) 0.0, replicate (pred flowSize) 0.0, 0.0, 0)
                    fixedCashFlow <- projFixCfwithAssumption (fixedBals, ds, fixedAmortFactor, replicate flowSize (ByRate fixRate), dp)
                                                             assumptionInput
                                                             begDate
                    floatedCashFlow <- traverse 
                                        (\x -> projIndexCashflows x dp assumptionInput mRates) 
                                        $ zip4 floatBalsBreakDown
                                               (replicate floaterSize ds)
                                               indexes
                                               spds 
                    return (fixedCashFlow, floatedCashFlow)



instance Ast.Asset ProjectedCashFlow where

    getCurrentBal (ProjectedByFactor ((_,begBal):_) _ _ _) = begBal
    getCurrentBal (ProjectedCashflow begBal _ _) = 0.0

    getOriginBal x = getCurrentBal x
    getOriginRate (ProjectedByFactor cf _ (fixRatePortion,fixRate) floatRatePortions)
      = let
          avgFloatRate = weightedBy ((view _1) <$> floatRatePortions) (toRational . (view _2) <$> floatRatePortions)
          floatRatePortion = sum $ (view _1) <$> floatRatePortions
        in 
          fromRational $ weightedBy [fixRatePortion,(1-fixRatePortion)] [toRational fixRate,avgFloatRate]
    getOriginRate (ProjectedCashflow _ flows _) = 0


    isDefaulted f = False
    getOriginDate (ProjectedByFactor ((startDate,cf):_) _ _ _)= startDate
    getOriginDate (ProjectedCashflow _ ((startDate,_,_):_) _ )= startDate

    getCurrentRate f = getOriginRate f

    calcCashflow f@(ProjectedByFactor cf _ fxPortion floatPortion) d mRate
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f Nothing mRate
          return $ foldl CF.combine fixedCashFlow floatedCashFlow

    calcCashflow f@(ProjectedCashflow (begBal,begDate) flows _) d _
      = let
          bals = tail $ scanl (\bal (_,p,_) -> bal - p) begBal flows
          txns = [ CF.MortgageFlow date b principal interest 0 0 0 0 0 Nothing Nothing Nothing | (b, (date, principal, interest)) <- (zip bals flows) ]
        in
          -- TODO need to verify begbal with principal/ need to fix rounding issue
          return $ CF.CashFlowFrame (begBal, begDate, Nothing) txns


    projCashflow a@(ProjectedCashflow (begBal,begDate) flows dp ) asOfDay (pAssump, _, _) _
      = let 
          flowSize = length flows
          ds = (view _1) <$> flows
          fixedPrincipalFlow = (view _2) <$> flows
          bals = scanl (\bal p -> bal - p) begBal fixedPrincipalFlow
          ints = (ByAmount.(view _3)) <$> flows
          fixedAmortFactor 
            | all (== 0) bals = replicate (pred flowSize) 0.0
            | otherwise =  zipWith divideBB fixedPrincipalFlow (init bals)
        in 
          do
            assumptionInput <- buildAssumptionPpyDefRecRate a (begDate:ds) pAssump 
            fixedCashFlow <- projFixCfwithAssumption (bals, begDate:ds, fixedAmortFactor, (ByAmount 0):ints, dp)
                                                     assumptionInput
                                                     begDate
            return (fixedCashFlow, Map.empty)


    projCashflow f asOfDay (pAssump, _, _) mRates
      = do
          (fixedCashFlow, floatedCashFlow) <- seperateCashflows f (Just pAssump) mRates
          return (foldl CF.combine fixedCashFlow floatedCashFlow, Map.empty)
    

    projCashflow a b c d = Left $ "Failed to match when proj projected flow with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getBorrowerNum f = 0

    splitWith f rs = [f]
