{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module AssetClass.AssetBase 
  (Installment(..),Lease(..),OriginalInfo(..),Status(..)
  ,LeaseStepUp(..),AccrualPeriod(..),PrepayPenaltyType(..)
  ,AmortPlan(..),Loan(..),Mortgage(..),AssetUnion(..),MixedAsset(..),FixedAsset(..)
  ,AmortRule(..),Capacity(..),AssociateExp(..),AssociateIncome(..),ReceivableFeeType(..),Receivable(..)
  ,ProjectedCashFlow(..),Obligor(..),LeaseRateCalc(..)
  ,calcAssetPrinInt, calcPmt ,ProjectedFlow ,ScheduleBalance
  )
  where

import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
--import Asset

import Data.OpenApi hiding (Server,contentType)

import Types hiding (startDate)
import DateUtil (genSerialDatesTill2)
import qualified Data.Time as T
import Data.Ratio
import Data.Proxy
import Data.Decimal
import Util
import qualified Data.Map as Map
import qualified InterestRate as IR
import qualified Cashflow as CF
import Control.Lens hiding (element,Index)
import Control.Lens.TH



type DailyRate = Balance

data AmortPlan = Level                    -- ^ for mortgage / french system  -> fixed payment each period which consist of increasing princial and decreasing interest.
                | Even                    -- ^ for linear mortgage   -> evenly distributed principal repayment
                | I_P                     -- ^ interest only and principal due at last payment
                | F_P                     -- ^ fee based 
                | PO_FirstN Int       -- ^ 0 fee for first N period
                | IO_FirstN Int AmortPlan -- ^ interest only for first N period
                | NO_FirstN Int AmortPlan -- ^ non payment during first N period
                | ScheduleRepayment Ts (Maybe DatePattern)   -- ^ custom principal follow
                | Balloon Int             -- ^ balloon payment with period N
                deriving (Show, Generic, Ord, Eq)

-- | calculate period payment (Annuity/Level mortgage)
calcPmt :: Balance -> IRate -> Int -> Amount
calcPmt bal rate periods 
  | rate == 0.0 = divideBI bal periods
  | otherwise = 
    let rate' = realToFrac rate :: Double
        logBase = log (1 + rate')
        num = exp (logBase * fromIntegral periods)
        den = num - 1
        r1 = num / den
    in mulBR (realToFrac bal) (toRational (rate' * r1))

type InterestAmount = Balance
type PrincipalAmount = Balance

calcAssetPrinInt :: AmortPlan -> Balance -> IRate -> Int -> Int -> (Balance,Int) -> (InterestAmount, PrincipalAmount)
calcAssetPrinInt pt bal rate ot rt (amortBal, amortTerm) = 
  let 
    interestAccrued = mulBIR bal rate
    pmt = calcPmt bal rate rt
    periodPassed = ot - rt
  in 
    case pt of 
      Level -> (interestAccrued, pmt - interestAccrued)
      Even -> (interestAccrued, bal / fromIntegral rt)
      I_P -> if rt == 1 then
               (interestAccrued, bal)
             else
               (interestAccrued, 0)
      NO_FirstN n _pt -> if periodPassed >= n then 
                          calcAssetPrinInt _pt bal rate ot rt (amortBal, amortTerm)
                         else
                          (0, negate interestAccrued)
      IO_FirstN n _pt -> if periodPassed >= n then 
                          calcAssetPrinInt _pt bal rate ot rt (amortBal, amortTerm)
                         else
                          (interestAccrued, 0)
      
      Balloon n -> if rt == 1 then
                     (interestAccrued, bal)
                   else
                     let 
                       bPmt = calcPmt bal rate (amortTerm - periodPassed)  -- `debug` ("Amort term"++show (amortTerm - periodPassed) <> " rt"++show periodPassed)
                     in 
                       (interestAccrued, bPmt - interestAccrued) -- `debug` ("bal"++show bal++"rate"++show rate++"ot"++show ot++"rt"++show rt++"bPmt"++show bPmt++ "interest"++show interestAccrued)    
                         
      _ -> error $ "unsupported pt "++ show pt


data Status = Current
            | Defaulted (Maybe Date)
            -- | Delinquency (Maybe Int)
            -- | Extended (Maybe T.Day)
            deriving (Show,Generic,Ord,Eq)

data PrepayPenaltyType = ByTerm Int Rate Rate           -- ^ using penalty rate 1 if period < Int, use penalty rate 2 if period > Int
                       | FixAmount Balance (Maybe Int)  -- ^ fixed penalty fee if any prepayment, or it only applies if period < Int
                       | FixPct Rate (Maybe Int)        -- ^ fixed percentage penalty fee as percentage of prepayment, or it only applies if period < Int
                       | Sliding Rate Rate              -- ^ starting with Rate1 at period 1 then decrease by step by rate2
                       | StepDown [(Int,Rate)]          -- ^ first tuple (n,r) ,first n periods use penalty rate r , then next n periods use pentaly rate in next tuple
                       -- | NMonthInterest Int
                       deriving (Show,Generic,Eq,Ord)

data AmortRule = DecliningBalance        -- ^ DecliningBalance Method
               | StraightLine            -- ^ Straight Line Method
               deriving (Show,Generic,Eq,Ord)

data ReceivableFeeType = FixedFee Balance                    -- ^ a flat fee amount
                       | FixedRateFee Rate                   -- ^ a percentage fee against balance for once
                       | FactorFee Rate Int Direction        -- ^ a percentage fee against balance for each period (N days)
                       | AdvanceFee Rate                     -- ^ annualized rate for discount fee based on advance amount
                       | CompoundFee [ReceivableFeeType]     -- ^ compound fee
                       deriving (Show,Generic,Eq,Ord)


data Obligor = Obligor {obligorId :: String
                        , obligorTag :: [String]
                        , obligorFields :: Map.Map String (Either String Double)
                        } deriving (Show,Generic,Eq,Ord)

data LeaseRateCalc = ByDayRate DailyRate DatePattern
                   | ByPeriodRental Balance Period
                   deriving (Show,Generic,Eq,Ord)


data OriginalInfo = MortgageOriginalInfo { originBalance :: Balance
                                          ,originRate :: IR.RateType
                                          ,originTerm :: Int
                                          ,period :: Period
                                          ,startDate :: Date
                                          ,prinType :: AmortPlan 
                                          ,prepaymentPenalty :: Maybe PrepayPenaltyType
                                          ,obligor :: Maybe Obligor }
                  | LoanOriginalInfo { originBalance :: Balance
                                      ,originRate :: IR.RateType
                                      ,originTerm :: Int
                                      ,period :: Period
                                      ,startDate :: Date
                                      ,prinType :: AmortPlan 
                                      ,obligor :: Maybe Obligor }
                  | LeaseInfo { startDate :: Date            -- ^ lease start date
                              ,originTerm :: Int             -- ^ total terms
                              ,originRental :: LeaseRateCalc -- ^ rental by day
                              ,obligor :: Maybe Obligor }       
                  | FixedAssetInfo { startDate :: Date 
                                     ,originBalance :: Balance 
                                     ,residualBalance :: Balance
                                     ,originTerm :: Int
                                     ,period :: Period
                                     ,accRule :: AmortRule
                                     ,capacity :: Capacity }
                  | ReceivableInfo { startDate :: Date
                                   ,originBalance :: Balance
                                   ,originAdvance :: Balance
                                   ,dueDate :: Date
                                   ,feeType :: Maybe ReceivableFeeType
                                   ,obligor :: Maybe Obligor }
                  deriving (Show,Generic,Ord,Eq)


data Installment = Installment OriginalInfo Balance RemainTerms Status
                 | Dummy
                 deriving (Show,Generic,Ord,Eq)

data LeaseStepUp = FlatRate Rate
                 | ByRateCurve [Rate]
                 | ByFlatAmount Balance
                 | ByAmountCurve [Balance]
                 deriving (Show,Generic,Ord,Eq)

data Lease = RegularLease OriginalInfo Balance RemainTerms Status
           | StepUpLease OriginalInfo LeaseStepUp Balance RemainTerms Status
           deriving (Show,Generic,Eq,Ord)

data AccrualPeriod = AccrualPeriod Date DailyRate
                    deriving (Show,Generic,Eq,Ord)

instance TimeSeries AccrualPeriod where 
    getDate (AccrualPeriod d _) = d

data Loan = PersonalLoan OriginalInfo Balance IRate RemainTerms Status
          | DUMMY
          deriving (Show,Generic,Ord,Eq)

data Mortgage = Mortgage OriginalInfo Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | AdjustRateMortgage OriginalInfo IR.ARM Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | ScheduleMortgageFlow Date [CF.TsRow] DatePattern
              deriving (Show,Generic,Eq,Ord)

data StudentLoan = StudentLoan OriginalInfo Balance IRate RemainTerms Status
                 | DUMMY3
                 deriving (Show,Generic,Eq,Ord)


type FixRatePortion   = (Rate, IRate)
type FloatRatePortion = (Rate, IRate, Spread, Index)
type ScheduleBalance = (Date, Balance)
type ScheduleFlow = (Date, Principal, Interest)
type ProjectedFlow = Balance

data ProjectedCashFlow = ProjectedByFactor [ScheduleBalance] DatePattern FixRatePortion [FloatRatePortion]
                       | ProjectedCashflow (Balance,Date) [ScheduleFlow] DatePattern
                       deriving (Show,Generic,Eq,Ord)


data Receivable = Invoice OriginalInfo Status
                | DUMMY4
                deriving (Show,Generic,Eq,Ord)

data MixedAsset = MixedPool (Map.Map String [AssetUnion])
                | DUMMY2
                deriving (Show,Generic,Eq,Ord)

type LineOfCredit = Maybe Balance

data Revolver = Heloc OriginalInfo LineOfCredit Balance IRate RemainTerms (Maybe BorrowerNum) Status
              | DUMMY5
              deriving (Show,Generic,Eq,Ord)

-- FixedAsset 
data Capacity = FixedCapacity Balance
              | CapacityByTerm [(Int,Balance)]
              deriving (Show,Generic,Ord,Eq)

data AssociateExp = ExpPerPeriod Balance 
                  | ExpPerUnit Balance
                  deriving (Show,Generic,Ord,Eq)

data AssociateIncome = IncomePerPeriod Balance 
                      | IncomePerUnit Balance
                      deriving (Show,Generic,Ord,Eq)

data FixedAsset = FixedAsset OriginalInfo Balance RemainTerms
                | Dummy5
                deriving (Show,Generic,Eq,Ord)


-- Base type to hold all asset types
data AssetUnion = MO Mortgage
                | LO Loan
                | IL Installment
                | LS Lease
                | FA FixedAsset
                | RE Receivable
                | PF ProjectedCashFlow
                deriving (Show, Generic,Ord,Eq)


-- ^ end date of an original term expressed in payment periods
termEndDate :: Period -> Date -> Int -> Date
termEndDate p sd n = case p of 
    Daily        -> T.addDays (toInteger n) sd
    Weekly       -> T.addDays (toInteger n * 7) sd
    BiWeekly     -> T.addDays (toInteger n * 14) sd
    Monthly      -> T.addGregorianDurationClip (T.CalendarDiffDays (toInteger n) 0) sd
    Quarterly    -> T.addGregorianDurationClip (T.CalendarDiffDays (toInteger n * 3) 0) sd
    SemiAnnually -> T.addGregorianDurationClip (T.CalendarDiffDays (toInteger n * 6) 0) sd
    Annually     -> T.addGregorianDurationClip (T.CalendarDiffDays (toInteger n * 12) 0) sd


-- ^ reset dates of a floating rate type over the asset's original term
rateResetDatesOverTerm :: IR.RateType -> Date -> Int -> Period -> Dates
rateResetDatesOverTerm (IR.Floater _ _ _ _ dp _ _ _) sd n p 
  = genSerialDatesTill2 NO_IE sd dp (termEndDate p sd n)
rateResetDatesOverTerm _ _ _ _ = []


instance IR.UseRate AssetUnion where
  getIndex (MO ma) = IR.getIndex ma
  getIndex (LO ma) = IR.getIndex ma
  getIndex (IL ma) = IR.getIndex ma
  getIndex (LS ma) = IR.getIndex ma
  getIndex (FA ma) = IR.getIndex ma
  getIndex (RE ma) = IR.getIndex ma
  getIndex (PF ma) = IR.getIndex ma

  getIndexes (MO ma) = IR.getIndexes ma
  getIndexes (LO ma) = IR.getIndexes ma
  getIndexes (IL ma) = IR.getIndexes ma
  getIndexes (LS ma) = IR.getIndexes ma
  getIndexes (FA ma) = IR.getIndexes ma
  getIndexes (RE ma) = IR.getIndexes ma
  getIndexes (PF ma) = IR.getIndexes ma

  isAdjustableRate (MO ma) = IR.isAdjustableRate ma
  isAdjustableRate (LO ma) = IR.isAdjustableRate ma
  isAdjustableRate (IL ma) = IR.isAdjustableRate ma
  isAdjustableRate (LS ma) = IR.isAdjustableRate ma
  isAdjustableRate (FA ma) = IR.isAdjustableRate ma
  isAdjustableRate (RE ma) = IR.isAdjustableRate ma
  isAdjustableRate (PF ma) = IR.isAdjustableRate ma

  getResetDates (MO ma) = IR.getResetDates ma
  getResetDates (LO ma) = IR.getResetDates ma
  getResetDates (IL ma) = IR.getResetDates ma
  getResetDates (LS ma) = IR.getResetDates ma
  getResetDates (FA ma) = IR.getResetDates ma
  getResetDates (RE ma) = IR.getResetDates ma
  getResetDates (PF ma) = IR.getResetDates ma

  getSpread (MO ma) = IR.getSpread ma
  getSpread (LO ma) = IR.getSpread ma
  getSpread (IL ma) = IR.getSpread ma
  getSpread (LS ma) = IR.getSpread ma
  getSpread (FA ma) = IR.getSpread ma
  getSpread (RE ma) = IR.getSpread ma
  getSpread (PF ma) = IR.getSpread ma


instance IR.UseRate Mortgage where 
  getIndex (Mortgage oi@MortgageOriginalInfo{ originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _ _) = Just idx 
  getIndex Mortgage {} = Nothing
  getIndex (AdjustRateMortgage oi@MortgageOriginalInfo{ originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _ _ _) = Just idx 
  getIndex AdjustRateMortgage {} = Nothing

  -- ^ look-up from the single origin index
  getIndexes m@Mortgage {} = case IR.getIndex m of
                                Just idx -> Just [idx]
                                _ -> Nothing
  getIndexes m@AdjustRateMortgage {} = case IR.getIndex m of
                                          Just idx -> Just [idx]
                                          _ -> Nothing

  isAdjustableRate m@Mortgage {} = case IR.getIndex m of
                                      Just _ -> True
                                      _ -> False
  isAdjustableRate m@AdjustRateMortgage {} = case IR.getIndex m of
                                                Just _ -> True
                                                _ -> False

  -- ^ spread from the origin rate
  getSpread (Mortgage oi@MortgageOriginalInfo{originRate = rt} _ _ _ _ _) = IR._getSpread rt
  getSpread (AdjustRateMortgage oi@MortgageOriginalInfo{originRate = rt} _ _ _ _ _ _) = IR._getSpread rt

  -- ^ reset dates of the origin floating rate over the originated term
  getResetDates (Mortgage oi@MortgageOriginalInfo{originRate = rt, originTerm = ot, startDate = sd, period = p} _ _ _ _ _) 
    = rateResetDatesOverTerm rt sd ot p
  getResetDates (AdjustRateMortgage oi@MortgageOriginalInfo{originRate = rt, originTerm = ot, startDate = sd, period = p} _ _ _ _ _ _)
    = rateResetDatesOverTerm rt sd ot p

instance IR.UseRate Loan where
  getIndex (PersonalLoan oi@LoanOriginalInfo{originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _ _) = Just idx 
  getIndex PersonalLoan {} = Nothing

  getIndexes p@PersonalLoan {} = case IR.getIndex p of
                                    Just idx -> Just [idx]
                                    _ -> Nothing

  isAdjustableRate p@PersonalLoan {} = case IR.getIndex p of
                                          Just _ -> True
                                          _ -> False

  getSpread (PersonalLoan oi@LoanOriginalInfo{originRate = rt} _ _ _ _) = IR._getSpread rt

  getResetDates (PersonalLoan oi@LoanOriginalInfo{originRate = rt, originTerm = ot, startDate = sd, period = p} _ _ _ _)
    = rateResetDatesOverTerm rt sd ot p

instance IR.UseRate Installment where 
  getIndex (Installment oi@LoanOriginalInfo{originRate = IR.Floater _ idx _ _ _ _ _ _ } _ _ _) = Just idx 
  getIndex Installment {} = Nothing

  getIndexes p@Installment {} = case IR.getIndex p of
                                   Just idx -> Just [idx]
                                   _ -> Nothing

  isAdjustableRate p@Installment {} = case IR.getIndex p of
                                         Just _ -> True
                                         _ -> False

  getSpread (Installment oi@LoanOriginalInfo{originRate = rt} _ _ _) = IR._getSpread rt

  getResetDates (Installment oi@LoanOriginalInfo{originRate = rt, originTerm = ot, startDate = sd, period = p} _ _ _)
    = rateResetDatesOverTerm rt sd ot p
  
instance IR.UseRate Lease where
  getIndex :: Lease -> Maybe Index
  getIndex _ = Nothing
  -- ^ the rental rate is contractual (fixed by day), no floating index
  getIndexes _ = Nothing
  isAdjustableRate _ = False
  getResetDates _ = []
  getSpread _ = Nothing

instance IR.UseRate FixedAsset where
  getIndex _ = Nothing
  -- ^ depreciation-type asset with no indexed rate
  getIndexes _ = Nothing
  isAdjustableRate _ = False
  getResetDates _ = []
  getSpread _ = Nothing

instance IR.UseRate Receivable where
  getIndex _ = Nothing
  -- ^ receivable pays a face balance, no indexed rate
  getIndexes _ = Nothing
  isAdjustableRate _ = False
  getResetDates _ = []
  getSpread _ = Nothing

instance IR.UseRate ProjectedCashFlow where 
  getIndex (ProjectedByFactor cf _ _ []) = Nothing
  getIndex (ProjectedByFactor cf _ _ (f:fs)) = Just $ (\(_,a,b,c) -> c) f 
  getIndex (ProjectedCashflow _ fs _) = Nothing
  getIndexes (ProjectedByFactor cf _ _ fs ) = Just $ (\(a,_,b,c) -> c) <$> fs
  getIndexes (ProjectedCashflow _ fs _) = Nothing
  isAdjustableRate (ProjectedByFactor _ _ _ []) = False
  isAdjustableRate (ProjectedByFactor _ _ _ _) = True
  isAdjustableRate (ProjectedCashflow _ _ _) = False
  getResetDates _ = []
  getSpread _ = Nothing


$(concat <$> traverse (deriveJSON defaultOptions) [''Obligor, ''OriginalInfo, ''FixedAsset, ''AmortPlan, ''PrepayPenaltyType
    , ''Capacity, ''AmortRule, ''ReceivableFeeType, ''LeaseRateCalc])


makePrisms ''OriginalInfo

$(deriveJSON defaultOptions ''AssociateExp)
$(deriveJSON defaultOptions ''AssociateIncome)
$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''Installment)
$(deriveJSON defaultOptions ''LeaseStepUp)
$(deriveJSON defaultOptions ''Mortgage)
$(deriveJSON defaultOptions ''Loan)
$(deriveJSON defaultOptions ''Lease)
$(deriveJSON defaultOptions ''Receivable)
$(deriveJSON defaultOptions ''ProjectedCashFlow)
$(deriveJSON defaultOptions ''AssetUnion)
instance ToSchema Capacity
instance ToSchema AmortRule
instance ToSchema (Ratio Integer) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema (Decimal) where 
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema PrepayPenaltyType
instance ToSchema (TsPoint Int)
instance ToSchema Ts
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint IRate)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema (RoundingBy IRate)
instance ToSchema Obligor
instance ToSchema Index
instance ToSchema DayCount
instance ToSchema Direction
instance ToSchema AmortPlan
instance ToSchema CutoffType
instance ToSchema DatePattern
instance ToSchema IR.RateType
instance ToSchema CF.TsRow
instance ToSchema Period
instance ToSchema IR.ARM
instance ToSchema Status
instance ToSchema ReceivableFeeType
instance ToSchema LeaseRateCalc
instance ToSchema OriginalInfo
instance ToSchema Mortgage 
