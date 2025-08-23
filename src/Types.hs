{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Types
  (DayCount(..),DateType(..)
  ,DatePattern(..)
  ,BondName,BondNames,FeeName,FeeNames,AccName,AccNames,AccountName
  ,Ts(..),TsPoint(..),PoolSource(..)
  ,PerPoint(..),PerCurve(..),getValFromPerCurve
  ,Period(..), Threshold(..)
  ,RangeType(..),CutoffType(..),DealStatus(..)
  ,Balance,Index(..)
  ,Cmp(..),TimeHorizion(..)
  ,Date,Dates,TimeSeries(..),IRate,Amount,Rate,StartDate,EndDate,Lag
  ,Spread,Floor,Cap,Interest,Principal,Cash,Default,Loss,Rental,PrepaymentPenalty
  ,SplitType(..),BookItem(..),BookItems,BalanceSheetReport(..),CashflowReport(..)
  ,Floater,CeName,RateAssumption(..)
  ,PrepaymentRate,DefaultRate,RecoveryRate,RemainTerms,Recovery,Prepayment
  ,Table(..),lookupTable,Direction(..),epocDate,BorrowerNum
  ,Txn(..),TxnComment(..)
  ,RoundingBy(..),DateDirection(..)
  ,BookDirection(..),IRR(..),DealCycle(..),Limit(..),Pre(..)
  ,Liable(..),CumPrepay,CumDefault,CumDelinq,CumPrincipal,CumLoss,CumRecovery,PoolId(..)
  ,DealName,lookupIntervalTable,CutoffFields(..),PriceResult(..)
  ,DueInt,DuePremium, DueIoI,DateVector,DealStats(..)
  ,PricingMethod(..),CustomDataType(..),ResultComponent(..),DealStatType(..)
  ,ActionWhen(..),DealStatFields(..)
  ,getDealStatType,getPriceValue,preHasTrigger
  ,DealStatRtn,Queryable(..) 
  ,MyRatio,HowToPay(..),BondPricingMethod(..),InvestorAction(..)
  ,_BondTxn ,_InspectBal, _IrrResult,DueType(..)
  ,EvalExpr(..),ErrorRep,Accruable(..),Payable(..),Drawable(..)
  ,SupportAvailType(..),updateSupportAvailType
  )
  where

import qualified Data.Text as Text
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Time as Time
import qualified Data.Time.Format as TF
import qualified Data.Map as Map
import qualified Data.List.Split
import Text.Regex.Base
import Text.Regex.PCRE
import GHC.Generics
import Language.Haskell.TH

import Control.Lens hiding (element,Index,Empty)
import Control.Lens.TH

import Text.Read (readMaybe, get)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.GADT.TH

import Data.Ratio (Ratio, numerator, denominator)
import Data.Text (pack)
import Control.DeepSeq (NFData,rnf)

import Data.Scientific (fromRationalRepetend,formatScientific, Scientific,FPFormat(Fixed))

import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed hiding (Ratio)
import Data.Decimal
import Data.Ix


import Data.List (intercalate, findIndex, find)
-- import Cashflow (CashFlowFrame)

-- import Web.Hyperbole hiding (All,Fixed)

import Debug.Trace
-- import qualified Cashflow as CF
debug = flip trace



type BondName = String
type BondNames = [String]
type FeeName = String
type FeeNames = [String]
type AccName = String
type AccountName = String
type AccNames = [String]
type CeName = String
type Comment = String

type Date = Time.Day
type Dates = [Time.Day]
type StartDate = Date
type EndDate = Date
type LastIntPayDate = Date

type Balance = Centi
type Amount = Balance
type Principal = Balance
type Valuation = Balance

type Interest = Balance
type Default = Balance
type Loss = Balance
type Cash = Balance
type Recovery = Balance
type Prepayment = Balance
type Rental = Balance
type PrepaymentPenalty = Balance
type CumPrepay = Balance
type CumPrincipal = Balance
type CumDefault = Balance
type CumDelinq = Balance
type CumLoss = Balance
type CumRecovery = Balance
type AccruedInterest = Balance

type PerFace = Micro
type WAL = Balance
type Duration = Micro
type Convexity = Micro
type Yield = Micro
type IRR = Micro

type Rate = Rational  -- general Rate like pool factor
type PrepaymentRate = Rate
type DefaultRate = Rate
type RecoveryRate = Rate

type IRate = Micro    -- Interest Rate Type
type Spread = Micro
type Floor = Micro
type Cap = Micro

type RemainTerms = Int
type BorrowerNum = Int
type Lag = Int

type ErrorRep = String


data Index = LPR5Y
            | LPR1Y
            | LIBOR1M
            | LIBOR3M
            | LIBOR6M
            | LIBOR1Y
            | USTSY1Y
            | USTSY2Y
            | USTSY3Y
            | USTSY5Y
            | USTSY7Y
            | USTSY10Y
            | USTSY20Y
            | USTSY30Y
            | USCMT1Y
            | PRIME
            | COFI
            | SOFR1M
            | SOFR3M
            | SOFR6M
            | SOFR1Y
            | EURIBOR1M
            | EURIBOR3M
            | EURIBOR6M
            | EURIBOR12M
            | BBSW
            | IRPH --  The IRPH (Índice de Referencia de Préstamos Hipotecarios) is a reference index used in Spain to fix the interest rate of mortgage loans
            | SONIA 
            -- deriving (Show,Eq,Generic,Ord,Read, Bounded, Enum, Finite, Named, ProtoEnum)
            deriving (Show,Eq,Generic,Ord,Read)

type Floater = (Index,Spread)

epocDate = Time.fromGregorian 1970 1 1
-- http://www.deltaquants.com/day-count-conventions
data DayCount = DC_30E_360       -- ^ ISMA European 30S/360 Special German Eurobond Basis
              | DC_30Ep_360      -- ^ 30E+/360
              | DC_ACT_360       -- ^ Actual/360 , French
              | DC_ACT_365
              | DC_ACT_365A      -- ^ Actual/365 Actual 
              | DC_ACT_365L      -- ^ Actual/365 Leap Year
              | DC_NL_365        -- ^ Actual/365 No leap year
              | DC_ACT_365F      -- ^ Actual /365 Fixed, English
              | DC_ACT_ACT       -- ^ Actual/Actual ISDA 
              | DC_30_360_ISDA   -- ^ IDSA
              | DC_30_360_German -- ^ Gernman
              | DC_30_360_US     -- ^ 30/360 US Municipal , Bond basis
              deriving (Show,Eq,Generic,Ord,Read)


data DateType = ClosingDate             -- ^ deal closing day
              | CutoffDate              -- ^ after which, the pool cashflow was aggregated to SPV
              | FirstPayDate            -- ^ first payment day for bond/waterfall to run with
              | NextPayDate
              | NextCollectDate
              | FirstCollectDate        -- ^ first collection day for pool
              | LastCollectDate         -- ^ last collection day for pool
              | LastPayDate            -- ^ last payment day for bond/waterfall 
              | StatedMaturityDate      -- ^ sated maturity date, all cashflow projection/deal action stops by
              | DistributionDates       -- ^ distribution date for waterfall
              | CollectionDates         -- ^ collection date for pool
              | CustomExeDates String   -- ^ custom execution date
              deriving (Show,Ord,Eq,Generic,Read)


data DatePattern = MonthEnd
                 | QuarterEnd
                 | YearEnd 
                 | MonthFirst
                 | QuarterFirst
                 | MidYear
                 | YearFirst
                 | MonthDayOfYear Int Int  -- T.MonthOfYear T.DayOfMonth
                 | DayOfMonth Int -- T.DayOfMonth 
                 | SemiAnnual (Int, Int) (Int, Int)
                 | CustomDate [Date]
                 | SingletonDate Date
                 | DaysInYear [(Int, Int)] -- MM/DD
                 | EveryNMonth Date Int
                 | Weekday Int 
                 | AllDatePattern [DatePattern]
                 | StartsExclusive Date DatePattern -- TODO depricated
                 | StartsAt CutoffType Date DatePattern
                 | EndsAt CutoffType Date DatePattern
                 | Exclude DatePattern [DatePattern]
                 | OffsetBy DatePattern Int
                 -- | DayOfWeek Int -- T.DayOfWeek
                 deriving (Show, Eq, Generic, Ord, Read)


data Period = Daily 
            | Weekly 
            | BiWeekly
            | Monthly 
            | Quarterly 
            | SemiAnnually 
            | Annually
            deriving (Show,Eq,Generic,Ord)

type DateVector = (Date, DatePattern)

data RoundingBy a = RoundCeil a 
                  | RoundFloor a
                  deriving (Show, Generic, Eq, Ord, Read)

type DealName = String

data PoolId = PoolName String                         -- ^ pool name
            | PoolConsol                              -- ^ consolidate pool ( the only pool )
            | DealBondFlow DealName String Date Rate  -- ^ bond flow from deal
            deriving (Eq,Ord,Generic)

instance Show PoolId where
  show (PoolName n)  = n
  show PoolConsol = "PoolConsol"
  show (DealBondFlow dn bn sd r) = "BondFlow:"++dn++":"++bn++":"++show sd++":"++show r

instance (Read PoolId) where
  readsPrec d "PoolConsol" = [(PoolConsol,"")]
  readsPrec d rStr = 
    let 
      pn = Data.List.Split.splitOn ":" rStr
    in
      case pn of
        [dn,bn,sd,r] -> 
          let 
            sd' = TF.parseTimeOrError True TF.defaultTimeLocale "%Y-%m-%d" sd
            r' = read r::Rate
          in 
            [(DealBondFlow dn bn sd' r',"")]
        ["PoolName",pn] -> [(PoolName pn,"")]
        _ -> error $ "Invalid PoolId: "++ show pn




data Cmp = G      -- ^ Greater than 
         | GE     -- ^ Greater Equal than
         | L      -- ^ Less than
         | LE     -- ^ Less Equal than
         | E      -- ^ Equals to
         deriving (Generic,Eq,Ord,Read)

instance Show Cmp where
  show :: Cmp -> String
  show G  = ">"
  show GE = ">="
  show L  = "<"
  show LE = "<="
  show E  = "=="


data PoolSource = CollectedInterest               -- ^ interest
                | CollectedPrincipal              -- ^ schdule principal
                | CollectedRecoveries             -- ^ recoveries 
                | CollectedPrepayment             -- ^ prepayment
                | CollectedPrepaymentPenalty      -- ^ prepayment pentalty
                | CollectedRental                 -- ^ rental from pool
                | CollectedFeePaid                -- ^ fee from pool
                | CollectedCash                   -- ^ cash from pool
                | NewDefaults                     -- ^ new defaults in balance
                | NewLosses                       -- ^ new losses in balance
                | NewDelinquencies                -- ^ new delinquencies in balance
                | CurBalance                      -- ^ performing balance
                | CurBegBalance                   -- ^ performing balance at the beginning of the period
                deriving (Show,Ord,Read,Eq, Generic)


data TsPoint a = TsPoint Date a
                deriving (Show,Eq,Read,Generic)

instance Ord a => Ord (TsPoint a) where
  compare (TsPoint d1 tv1) (TsPoint d2 tv2) = compare d1 d2

data PerPoint a = PerPoint Int a
                deriving (Show,Eq,Read,Generic)

data PerCurve a = CurrentVal [PerPoint a]
                | WithTrailVal [PerPoint a]
                deriving (Show,Eq,Read,Generic,Ord)

getValFromPerCurve :: PerCurve a -> DateDirection -> CutoffType -> Int -> Maybe a
getValFromPerCurve (WithTrailVal []) _ _ _ = Nothing 
getValFromPerCurve (CurrentVal []) _ _ _ = Nothing 
getValFromPerCurve (CurrentVal (v:vs)) Future p i 
  = let 
      cmp = case p of
              Inc -> (>=)
              Exc -> (>)
    in
      if cmp (getIdxFromPerPoint v) i then 
        Just $ getValFromPerPoint v
      else 
        getValFromPerCurve (CurrentVal vs) Future p i

getValFromPerCurve (CurrentVal vs) Past p i
  = let 
      cmp = case p of
              Inc -> (<=)
              Exc -> (<)
      ps = reverse vs
    in
      case find (\x -> cmp (getIdxFromPerPoint x) i) ps of
        Just rs -> Just $ getValFromPerPoint rs
        Nothing -> Nothing


getValFromPerCurve (WithTrailVal _ps) dr p i 
  = let 
      ps = case dr of 
            Future -> _ps
            Past -> reverse _ps
      cmp = case p of 
              Inc -> (>=)
              Exc -> (>)
    in 
      case find (\x -> cmp (getIdxFromPerPoint x) i) ps of
        Nothing -> Just $ getValFromPerPoint (last ps)
        Just rs -> Just $ getValFromPerPoint rs

getIdxFromPerPoint :: PerPoint a -> Int
getIdxFromPerPoint (PerPoint i _) = i

getValFromPerPoint :: PerPoint a -> a
getValFromPerPoint (PerPoint _ v) = v


instance Ord a => Ord (PerPoint a) where
  compare (PerPoint i _) (PerPoint j _) = compare i j

data RangeType = II     -- ^ include both start and end date
               | IE     -- ^ include start date ,but not end date
               | EI     -- ^ exclude start date but include end date
               | EE     -- ^ exclude either start date and end date 
               | NO_IE  -- ^ no handling on start date and end date
               deriving (Show,Eq,Read,Generic,Ord)

data CutoffType = Inc 
                | Exc
                deriving (Show,Ord,Read,Generic,Eq)

data DateDirection = Future 
                   | Past
                   deriving (Show,Read,Generic)

data InvestorAction = Buy 
                    | Sell
                    deriving (Show,Ord,Read,Generic,Eq)


data SupportAvailType = ByAvailAmount Balance 
                      | Unlimit
                      deriving (Show, Eq, Ord, Generic, Read)

updateSupportAvailType :: (Balance -> Balance) -> SupportAvailType -> SupportAvailType
updateSupportAvailType f (ByAvailAmount b) = ByAvailAmount (f b)
updateSupportAvailType _ Unlimit = Unlimit


$(deriveJSON defaultOptions ''SupportAvailType)

class TimeSeries ts where 
    cmp :: ts -> ts -> Ordering
    cmp t1 t2 = compare (getDate t1) (getDate t2)
    sameDate :: ts -> ts -> Bool
    sameDate t1 t2 =  getDate t1 == getDate t2
    getDate :: ts -> Date
    getDates :: [ts] -> [Date]
    getDates ts = [ getDate t | t <- ts ]
    filterByDate :: [ts] -> Date -> [ts]
    filterByDate ts d = filter (\x -> getDate x == d ) ts
    sliceBy :: RangeType -> StartDate -> EndDate -> [ts] -> [ts]
    sliceBy rt sd ed ts
      = case rt of 
          II -> filter (\x -> getDate x >= sd && getDate x <= ed ) ts 
          IE -> filter (\x -> getDate x >= sd && getDate x < ed ) ts 
          EI -> filter (\x -> getDate x > sd && getDate x <= ed) ts 
          EE -> filter (\x -> getDate x > sd && getDate x < ed ) ts 
          _  -> error "Not support NO_IE for sliceBy in TimeSeries"
    cutBy :: CutoffType -> DateDirection -> Date -> [ts] -> [ts]
    cutBy ct dd d ts 
      = case (ct,dd) of
          (Inc, Future) ->  filter (\x -> getDate x >= d) ts
          (Inc, Past) ->  filter (\x -> getDate x <= d) ts
          (Exc, Future) ->  filter (\x -> getDate x > d) ts
          (Exc, Past) ->  filter (\x -> getDate x < d) ts

    cmpWith :: ts -> Date -> Ordering
    cmpWith t d = compare (getDate t) d

    isAfter :: ts -> Date -> Bool 
    isAfter t d = getDate t > d
    isOnAfter :: ts -> Date -> Bool 
    isOnAfter t d = getDate t >= d
    isBefore :: ts -> Date -> Bool 
    isBefore t d = getDate t < d
    isOnBefore :: ts -> Date -> Bool 
    isOnBefore t d = getDate t <= d

    splitBy :: Date -> CutoffType -> [ts] -> ([ts],[ts])
    splitBy d ct tss = 
      let 
        ffunR x = case ct of
                    Inc -> getDate x > d -- include ts in the Left
                    Exc -> getDate x >= d  -- 
        ffunL x = case ct of
                    Inc -> getDate x <= d -- include ts in the Left
                    Exc-> getDate x < d  -- 
      in 
        (filter ffunL tss, filter ffunR tss)

    getByDate :: Date -> [ts] -> Maybe ts
    getByDate d ts = case filterByDate ts d of 
                      [] -> Nothing
                      (x:_) -> Just x
 
-- ^ different types of curves, which determine how to interpolate between two points
data Ts = FloatCurve [TsPoint Rational]
        | BoolCurve [TsPoint Bool]
        | BalanceCurve [TsPoint Balance]
        | LeftBalanceCurve [TsPoint Balance]
        | RatioCurve [TsPoint Rational]
        | ThresholdCurve [TsPoint Rational]
        | IRateCurve [TsPoint IRate]
        | FactorCurveClosed [TsPoint Rational] Date
        | PricingCurve [TsPoint Rational] 
        | PeriodCurve [TsPoint Int]
        | IntCurve [TsPoint Int]
        deriving (Show,Eq,Ord,Read,Generic)


data Direction = Up 
               | Down
               deriving (Show,Read,Generic,Eq,Ord)

-- ^ direction of the transaction, in terms of the book keeping
data BookDirection = Credit
                   | Debit
                   deriving (Show,Ord, Eq,Read, Generic)


type DueInt = Balance
type DuePremium = Balance
type DueIoI = Balance

data DealCycle = EndCollection         -- ^ | collection period <HERE> collection action , waterfall action
               | EndCollectionWF       -- ^ | collection period  collection action <HERE>, waterfall action
               | BeginDistributionWF   -- ^ | collection period  collection action , <HERE>waterfall action
               | EndDistributionWF     -- ^ | collection period  collection action , waterfall action<HERE>
               | InWF                  -- ^ | collection period  collection action , waterfall <HERE> action
               deriving (Show, Ord, Eq, Read, Generic)

-- ^ different status of the deal
data DealStatus = DealAccelerated (Maybe Date)      -- ^ Deal is accelerated status with optinal accerlerated date
                | DealDefaulted (Maybe Date)        -- ^ Deal is defaulted status with optinal default date
                | Amortizing                        -- ^ Deal is amortizing 
                | Revolving                         -- ^ Deal is revolving
                | PreClosing DealStatus             -- ^ Deal is not closed, but has a closing date
                | Warehousing (Maybe DealStatus)    -- ^ Deal is not closed, but closing date is not determined yet
                | Called                            -- ^ Deal is called
                | Ended (Maybe Date)                -- ^ Deal is marked as closed
                deriving (Show,Ord,Eq,Read, Generic)

-- ^ pricing methods for assets
data PricingMethod = BalanceFactor Rate Rate          -- ^ [balance] to be multiply with rate1 and rate2 if status of asset is "performing" or "defaulted"
                   | BalanceFactor2 Rate Rate Rate    -- ^ [balance] by performing/delinq/default factor
                   | DefaultedBalance Rate            -- ^ [balance] only liquidate defaulted balance
                   | PV IRate Rate                    -- ^ discount factor, recovery pct on default
                   | PVCurve Ts                       -- ^ [CF] Pricing cashflow with a Curve
                   | PvRate IRate                     -- ^ [CF] Pricing cashflow with a constant rate
                   | PvWal Ts
                   | PvByRef DealStats                -- ^ [CF] Pricing cashflow with a ref rate
                   | Custom Rate                      -- ^ custom amount
                   deriving (Show, Eq ,Generic, Read, Ord)

-- ^ pricing methods for bonds
data BondPricingMethod = BondBalanceFactor Rate 
                        | PvBondByRate Rate
                        | PvBondByCurve Ts
                        deriving (Show, Eq ,Generic, Read, Ord)


-- ^ condition which can be evaluated to a boolean value
data Pre = IfZero DealStats
         | If Cmp DealStats Balance
         | IfRate Cmp DealStats Micro
         | IfCurve Cmp DealStats Ts
         | IfByPeriodCurve Cmp DealStats DealStats (PerCurve Balance)
         | IfRateCurve Cmp DealStats Ts
         | IfRateByPeriodCurve Cmp DealStats DealStats (PerCurve Rate)
         | IfIntCurve Cmp DealStats Ts
         -- Integer
         | IfInt Cmp DealStats Int
         | IfIntBetween DealStats RangeType Int Int
         | IfIntIn DealStats [Int]
         -- Dates
         | IfDate Cmp Date
         | IfDateBetween RangeType Date Date
         | IfDateIn Dates
         -- Bool
         | IfBool DealStats Bool
         -- compare deal status 
         | If2 Cmp DealStats DealStats
         | IfRate2 Cmp DealStats DealStats
         | IfInt2 Cmp DealStats DealStats
         -- | IfRateCurve DealStats Cmp Ts
         | IfDealStatus DealStatus
         | Always Bool
         | IfNot Pre
         | Any [Pre]
         | All [Pre]                            -- ^ 
         deriving (Show,Generic,Eq,Ord,Read)


data Table a b = ThresholdTable [(a,b)]
                 deriving (Show,Eq,Ord,Read,Generic)


data ActionType = ActionResetRate  -- ^ reset interest rate from curve
                | ActionAccrue     -- ^ accrue liablity
                 deriving (Show,Eq,Ord,Read,Generic)

-- ^ comment of the transaction in the accounts
data TxnComment = PayInt [BondName]
                | PayYield BondName 
                | PayPrin [BondName] 
                | PayGroupPrin [BondName]
                | PayGroupInt [BondName]
                | WriteOff BondName Balance
                | FundWith BondName Balance
                | PayPrinResidual [BondName] 
                | PayFee FeeName
                | SeqPayFee [FeeName] 
                | PayFeeYield FeeName
                | Transfer AccName AccName 
                | TransferBy AccName AccName Limit
                | BookLedgerBy BookDirection String
                | PoolInflow (Maybe [PoolId]) PoolSource
                | LiquidationProceeds [PoolId]
                | LiquidationSupport String
                | LiquidationDraw
                | LiquidationRepay String
                | LiquidationSupportInt Balance Balance
                | BankInt
                | SupportDraw
                | Empty 
                | Tag String
                | UsingDS DealStats
                | SwapAccrue
                | SwapInSettle String
                | SwapOutSettle String
                | PurchaseAsset String Balance
                | IssuanceProceeds String
                | TxnDirection BookDirection
                | TxnComments [TxnComment]
                deriving (Eq, Show, Ord ,Read, Generic)

type FeeDue = Balance
type FeeArrears = Balance

-- ^ transaction record in each entity
data Txn = BondTxn Date Balance Interest Principal IRate Cash DueInt DueIoI (Maybe Float) TxnComment     -- ^ bond transaction record for interest and principal 
         | AccTxn Date Balance Amount TxnComment                                                         -- ^ account transaction record 
         | ExpTxn Date FeeDue Amount FeeArrears TxnComment                                                 -- ^ expense transaction record
         | SupportTxn Date SupportAvailType Balance DueInt DuePremium Cash TxnComment                     -- ^ liquidity provider transaction record
         | IrsTxn Date Balance Amount IRate IRate Balance TxnComment                                     -- ^ interest swap transaction record
         | EntryTxn Date Balance Amount TxnComment                                                       -- ^ ledger book entry
         | TrgTxn Date Bool TxnComment
         deriving (Show, Generic, Eq, Read)


data DealStatFields = PoolCollectedPeriod
                    | BondPaidPeriod
                    deriving (Generic, Eq, Ord, Show, Read)

-- ^ different types of deal stats
data DealStats = CurrentBondBalance
               | CurrentPoolBalance (Maybe [PoolId])
               | CurrentPoolBegBalance (Maybe [PoolId])
               | CurrentPoolDefaultedBalance
               | CumulativePoolDefaultedBalance (Maybe [PoolId])  -- ^ Depreciated, use PoolCumCollection
               | CumulativePoolRecoveriesBalance (Maybe [PoolId]) -- ^ Depreciated, use PoolCumCollection
               | CumulativeNetLoss (Maybe [PoolId])
               | OriginalBondBalance
               | OriginalBondBalanceOf [BondName]
               | BondTotalFunding [BondName]
               | OriginalPoolBalance (Maybe [PoolId])
               | DealIssuanceBalance (Maybe [PoolId])
               | UseCustomData String
               | PoolCumCollection [PoolSource] (Maybe [PoolId])
               | PoolCumCollectionTill Int [PoolSource] (Maybe [PoolId])
               | PoolCurCollection [PoolSource] (Maybe [PoolId])
               | PoolCollectionStats Int [PoolSource] (Maybe [PoolId])
               | PoolWaSpread (Maybe [PoolId])
               | AllAccBalance
               | AccBalance [AccName]
               | LedgerBalance [String]
               | LedgerBalanceBy BookDirection [String]
               | LedgerTxnAmt [String] (Maybe TxnComment)
               | ReserveBalance [AccName] 
               | ReserveGap [AccName]
               | ReserveExcess [AccName] 
               | ReserveGapAt Date [AccName] 
               | ReserveExcessAt Date [AccName] 
               | FutureCurrentPoolBalance (Maybe [PoolId])
               | FutureCurrentSchedulePoolBalance (Maybe [PoolId])
               | FutureCurrentSchedulePoolBegBalance (Maybe [PoolId])
               | PoolScheduleCfPv PricingMethod (Maybe [PoolId])
               | FuturePoolScheduleCfPv Date PricingMethod (Maybe [PoolId])
               | FutureWaCurrentPoolBalance Date Date (Maybe [PoolId])
               | FutureCurrentPoolBegBalance (Maybe [PoolId])
               | FutureCurrentBondBalance Date
               | CurrentBondBalanceOf [BondName]
               | BondIntPaidAt Date BondName
               | BondsIntPaidAt Date [BondName]
               | BondPrinPaidAt Date BondName
               | BondsPrinPaidAt Date [BondName]
               | BondBalanceTarget [BondName]
               | BondBalanceGap BondName
               | BondBalanceGapAt Date BondName
               | BondDuePrin [BondName]
               | BondReturn BondName Balance [TsPoint Amount]
               | FeePaidAmt [FeeName]
               | FeeTxnAmt [FeeName] (Maybe TxnComment)
               | BondTxnAmt [BondName] (Maybe TxnComment)
               | AccTxnAmt  [AccName] (Maybe TxnComment)
               | FeeTxnAmtBy Date [FeeName] (Maybe TxnComment)
               | BondTxnAmtBy Date [BondName] (Maybe TxnComment)
               | AccTxnAmtBy Date [AccName] (Maybe TxnComment)
               | FeesPaidAt Date [FeeName] 
               | CurrentDueBondInt [BondName]
               | CurrentDueBondIntAt Int [BondName]
               | CurrentDueBondIntOverInt [BondName]
               | CurrentDueBondIntOverIntAt Int [BondName]
               | CurrentDueBondIntTotal [BondName]
               | CurrentDueBondIntTotalAt Int [BondName]
               | CurrentDueFee [FeeName]
               | LastBondIntPaid [BondName]
               | LastBondPrinPaid [BondName]
               | LastFeePaid [FeeName]
               | LiqCredit [String]
               | LiqBalance [String]
               | RateCapNet String
               | RateSwapNet String
               | BondBalanceHistory Date Date
               | PoolCollectionHistory PoolSource Date Date (Maybe [PoolId])
               | UnderlyingBondBalance (Maybe [BondName])
               | WeightedAvgCurrentPoolBalance Date Date (Maybe [PoolId])
               | WeightedAvgCurrentBondBalance Date Date [BondName]
               | WeightedAvgOriginalPoolBalance Date Date (Maybe [PoolId])
               | WeightedAvgOriginalBondBalance Date Date [BondName]
               | CustomData String Date
               | DealStatBalance DealStatFields
               -- analytical query
               | AmountRequiredForTargetIRR Double BondName 
               -- integer type
               | CurrentPoolBorrowerNum (Maybe [PoolId])
               | FutureCurrentPoolBorrowerNum Date (Maybe [PoolId])
               | ProjCollectPeriodNum
               | MonthsTillMaturity BondName
               | DealStatInt DealStatFields
               -- boolean type
               | TestRate DealStats Cmp Micro
               | TestAny Bool [DealStats]
               | TestAll Bool [DealStats]
               | TestNot DealStats
               | IsDealStatus DealStatus
               | IsMostSenior BondName [BondName]
               | IsPaidOff [BondName]
               | IsFeePaidOff [String]
               | IsLiqSupportPaidOff [String]
               | IsRateSwapPaidOff [String]
               | IsOutstanding [BondName]
               | HasPassedMaturity [BondName]
               | TriggersStatus DealCycle String
               | DealStatBool DealStatFields
               -- rate type
               | PoolWaRate (Maybe PoolId)
               | BondRate BondName
               | CumulativeNetLossRatio (Maybe [PoolId])
               | FutureCurrentBondFactor Date
               | FutureCurrentPoolFactor Date (Maybe [PoolId])
               | BondFactor
               | BondFactorOf BondName
               | CumulativePoolDefaultedRate (Maybe [PoolId])
               | CumulativePoolDefaultedRateTill Int (Maybe [PoolId])
               | PoolFactor (Maybe [PoolId])
               | BondWaRate [BondName]
               | DealStatRate DealStatFields
               -- Compond type
               | Factor DealStats Rational
               | Multiply [DealStats]
               | Max [DealStats]
               | Min [DealStats]
               | Sum [DealStats]
               | Substract [DealStats]
               | Subtract [DealStats]
               | Excess [DealStats]
               | Avg [DealStats]
               | AvgRatio [DealStats]
               | Divide DealStats DealStats
               | DivideRatio DealStats DealStats
               | Constant Rational
               | FloorAndCap DealStats DealStats DealStats
               | FloorWith DealStats DealStats
               | FloorWithZero DealStats
               | CapWith DealStats DealStats
               | Abs DealStats
               | Round DealStats (RoundingBy Rational)
               deriving (Show,Eq,Ord,Read,Generic)


data EvalExpr a where
     EvalSum :: (Num a, Read a, Generic a, Ord a, Eq a, Show a) => [EvalExpr a] -> EvalExpr a 
     CurrentBondBalance' :: Balance -> EvalExpr Balance
     EvalSubtract :: (Num a) => [EvalExpr a] -> EvalExpr a
     -- EvalAvg :: (Num a) => [EvalExpr a] -> EvalExpr a
     -- EvalMax :: (Num a) => [EvalExpr a] -> EvalExpr a
     -- EvalMin :: (Num a) => [EvalExpr a] -> EvalExpr a
     -- EvalMultiply :: (Num a) => [EvalExpr a] -> EvalExpr a
     -- EvalDivide :: (Num a, Fractional a) => EvalExpr a -> EvalExpr a -> EvalExpr a
     -- EvalFloorAndCap :: (Num a) => EvalExpr a -> EvalExpr a -> EvalExpr a -> EvalExpr a
     -- EvalFloorWith :: (Num a) => EvalExpr a -> EvalExpr a -> EvalExpr a
     -- EvalFloorWithZero :: (Num a) => EvalExpr a -> EvalExpr a
     -- EvalExcess :: (Num a) => EvalExpr a -> [EvalExpr a] -> EvalExpr a
     -- EvalCapWith :: (Num a) => EvalExpr a -> EvalExpr a -> EvalExpr a
     -- EvalAbs :: (Num a) => EvalExpr a -> EvalExpr a
     -- EvalRound :: (Num a) => Int -> EvalExpr a -> EvalExpr a
     -- EvalConstant :: (Num a) => EvalExpr a
     -- EvalBalance :: Balance -> EvalExpr Balance
     -- EvalRate :: Rate -> EvalExpr Rate
     -- EvalInt :: Int -> EvalExpr Int
     -- EvalBool :: Bool -> EvalExpr Bool
     -- deriving (Show,Eq,Ord,Read,Generic)

-- deriving instance Read a => Read (EvalExpr a)
-- deriving instance Generic a => Generic (EvalExpr a)

-- instance Read (EvalExpr a) where
--   readsPrec _ "EvalSum" = [(EvalSum [],"")]
--   readsPrec _ "CurrentBondBalance'" = [(CurrentBondBalance', "")]
--   readsPrec _ _ = error "EvalExpr: not implemented for other constructors"
     
preHasTrigger :: Pre -> [(DealCycle,String)]
preHasTrigger (IfBool (TriggersStatus dc tName) _) = [(dc,tName)]
preHasTrigger (Any ps) = concatMap preHasTrigger ps
preHasTrigger (All ps) = concatMap preHasTrigger ps
preHasTrigger _ = []


data Limit = DuePct Rate            -- ^ up to % of total amount due
           | DueCapAmt Balance      -- ^ up to $ amount 
           | KeepBalAmt DealStats   -- ^ pay till a certain amount remains in an account
           | DS DealStats           -- ^ transfer with limit described by a `DealStats`
           | RemainBalPct Rate      -- ^ pay till remain balance equals to a percentage of `stats`
           | TillTarget             -- ^ transfer amount which make target account up reach reserve balanace
           | TillSource             -- ^ transfer amount out till source account down back to reserve balance
           | Multiple Limit Float   -- ^ factor of a limit
           deriving (Show,Ord,Eq,Read,Generic)

data HowToPay = ByProRata
              | BySequential
              deriving (Show,Ord,Eq,Read, Generic)

type BookItems = [BookItem]

data BookItem = Item String Balance 
              | ParentItem String BookItems
              deriving (Show,Read,Generic,Eq)

data BalanceSheetReport = BalanceSheetReport {
                            asset :: BookItem
                            ,liability :: BookItem
                            ,equity :: BookItem
                            ,reportDate :: Date}         -- ^ snapshot date of the balance sheet
                            deriving (Show,Read,Generic,Eq)

data CashflowReport = CashflowReport {
                        inflow :: BookItem
                        ,outflow :: BookItem
                        ,net ::  BookItem
                        ,startDate :: Date 
                        ,endDate :: Date }
                        deriving (Show,Read,Generic,Eq)


data Threshold = Below
               | EqBelow
               | Above
               | EqAbove
               deriving (Show,Eq,Ord,Read,Generic)

data SplitType = EqToLeft   -- if equal, the element belongs to left
               | EqToRight  -- if equal, the element belongs to right
               | EqToLeftKeepOne
               | EqToLeftKeepOnes
               deriving (Show, Eq, Generic)

-- ^ deal level cumulative statistics
data CutoffFields = IssuanceBalance              -- ^ pool issuance balance
                  | HistoryRecoveries            -- ^ cumulative recoveries
                  | HistoryInterest              -- ^ cumulative interest collected
                  | HistoryPrepayment            -- ^ cumulative prepayment collected
                  | HistoryPrepaymentPentalty    -- ^ cumulative prepayment collected
                  | HistoryPrincipal             -- ^ cumulative principal collected
                  | HistoryRental                -- ^ cumulative rental collected
                  | HistoryDefaults              -- ^ cumulative default balance
                  | HistoryDelinquency           -- ^ cumulative delinquency balance
                  | HistoryLoss                  -- ^ cumulative loss/write-off balance
                  | HistoryCash                  -- ^ cumulative cash
                  | HistoryFeePaid
                  | AccruedInterest              -- ^ accrued interest at closing
                  | RuntimeCurrentPoolBalance    -- ^ current pool balance
                  deriving (Show,Ord,Eq,Read,Generic,NFData)


data PriceResult = PriceResult Valuation PerFace WAL Duration Convexity AccruedInterest [Txn]
                 | AssetPrice Valuation WAL Duration Convexity AccruedInterest
                 | OASResult PriceResult [Valuation] Spread  
                 | ZSpread Spread 
                 | IrrResult IRR [Txn]
                 deriving (Show, Eq, Generic)

makePrisms ''PriceResult

getPriceValue :: PriceResult -> Balance
getPriceValue (AssetPrice v _ _ _ _ ) = v
getPriceValue (PriceResult v _ _ _ _ _ _) = v
getPriceValue x = error  $ "failed to match with type when geting price value" ++ show x


-- getValuation :: PriceResult -> PerFace
-- getValuation (PriceResult _ val _ _ _ _ _) = val
-- getValuation (OASResult pr _ _) = getValuation pr
-- getValuation pr =  error $ "not support for pricing result"++ show pr


class Liable lb where 

  -- must implement
  isPaidOff :: lb -> Bool
  getCurBalance :: lb -> Balance
  getCurRate :: lb -> IRate
  getOriginBalance :: lb -> Balance
  getOriginDate :: lb -> Date
  getAccrueBegDate :: lb -> Date
  getDueInt :: lb -> Balance
  getDueIntAt :: lb -> Int -> Balance
  getDueIntOverInt :: lb -> Balance
  getDueIntOverIntAt :: lb -> Int -> Balance
  getTotalDueInt :: lb -> Balance
  getTotalDueIntAt :: lb -> Int -> Balance
  getOutstandingAmount :: lb -> Balance


data DueType = DueInterest (Maybe Int) -- ^ interest due
             | DuePrincipal            -- ^ principal due
             | DueFee                  -- ^ fee due
             | DueResidual             -- ^ residual 
             | DueArrears              -- ^ something that is not paid in the past
             | DueTotalOf [DueType]    -- ^ a combination of above with sequence
             deriving (Show, Eq, Generic)


class Accruable ac where 
  bookAccrual :: Date -> Balance -> ac -> ac
  getAccrualDates :: Date -> ac -> [Date]
  accrueTo :: Date -> ac -> ac
  -- accrueWithDeal :: Date -> deal -> ac -> ac

class Payable pa where
  pay :: Date -> DueType -> Balance -> pa -> Either ErrorRep pa
  getDueBal :: Date -> Maybe DueType -> pa -> Balance
  writeOff :: Date -> DueType -> Amount -> pa -> Either ErrorRep pa

class RateResettable rs where
  getResetDates :: Date -> rs -> [Dates]
  reset :: Date -> rs -> rs

class Drawable dr where
  draw :: Date -> Balance -> TxnComment -> dr -> Either ErrorRep dr
  availForDraw :: Date -> dr -> SupportAvailType

  -- buildAccrualAction :: ac -> Date -> Date -> [ActionOnDate]

-- class Resettable rs where 
--   reset :: Date -> rs -> rs
--   buildResetAction :: rs -> Date -> Date -> [Txn]

lookupTable :: Ord a => Table a b -> Direction -> (a -> Bool) -> Maybe b
lookupTable (ThresholdTable rows) direction lkUpFunc
  = case findIndex lkUpFunc rs of 
      Nothing -> Nothing
      Just i -> Just $ vs!!i  
    where 
        rs = case direction of 
                Up -> reverse $ map fst rows
                Down -> map fst rows
        vs = case direction of 
                Up -> reverse $ map snd rows
                Down -> map snd rows

lookupIntervalTable :: Ord a => Table a b -> Direction -> (a -> Bool) -> Maybe ((a,b),(a,b))
lookupIntervalTable (ThresholdTable rows) direction lkUpFunc
  = case findIndex lkUpFunc rs of 
      Nothing -> Nothing
      Just i -> if succ i == length rows then 
                  Nothing
                else
                  Just $ (rows!!i, rows!!(i+1)) -- `debug` ("Find index"++ show i)
    where 
        rs = case direction of 
                Up -> reverse $ map fst rows
                Down -> map fst rows


data RateAssumption = RateCurve Index Ts     -- ^ a rate curve ,which value of rates depends on time
                    | RateFlat Index IRate   -- ^ a rate constant
                    deriving (Show, Generic)

data TimeHorizion = ByMonth
                  | ByYear
                  | ByQuarter

instance TimeSeries (TsPoint a) where 
    getDate (TsPoint d a) = d


$(deriveJSON defaultOptions ''DecimalRaw)
$(deriveJSON defaultOptions ''TsPoint)
$(deriveJSON defaultOptions ''PerPoint)
$(deriveJSON defaultOptions ''Ts)
$(deriveJSON defaultOptions ''Cmp)
$(deriveJSON defaultOptions ''PoolSource)
$(deriveJSON defaultOptions ''RoundingBy)
$(deriveJSON defaultOptions ''PoolId)



instance ToJSONKey PoolId where
  toJSONKey :: ToJSONKeyFunction PoolId
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey PoolId where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t++">>"++ show (T.unpack t))

-- ^ different types of waterfall execution
data ActionWhen = EndOfPoolCollection             -- ^ waterfall executed at the end of pool collection
                | DistributionDay DealStatus      -- ^ waterfall executed depends on deal status
                | CleanUp                         -- ^ waterfall exectued upon a clean up call
                | OnClosingDay                    -- ^ waterfall executed on deal closing day
                | DefaultDistribution             -- ^ default waterfall executed
                | RampUp                          -- ^ ramp up
                | WithinTrigger String            -- ^ waterfall executed within a trigger  
                | CustomWaterfall String          -- ^ custom waterfall
                deriving (Show,Ord,Eq,Generic,Read)


data ResultComponent = CallAt Date                                          -- ^ the date when deal called
                     | DealStatusChangeTo Date DealStatus DealStatus String -- ^ record when & why status changed
                     | BondOutstanding String Balance Balance               -- ^ when deal ends,calculate oustanding principal balance 
                     | BondOutstandingInt String Balance Balance            -- ^ when deal ends,calculate oustanding interest due 
                     | InspectBal Date DealStats Balance                    -- ^ A bal value from inspection
                     | InspectInt Date DealStats Int                        -- ^ A int value from inspection
                     | InspectRate Date DealStats Micro                     -- ^ A rate value from inspection
                     | InspectBool Date DealStats Bool                      -- ^ A bool value from inspection
                     | RunningWaterfall Date ActionWhen                     -- ^ running waterfall at a date 
                     | FinancialReport StartDate EndDate BalanceSheetReport CashflowReport
                     | InspectWaterfall Date (Maybe String) [DealStats] [String]
                     | ErrorMsg String
                     | WarningMsg String
                     | EndRun (Maybe Date) String                             -- ^ end of run with a message
                     deriving (Show, Generic,Eq)

makePrisms ''ResultComponent


listToStrWithComma :: [String] -> String
listToStrWithComma = intercalate ","

instance ToJSON TxnComment where 
  toJSON (PayInt bns ) = String $ T.pack $ "<PayInt:"++ listToStrWithComma bns ++ ">"
  toJSON (PayYield bn ) = String $ T.pack $ "<PayYield:"++ bn ++">"
  toJSON (PayPrin bns ) =  String $ T.pack $ "<PayPrin:"++ listToStrWithComma bns ++ ">"
  toJSON (WriteOff bn amt ) =  String $ T.pack $ "<WriteOff:"++ bn ++","++ show amt ++ ">"
  toJSON (FundWith b bal) = String $ T.pack $ "<FundWith:"++b++","++show bal++">"
  toJSON (PayPrinResidual bns ) =  String $ T.pack $ "<PayPrinResidual:"++ listToStrWithComma bns ++ ">"
  toJSON (PayFee fn ) =  String $ T.pack $ "<PayFee:" ++ fn ++ ">"
  toJSON (SeqPayFee fns) =  String $ T.pack $ "<SeqPayFee:"++ listToStrWithComma fns++">"
  toJSON (PayFeeYield fn) =  String $ T.pack $ "<PayFeeYield:"++ fn++">"
  toJSON (Transfer an1 an2) =  String $ T.pack $ "<Transfer:"++ an1 ++","++ an2++">"
  toJSON (TransferBy an1 an2 limit) =  String $ T.pack $ "<TransferBy:"++ an1 ++","++ an2++","++show limit++">"
  toJSON (PoolInflow mPids ps) =  String $ T.pack $ "<Pool"++ maybe "" (intercalate "|" . (show <$>)) mPids ++":"++ show ps++">"
  toJSON (LiquidationProceeds pids) =  String $ T.pack $ "<Liquidation:"++ listToStrWithComma (show <$> pids) ++">"
  toJSON (UsingDS ds) =  String $ T.pack $ "<DS:"++ show ds++">"
  toJSON BankInt =  String $ T.pack $ "<BankInterest:>"
  toJSON Empty =  String $ T.pack $ "" 
  toJSON (LiquidationSupport source) = String $ T.pack $ "<Support:"++source++">"
  toJSON (LiquidationSupportInt b1 b2) =  String $ T.pack $ "<SupportExp:(Int:"++ show b1 ++ ",Fee:" ++ show b2 ++")>"
  toJSON LiquidationDraw = String $ T.pack $ "<Draw:>"
  toJSON (LiquidationRepay s) = String $ T.pack $ "<Repay:"++ s ++">"
  toJSON SwapAccrue = String $ T.pack $ "<Accure:>"
  toJSON (SwapInSettle s)= String $ T.pack $ "<SettleIn:"++ s ++">"
  toJSON (SwapOutSettle s) = String $ T.pack $ "<SettleOut:"++ s ++">"
  toJSON (PurchaseAsset rPoolName bal) = String $ T.pack $ "<PurchaseAsset:"<> rPoolName <>","++show bal++">"
  toJSON (TxnDirection dr) = String $ T.pack $ "<TxnDirection:"++show dr++">"
  toJSON SupportDraw = String $ T.pack $ "<SupportDraw:>"
  toJSON (IssuanceProceeds nb) = String $ T.pack $ "<IssuanceProceeds:"++nb++">"
  toJSON (Tag cmt) = String $ T.pack $ "<Tag:"++cmt++">"
  toJSON (TxnComments tcms) = Array $ V.fromList $ map toJSON tcms
  toJSON (PayGroupInt bns) = String $ T.pack $ "<PayGroupInt:"++ listToStrWithComma bns ++ ">"
  toJSON (PayGroupPrin bns) = String $ T.pack $ "<PayGroupPrin:"++ listToStrWithComma bns ++ ">"
  toJSON (BookLedgerBy dr lName) = String $ T.pack $ "<BookLedger:"++ lName ++ ">"
  toJSON x = error $ "Not support for toJSON for "++show x

instance FromJSON TxnComment where
    parseJSON = withText "Empty" parseTxn

parseTxn :: T.Text -> Parser TxnComment 
parseTxn "" = return Empty 
parseTxn "<BankInt>" = return BankInt
parseTxn t = case tagName of 
  "Transfer" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ Transfer (T.unpack (head sv)) (T.unpack (sv!!1))
  "Support" -> return $ LiquidationSupport contents
  "PayInt" -> return $ PayInt [contents]
  "PayYield" -> return $ PayYield contents
  "PayPrin" -> return $ PayPrin [contents]
  "WriteOff" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ WriteOff (T.unpack (head sv)) (read (T.unpack (sv!!1))::Balance)
  "PayPrinResidual" -> return $ PayPrinResidual [contents]
  "PayFee" -> return $ PayFee contents
  "SeqPayFee" -> return $ SeqPayFee [contents]
  "PayFeeYield" -> return $ PayFeeYield contents
  "TransferBy" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ TransferBy (T.unpack (head sv)) (T.unpack (sv!!1)) (read (T.unpack (sv!!2))::Limit)
  "Pool" -> let 
              sr = T.splitOn (T.pack ":") $ T.pack contents
              mPids = if head sr == "Nothing" then 
                        Nothing 
                      else 
                        Just (read <$> T.unpack <$> sr)::(Maybe [PoolId])
            in 
              return $ PoolInflow mPids (read (T.unpack (sr!!1))::PoolSource)
  "Liquidation" -> let 
                      sv = T.splitOn (T.pack ",") $ T.pack contents
                      pids::[PoolId] = read <$> T.unpack <$> sv
                    in
                      return $ LiquidationProceeds pids

  "DS" -> return $ UsingDS (read (contents)::DealStats)
  "LiquidationSupportExp" -> let 
                              sv = T.splitOn (T.pack ",") $ T.pack contents
                            in 
                              return $ LiquidationSupportInt (read (T.unpack (head sv))::Balance) (read (T.unpack (sv!!1))::Balance)
  "SupportDraw" -> return SupportDraw
  "Draw" -> return LiquidationDraw
  "Repay" -> return $ LiquidationRepay contents
  "Accure" -> return SwapAccrue
  "SettleIn" -> return $ SwapInSettle contents
  "SettleOut" -> return $ SwapOutSettle contents
  "PurchaseAsset" -> let 
                      sv = T.splitOn (T.pack ",") $ T.pack contents
                     in 
                      return $ PurchaseAsset (read (T.unpack (sv!!0))::String)  (read (T.unpack (sv!!1))::Balance)

  "TxnDirection" -> return $ TxnDirection (read contents::BookDirection)
  "FundWith" -> let 
                  sv = T.splitOn (T.pack ",") $ T.pack contents
                in 
                  return $ FundWith (T.unpack (head sv)) (read (T.unpack (sv!!1))::Balance)
--   toJSON (IssuanceProceeds nb) = String $ T.pack $ "<IssuanceProceeds:"++nb++">"
  "IssuanceProceeds" -> return $ IssuanceProceeds contents                  
  "Tag" -> return $ Tag contents                  
  where 
      pat = "<(\\S+):(\\S+)>"::String
      sr = (T.unpack t =~ pat)::[[String]]
      tagName =  head sr!!1::String
      contents = head sr!!2::String


mapKVTraversal :: Traversal' (Map.Map k a) (k, a)
mapKVTraversal = itraversed . withIndex

data DealStatType = RtnBalance 
                  | RtnRate 
                  | RtnBool 
                  | RtnInt
                  deriving (Show,Eq,Ord,Read,Generic)

type DealStatRtn :: DealStats -> DealStatType
type family DealStatRtn ds where
  DealStatRtn (CumulativePoolDefaultedRateTill _ _) = RtnRate
  DealStatRtn (CumulativePoolDefaultedRate _) = RtnRate
  DealStatRtn (CumulativeNetLossRatio _) = RtnRate
  DealStatRtn BondFactor = RtnRate
  DealStatRtn (BondFactorOf _) = RtnRate
  DealStatRtn (PoolFactor _) = RtnRate
  DealStatRtn (FutureCurrentBondFactor _) = RtnRate
  DealStatRtn (FutureCurrentPoolFactor _ _) = RtnRate
  DealStatRtn (BondWaRate _) = RtnRate
  DealStatRtn (PoolWaRate _) = RtnRate
  DealStatRtn (BondRate _) = RtnRate
  DealStatRtn (DivideRatio _ _) = RtnRate
  DealStatRtn (AvgRatio _) = RtnRate
  DealStatRtn (DealStatRate _) = RtnRate
  DealStatRtn (Avg dss) = RtnRate
  DealStatRtn (Divide ds1 ds2) = RtnRate
  DealStatRtn (Multiply _) = RtnRate
  DealStatRtn (Factor _ _) = RtnRate
  DealStatRtn (PoolWaSpread _) = RtnRate

  DealStatRtn (IsMostSenior _ _) = RtnBool
  DealStatRtn (IsPaidOff _) = RtnBool
  DealStatRtn (IsOutstanding _) = RtnBool
  DealStatRtn (HasPassedMaturity _) = RtnBool
  DealStatRtn (TriggersStatus _ _)= RtnBool
  DealStatRtn (IsDealStatus _)= RtnBool
  DealStatRtn (TestRate _ _ _) = RtnBool
  DealStatRtn (TestAny _ _) = RtnBool
  DealStatRtn (TestAll _ _) = RtnBool
  DealStatRtn (DealStatBool _) = RtnBool
  
  DealStatRtn  (Max (s:dss)) = DealStatRtn s
  DealStatRtn (Min (s:dss)) = DealStatRtn s
  DealStatRtn _ = RtnBalance

class Queryable ds where
  getDealStatType' :: DealStats -> DealStatType

getDealStatType :: DealStats -> DealStatType
getDealStatType (CumulativePoolDefaultedRateTill _ _) = RtnRate
getDealStatType (CumulativePoolDefaultedRate _) = RtnRate
getDealStatType (CumulativeNetLossRatio _) = RtnRate
getDealStatType BondFactor = RtnRate
getDealStatType (BondFactorOf _) = RtnRate
getDealStatType (PoolFactor _) = RtnRate
getDealStatType (FutureCurrentBondFactor _) = RtnRate
getDealStatType (FutureCurrentPoolFactor _ _) = RtnRate
getDealStatType (BondWaRate _) = RtnRate
getDealStatType (PoolWaRate _) = RtnRate
getDealStatType (BondRate _) = RtnRate
getDealStatType DivideRatio {} = RtnRate
getDealStatType AvgRatio {} = RtnRate
getDealStatType (DealStatRate _) = RtnRate
getDealStatType (Avg dss) = RtnRate
getDealStatType (Divide ds1 ds2) = RtnRate
getDealStatType (Multiply _) = RtnRate
getDealStatType (Factor _ _) = RtnRate
getDealStatType (PoolWaSpread _) = RtnRate

getDealStatType (CurrentPoolBorrowerNum _) = RtnInt
getDealStatType (MonthsTillMaturity _) = RtnInt
getDealStatType ProjCollectPeriodNum = RtnInt
getDealStatType (DealStatInt _) = RtnInt

getDealStatType (IsMostSenior _ _) = RtnBool
getDealStatType IsPaidOff {} = RtnBool
getDealStatType IsOutstanding {} = RtnBool
getDealStatType HasPassedMaturity {} = RtnBool
getDealStatType (TriggersStatus _ _)= RtnBool
getDealStatType (IsDealStatus _)= RtnBool
getDealStatType TestRate {} = RtnBool
getDealStatType (TestAny _ _) = RtnBool
getDealStatType (TestAll _ _) = RtnBool
getDealStatType (DealStatBool _) = RtnBool

getDealStatType (Max dss) = getDealStatType (head dss)
getDealStatType (Min dss) = getDealStatType (head dss)
getDealStatType _ = RtnBalance

dealStatType _ = RtnBalance

data CustomDataType = CustomConstant Rational 
                    | CustomCurve    Ts 
                    | CustomDS       DealStats
                    deriving (Show,Ord,Eq,Read,Generic)

opts :: JSONKeyOptions
opts = defaultJSONKeyOptions -- { keyModifier = toLower }


$(deriveJSON defaultOptions ''BondPricingMethod)
$(deriveJSON defaultOptions ''DealStatus)
$(deriveJSON defaultOptions ''CutoffType)
$(deriveJSON defaultOptions ''DealStatFields)
$(concat <$> traverse (deriveJSON defaultOptions) [''BookDirection, ''DealStats, ''PricingMethod, ''DealCycle, ''DateType, ''Period, 
  ''DatePattern, ''Table, ''BalanceSheetReport, ''BookItem, ''CashflowReport, ''Txn] )

instance ToJSONKey DateType where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey DateType where
  fromJSONKey = FromJSONKeyTextParser $ \t -> 
    case T.splitOn " " t of
      ["CustomExeDates", rest] -> pure $ CustomExeDates (T.unpack rest)
      _ -> case readMaybe (T.unpack t) of
        Just k -> pure k
        Nothing -> fail ("Invalid key (DateType): " ++ show t++">>"++ show (T.unpack t))



$(deriveJSON defaultOptions ''RangeType)
$(deriveJSON defaultOptions ''PerCurve)
$(deriveJSON defaultOptions ''Pre)
$(deriveJSON defaultOptions ''CustomDataType)
$(deriveJSON defaultOptions ''ActionWhen)

instance ToJSONKey ActionWhen where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey ActionWhen where
  fromJSONKey = FromJSONKeyTextParser $ \t -> 
    case T.splitOn " " t of
      ["CustomWaterfall", rest] -> pure $ CustomWaterfall (T.unpack rest)
      _ -> case readMaybe (T.unpack t) of
        Just k -> pure k
        Nothing -> fail ("Invalid key (Action When): " ++ show t++">>"++ show (T.unpack t))


$(deriveJSON defaultOptions ''ResultComponent)
$(deriveJSON defaultOptions ''PriceResult)
$(deriveJSON defaultOptions ''CutoffFields)
$(deriveJSON defaultOptions ''HowToPay)



instance ToJSONKey DealCycle where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey DealCycle where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


instance ToJSONKey CutoffFields where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSONKey CutoffFields where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (Text.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)


newtype MyRatio = MyRatio (Ratio Integer)

instance ToJSON MyRatio where
  toJSON (MyRatio r) = case fromRationalRepetend Nothing r of
      Left (sci, _)         -> toJSON $ formatScientific Fixed (Just 8) sci
      Right (sci, rep) -> toJSON $ formatScientific Fixed (Just 8) sci

instance Show MyRatio where
  show (MyRatio r) = case fromRationalRepetend Nothing r of
      Left (sci, _)         -> show $ formatScientific Fixed (Just 8) sci
      Right (sci, rep) -> show $ formatScientific Fixed (Just 8) sci

$(deriveJSON defaultOptions ''Index)
$(deriveJSON defaultOptions ''DayCount)
$(deriveJSON defaultOptions ''Threshold)
instance ToJSONKey Threshold where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey Threshold where
  fromJSONKey = genericFromJSONKey opts


$(deriveJSON defaultOptions ''RateAssumption)
$(deriveJSON defaultOptions ''Direction)
-- $(deriveJSON defaultOptions ''(EvalExpr Balance))

-- $(deriveJSONGADT ''EvalExpr)
makePrisms ''Txn
$(concat <$> traverse (deriveJSON defaultOptions) [''Limit] )
