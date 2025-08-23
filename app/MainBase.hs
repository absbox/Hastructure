{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module MainBase(DealType(..),RunResp,PoolTypeWrap(..),RunPoolTypeRtn,RunPoolTypeRtn_
                ,RunAssetReq(..),RunAssetResp,ScenarioName,DealRunInput,RunDealReq(..),RunSimDealReq(..),RunPoolReq(..)
                ,RunDateReq(..),Version(..),RunRespRight
                ,RootFindReq(..),RootFindResp(..),TargetBonds,PoolRunResp,RootFindTweak(..),RootFindStop(..)
                )

where

import Prelude ()
import Prelude.Compat
import System.Environment
import Control.Monad.IO.Class    (liftIO)
import Control.Monad (mapM)
import Control.Exception (Exception,throwIO,throw)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
-- import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import qualified Data.DList as DL
import Data.Map
import Data.Proxy
import qualified Data.Text as T
import Data.Maybe
import Data.Yaml as Y
import Data.OpenApi hiding (Server,contentType)
import qualified Data.Map as Map
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import GHC.Real
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
-- import qualified Data.Aeson.Parser
import Language.Haskell.TH
-- import Network.HTTP.Types.Status
import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)

import Types
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Deal.DealCollection as DC
import qualified Deal.DealQuery as Q
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Ledger as LD
import qualified AssetClass.Installment 
import qualified AssetClass.Mortgage 
import qualified AssetClass.Loan 
import qualified AssetClass.Lease 
import qualified AssetClass.ProjectedCashFlow
import qualified AssetClass.MixedAsset as MA
import qualified AssetClass.AssetBase as AB 
import qualified Assumptions as AP
import qualified Cashflow as CF
import qualified Accounts as A
import qualified Revolving 
import qualified Liability as L
import qualified Call as C
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified InterestRate as IR
import qualified Stmt
import qualified Triggers as TRG
import qualified Revolving as RV
import qualified Lib
import qualified Util as U
import qualified DateUtil as DU
import Data.Scientific (fromRationalRepetend,formatScientific, Scientific,FPFormat(Fixed))
import Control.Lens
import qualified Types as W
import Cashflow (patchCumulative)

data DealType = MDeal (DB.TestDeal AB.Mortgage)
              | LDeal (DB.TestDeal AB.Loan)
              | IDeal (DB.TestDeal AB.Installment) 
              | RDeal (DB.TestDeal AB.Lease) 
              | FDeal (DB.TestDeal AB.FixedAsset) 
              | VDeal (DB.TestDeal AB.Receivable)
              | PDeal (DB.TestDeal AB.ProjectedCashFlow) 
              | UDeal (DB.TestDeal AB.AssetUnion)
              deriving(Show, Generic)

makePrisms ''DealType

data PoolTypeWrap = LPool (DB.PoolType AB.Loan)
                  | IPool (DB.PoolType AB.Installment)
                  | MPool (DB.PoolType AB.Mortgage)
                  | RPool (DB.PoolType AB.Lease)
                  | FPool (DB.PoolType AB.FixedAsset)
                  | VPool (DB.PoolType AB.Receivable)
                  | PPool (DB.PoolType AB.ProjectedCashFlow)
                  | UPool (DB.PoolType AB.AssetUnion)
                  deriving(Show, Generic)


type RunPoolTypeRtn_ = Map.Map PoolId CF.PoolCashflow
type RunPoolTypeRtn = Either String RunPoolTypeRtn_



data RunAssetReq = RunAssetReq Date [AB.AssetUnion] (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption]) (Maybe PricingMethod)
                   deriving(Show, Generic)

type RunAssetResp = Either String (CF.AssetCashflow, Maybe [PriceResult])

type ScenarioName = String
type DealRunInput = (DealType, Maybe AP.ApplyAssumptionType, AP.NonPerfAssumption, [D.ExpectReturn])
data RunDealReq = SingleRunReq [D.ExpectReturn] DealType (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiScenarioRunReq [D.ExpectReturn] DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption --- multi pool perf
                | MultiDealRunReq [D.ExpectReturn] (Map.Map ScenarioName DealType) (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption  -- multi deal struct
                | MultiRunAssumpReq [D.ExpectReturn] DealType (Maybe AP.ApplyAssumptionType) (Map.Map ScenarioName AP.NonPerfAssumption) -- multi run assump 
                | MultiComboReq [D.ExpectReturn] (Map.Map ScenarioName DealType)  (Map.Map ScenarioName (Maybe AP.ApplyAssumptionType))  (Map.Map ScenarioName AP.NonPerfAssumption)
                deriving(Show, Generic)

data RunSimDealReq = OASReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                    deriving(Show, Generic)


type RunRespRight = (DealType , Map.Map PoolId CF.CashFlowFrame, [ResultComponent],Map.Map String PriceResult, Map.Map PoolId CF.PoolCashflow)
type RunResp = Either String RunRespRight

data RunPoolReq = SingleRunPoolReq Bool PoolTypeWrap (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption])
                | MultiScenarioRunPoolReq Bool PoolTypeWrap (Map.Map ScenarioName AP.ApplyAssumptionType) (Maybe [RateAssumption])
                deriving(Show, Generic)


data RunDateReq = RunDateReq Date DatePattern (Maybe Date)
                deriving(Show, Generic)
instance ToSchema RunDateReq

type PoolRunResp = Either String (Map.Map PoolId CF.PoolCashflow)


type TargetBonds = [BondName]
-- calcualte best spread that
--- 1. make sure all bonds are paid off
--- 2. make sure WAC cap is met
data RootFindReq = FirstLossReq DealRunInput BondName
                 | MaxSpreadToFaceReq DealRunInput BondName Bool Bool
                 | RootFinderReq DealRunInput RootFindTweak RootFindStop
                 deriving(Show, Generic)

type RangeInput = (Double, Double) -- (min, max)

data RootFindTweak = StressPoolDefault RangeInput                      -- stressed pool perf 
                   | StressPoolPrepayment RangeInput                   -- stressed pool prepayment
                   | MaxSpreadTo BondName RangeInput                   -- bond component
                   | SplitFixedBalance BondName BondName RangeInput    -- bond component
                   deriving(Show, Generic)

data RootFindStop = BondIncurLoss BondName
                  | BondIncurPrinLoss BondName Balance
                  | BondIncurIntLoss BondName Balance
                  | BondPricingEqOriginBal BondName Bool Bool
                  | BondMetTargetIrr BondName IRR
                  | BalanceFormula DealStats Balance
                  deriving(Show, Generic)

data RootFindResp = RFResult Double DealRunInput
                  -- | BestSpreadResult Double (Map.Map BondName L.Bond) DealType
                  -- | FirstLossResult Double AP.ApplyAssumptionType (Maybe AP.RevolvingAssumption)
                  deriving(Show, Generic)

$(deriveJSON defaultOptions ''RootFindTweak)
$(deriveJSON defaultOptions ''RootFindStop)

instance ToSchema D.ExpectReturn
instance ToSchema RootFindReq
instance ToSchema RootFindTweak
instance ToSchema RootFindStop
instance ToSchema CF.CashFlowFrame
instance ToSchema AB.Loan
instance ToSchema AB.Installment
instance ToSchema AB.AccrualPeriod
instance ToSchema AB.LeaseStepUp
instance ToSchema AB.Lease
instance ToSchema AB.FixedAsset
instance ToSchema AB.Receivable
instance ToSchema AB.ProjectedCashFlow
instance ToSchema CutoffFields
instance ToSchema (P.Pool AB.Mortgage)
instance ToSchema (P.Pool AB.Loan)
instance ToSchema (P.Pool AB.Installment)
instance ToSchema (P.Pool AB.Lease)
instance ToSchema (P.Pool AB.FixedAsset)
instance ToSchema (P.Pool AB.Receivable)
instance ToSchema (P.Pool AB.AssetUnion)
instance ToSchema (P.Pool AB.ProjectedCashFlow)
instance ToSchema AB.AssetUnion
instance ToSchema PoolId
instance ToSchema DealStatus
instance ToSchema DateType
instance ToSchema DB.DateDesp
instance ToSchema DB.ActionOnDate
instance ToSchema DealStats
instance ToSchema Cmp
instance ToSchema PricingMethod
instance ToSchema Stmt.TxnComment
instance ToSchema BookDirection
instance ToSchema Limit
instance ToSchema PoolSource
instance ToSchema (RoundingBy Rate)
instance ToSchema (RoundingBy Integer)
instance ToSchema (RoundingBy Balance)
instance ToSchema DealCycle
instance ToSchema (Table Balance Balance)
instance ToSchema (Table Float Spread)
instance ToSchema A.Account
instance ToSchema A.InterestInfo
instance ToSchema F.Fee
instance ToSchema F.FeeType
instance ToSchema HE.RateCap
instance ToSchema LD.Ledger
instance ToSchema A.ReserveAmount
instance ToSchema L.Bond
instance ToSchema L.StepUp
instance ToSchema L.BondType
instance ToSchema L.OriginalInfo
instance ToSchema L.InterestInfo
instance ToSchema L.InterestOverInterestType
instance ToSchema (PerPoint (Ratio Integer))
instance ToSchema (PerCurve Rate)
instance ToSchema Pre
instance ToSchema W.PayOrderBy
instance ToSchema W.ActionWhen
instance ToSchema W.ExtraSupport
instance ToSchema W.Action
instance ToSchema W.BookType
instance ToSchema DC.CollectionRule
instance ToSchema C.CallOption
instance ToSchema CE.LiqCreditCalc
instance ToSchema CE.LiqFacility
instance ToSchema HE.RateSwap
instance ToSchema HE.RateSwapType
instance ToSchema HE.RateSwapBase
instance ToSchema HE.CurrencySwap
instance ToSchema CE.LiqSupportType
instance ToSchema CE.LiqRepayType
instance ToSchema CE.LiqDrawType
instance ToSchema CustomDataType
instance ToSchema TRG.Trigger
instance ToSchema TRG.TriggerEffect
instance ToSchema Types.BalanceSheetReport
instance ToSchema Types.CashflowReport
instance ToSchema Types.BookItem
-- instance ToSchema a => ToSchema (DL.DList a)
instance ToSchema Types.Txn

-- instance ToSchema (DL.DList Types.Txn) where
--   declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [Types.Txn])

-- instance ToSchema (Generic (DL.DList Types.Txn)) 
-- instance ToSchema (DL.DList Types.Txn)
instance ToSchema a => ToSchema (DL.DList a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])

instance ToSchema Stmt.Statement
instance ToSchema AB.AssociateExp
instance ToSchema AB.AssociateIncome
instance ToSchema RV.RevolvingPool
instance ToSchema (TsPoint [AB.AssetUnion])
instance ToSchema AP.IssueBondEvent
instance ToSchema (TsPoint AP.IssueBondEvent)
instance ToSchema (TsPoint AP.RefiEvent)
instance ToSchema AP.RefiEvent
instance ToSchema AP.InspectType
instance ToSchema AP.CallOpt
instance ToSchema AP.StopBy
instance ToSchema AP.NonPerfAssumption
instance ToSchema BondPricingMethod
instance ToSchema AP.TradeType
instance ToSchema AP.IrrType
instance ToSchema AP.BondPricingInput
instance ToSchema AP.RevolvingAssumption
instance ToSchema AP.TagMatchRule
instance ToSchema RangeType
instance ToSchema AP.FieldMatchRule
instance ToSchema AP.ObligorStrategy
instance ToSchema AP.ApplyAssumptionType
instance ToSchema AP.LeaseEndType
instance ToSchema AP.LeaseDefaultType
instance ToSchema AP.AssetPerfAssumption
instance ToSchema AP.AssetDelinqPerfAssumption
instance ToSchema AP.AssetDefaultedPerfAssumption
instance ToSchema AP.AssetDefaultAssumption
instance ToSchema AP.AssetPrepayAssumption
instance ToSchema AP.RecoveryAssumption
instance ToSchema RateAssumption
instance ToSchema AP.ExtraStress
instance ToSchema AP.AssetDelinquencyAssumption
instance ToSchema AP.LeaseAssetGapAssump
instance ToSchema AP.LeaseAssetRentAssump
instance ToSchema Threshold
instance ToSchema DB.DealStatFields
instance ToSchema (PerPoint Balance)
instance ToSchema (PerCurve Balance)
instance ToSchema (DB.TestDeal AB.Mortgage)
instance ToSchema (DB.TestDeal AB.Loan)
instance ToSchema (DB.TestDeal AB.Installment)
instance ToSchema (DB.TestDeal AB.Lease)
instance ToSchema (DB.TestDeal AB.Receivable)
instance ToSchema (DB.TestDeal AB.ProjectedCashFlow)
instance ToSchema (DB.TestDeal AB.AssetUnion)
instance ToSchema (DB.TestDeal AB.FixedAsset)
instance ToSchema (DB.PoolType AB.Mortgage)
instance ToSchema (DB.PoolType AB.Loan)
instance ToSchema (DB.PoolType AB.Installment)
instance ToSchema (DB.PoolType AB.Lease)
instance ToSchema (DB.PoolType AB.FixedAsset)
instance ToSchema (DB.PoolType AB.Receivable)
instance ToSchema (DB.PoolType AB.ProjectedCashFlow)
instance ToSchema (DB.PoolType AB.AssetUnion)
instance ToSchema (DB.UnderlyingDeal AB.Mortgage)
instance ToSchema (DB.UnderlyingDeal AB.Loan)
instance ToSchema (DB.UnderlyingDeal AB.Installment)
instance ToSchema (DB.UnderlyingDeal AB.Lease)
instance ToSchema (DB.UnderlyingDeal AB.FixedAsset)
instance ToSchema (DB.UnderlyingDeal AB.Receivable)
instance ToSchema (DB.UnderlyingDeal AB.ProjectedCashFlow)
instance ToSchema (DB.UnderlyingDeal AB.AssetUnion)
instance ToSchema ResultComponent
instance ToSchema PriceResult
instance ToSchema DealType
instance ToSchema SupportAvailType

-- $(concat <$> traverse (deriveJSON defaultOptions) [''DealType,''RootFindResp])
$(deriveJSON defaultOptions ''DealType)
$(deriveJSON defaultOptions ''RootFindResp)
$(deriveJSON defaultOptions ''RootFindReq)
$(concat <$> traverse (deriveJSON defaultOptions) [''RunDealReq, ''RunPoolReq,''RunAssetReq, ''RunDateReq,''PoolTypeWrap])

data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version
instance ToSchema RunDealReq
instance ToSchema PoolTypeWrap
instance ToSchema RunPoolReq
instance ToSchema RunAssetReq
instance ToSchema RootFindResp



