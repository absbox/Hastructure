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
module Main where 

import Prelude ()
import Prelude.Compat
import System.Environment

import Control.Monad.Catch       (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class    (liftIO)
import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
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
import Lucid hiding (type_)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Aeson.Parser
import Language.Haskell.TH

import Network.HTTP.Types.Status
import Control.Exception (throw)

import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)


import Types 
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Ledger as LD
import qualified AssetClass.Installment 
import qualified AssetClass.Mortgage 
import qualified AssetClass.Loan 
import qualified AssetClass.Lease 
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

-- import Servant.Checked.Exceptions (NoThrow, Throws)
-- import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))

import Debug.Trace
import qualified Types as W
import Cashflow (patchCumulative)
debug = flip Debug.Trace.trace

data Version = Version 
  { _version :: String 
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Version)
instance ToSchema Version

version1 :: Version 
version1 = Version "0.27.6"


data DealType = MDeal (DB.TestDeal AB.Mortgage)
              | LDeal (DB.TestDeal AB.Loan)
              | IDeal (DB.TestDeal AB.Installment) 
              | RDeal (DB.TestDeal AB.Lease) 
              | FDeal (DB.TestDeal AB.FixedAsset) 
              | VDeal (DB.TestDeal AB.Receivable) 
              | UDeal (DB.TestDeal AB.AssetUnion) 
              deriving(Show, Generic)

instance ToSchema CF.CashFlowFrame
instance ToSchema AB.Loan
instance ToSchema AB.Installment
instance ToSchema AB.AccrualPeriod
instance ToSchema AB.LeaseStepUp
instance ToSchema AB.Lease
instance ToSchema AB.FixedAsset
instance ToSchema AB.Receivable
instance ToSchema CutoffFields
instance ToSchema (P.Pool AB.Mortgage)
instance ToSchema (P.Pool AB.Loan)
instance ToSchema (P.Pool AB.Installment)
instance ToSchema (P.Pool AB.Lease)
instance ToSchema (P.Pool AB.FixedAsset)
instance ToSchema (P.Pool AB.Receivable)
instance ToSchema (P.Pool AB.AssetUnion)
instance ToSchema AB.AssetUnion
instance ToSchema PoolId
instance ToSchema DealStatus
instance ToSchema DateType
instance ToSchema DateDesp
instance ToSchema ActionOnDate
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
instance ToSchema Pre
instance ToSchema W.ActionWhen
instance ToSchema W.ExtraSupport
instance ToSchema W.Action
instance ToSchema W.BookType
instance ToSchema W.CollectionRule
instance ToSchema C.CallOption
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
instance ToSchema OverrideType
instance ToSchema Types.BalanceSheetReport
instance ToSchema Types.CashflowReport
instance ToSchema Types.BookItem
instance ToSchema Stmt.Statement
instance ToSchema Stmt.Txn
instance ToSchema AB.AssociateExp
instance ToSchema AB.AssociateIncome
instance ToSchema RV.RevolvingPool
instance ToSchema (TsPoint [AB.AssetUnion])
instance ToSchema AP.NonPerfAssumption
instance ToSchema AP.BondPricingInput
instance ToSchema AP.RevolvingAssumption
instance ToSchema AP.ApplyAssumptionType
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

instance ToSchema (DB.TestDeal AB.Mortgage)
instance ToSchema (DB.TestDeal AB.Loan)
instance ToSchema (DB.TestDeal AB.Installment)
instance ToSchema (DB.TestDeal AB.Lease)
instance ToSchema (DB.TestDeal AB.Receivable)
instance ToSchema (DB.TestDeal AB.AssetUnion)
instance ToSchema (DB.TestDeal AB.FixedAsset)

instance ToSchema (DB.PoolType AB.Mortgage)
instance ToSchema (DB.PoolType AB.Loan)
instance ToSchema (DB.PoolType AB.Installment)
instance ToSchema (DB.PoolType AB.Lease)
instance ToSchema (DB.PoolType AB.FixedAsset)
instance ToSchema (DB.PoolType AB.Receivable)
instance ToSchema (DB.PoolType AB.AssetUnion)

instance ToSchema (DB.UnderlyingDeal AB.Mortgage)
instance ToSchema (DB.UnderlyingDeal AB.Loan)
instance ToSchema (DB.UnderlyingDeal AB.Installment)
instance ToSchema (DB.UnderlyingDeal AB.Lease)
instance ToSchema (DB.UnderlyingDeal AB.FixedAsset)
instance ToSchema (DB.UnderlyingDeal AB.Receivable)
instance ToSchema (DB.UnderlyingDeal AB.AssetUnion)

instance ToSchema ResultComponent
instance ToSchema L.PriceResult
instance ToSchema DealType

type RunResp = (DealType , Maybe (Map.Map PoolId CF.CashFlowFrame), Maybe [ResultComponent],Maybe (Map.Map String L.PriceResult))

wrapRun :: DealType -> Maybe AP.ApplyAssumptionType -> AP.NonPerfAssumption -> RunResp
wrapRun (MDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump 
                                     in 
                                       (MDeal _d,_pflow,_rs,_p) -- `debug` ("Run Done with deal->"++ show _d)
wrapRun (RDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (RDeal _d,_pflow,_rs,_p)
wrapRun (IDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (IDeal _d,_pflow,_rs,_p)
wrapRun (LDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (LDeal _d,_pflow,_rs,_p)
wrapRun (FDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (FDeal _d,_pflow,_rs,_p)
wrapRun (UDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump 
                                     in 
                                       (UDeal _d,_pflow,_rs,_p)                                       
wrapRun (VDeal d) mAssump mNonPerfAssump = let 
                                       (_d,_pflow,_rs,_p) = D.runDeal d D.DealPoolFlowPricing mAssump mNonPerfAssump
                                     in 
                                       (VDeal _d,_pflow,_rs,_p)                                       
wrapRun x _ _ = error $ "RunDeal Failed ,due to unsupport deal type "++ show x


data PoolTypeWrap = LPool (DB.PoolType AB.Loan)
                  | IPool (DB.PoolType AB.Installment)
                  | MPool (DB.PoolType AB.Mortgage)
                  | RPool (DB.PoolType AB.Lease)
                  | FPool (DB.PoolType AB.FixedAsset)
                  | VPool (DB.PoolType AB.Receivable)
                  | UPool (DB.PoolType AB.AssetUnion)
                  deriving(Show, Generic)


type RunPoolTypeRtn = Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance)

patchCumulativeToPoolRun :: RunPoolTypeRtn -> RunPoolTypeRtn
patchCumulativeToPoolRun = Map.map (\(CF.CashFlowFrame txns,stats) -> (CF.CashFlowFrame (CF.patchCumulative (0,0,0,0,0,0) txns []),stats))

wrapRunPoolType :: PoolTypeWrap -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] ->  RunPoolTypeRtn
wrapRunPoolType (MPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (LPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (IPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (RPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (FPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (VPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType (UPool pt) assump mRates = D.runPoolType pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType x _ _ = error $ "RunPool Failed ,due to unsupport pool type "++ show x


data RunAssetReq = RunAssetReq Date [AB.AssetUnion] (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption]) (Maybe PricingMethod)
                   deriving(Show, Generic)

instance ToSchema RunAssetReq

wrapRunAsset :: RunAssetReq -> ((CF.CashFlowFrame, Map.Map CutoffFields Balance), Maybe [PriceResult])
wrapRunAsset (RunAssetReq d assets Nothing mRates Nothing) 
  = (P.aggPool Nothing ((\a -> (MA.calcAssetUnion a d mRates,Map.empty)) <$> assets), Nothing) 
wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates Nothing) 
  = (P.aggPool Nothing ((\a -> MA.projAssetUnion a d assumps mRates) <$> assets), Nothing) 

wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates (Just pm)) 
  = let 
      assetCf = P.aggPool Nothing $ (\a -> D.projAssetUnion a d assumps mRates ) <$> assets 
      pricingResult = (\a -> D.priceAssetUnion a d pm assumps mRates) <$> assets
    in
      (assetCf , Just pricingResult)

--TODO implement on running via ByIndex

type ScenarioName = String

data RunDealReq = SingleRunReq DealType (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiScenarioRunReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiDealRunReq (Map.Map ScenarioName DealType) (Maybe AP.ApplyAssumptionType) AP.NonPerfAssumption
                | MultiScenarioAndCurveRunReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                deriving(Show, Generic)

data RunSimDealReq = OASReq DealType (Map.Map ScenarioName AP.ApplyAssumptionType) AP.NonPerfAssumption
                    deriving(Show, Generic)

instance ToSchema PoolTypeWrap

instance ToSchema RunDealReq
data RunPoolReq = SingleRunPoolReq PoolTypeWrap (Maybe AP.ApplyAssumptionType) (Maybe [RateAssumption])
                | MultiScenarioRunPoolReq PoolTypeWrap (Map.Map ScenarioName AP.ApplyAssumptionType) (Maybe [RateAssumption])
                deriving(Show, Generic)

instance ToSchema RunPoolReq

data RunDateReq = RunDateReq Date DatePattern
                deriving(Show, Generic)
instance ToSchema RunDateReq


$(deriveJSON defaultOptions ''DealType)

$(concat <$> traverse (deriveJSON defaultOptions) [''RunDealReq, ''RunPoolReq,''RunAssetReq, ''RunDateReq,''PoolTypeWrap])

-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type PoolRunResp = Map.Map PoolId (CF.CashFlowFrame, Map.Map CutoffFields Balance)

type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] ((CF.CashFlowFrame, Map.Map CutoffFields Balance),Maybe [PriceResult])
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] PoolRunResp
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName PoolRunResp)
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
--            :<|> "runDealOAS" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName [PriceResult])
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runDate" :> ReqBody '[JSON] RunDateReq :> Post '[JSON] [Date]


engineAPI :: Proxy EngineAPI
engineAPI = Proxy


type API = SwaggerAPI :<|> EngineAPI

engineSwagger:: OpenApi
engineSwagger = toOpenApi engineAPI
                    & info.title .~ "Hastructure API"
                    & info.version .~  T.pack (_version version1)
                    & info.description ?~ "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs"
                    & info.license ?~ "BSD 3"



-- showVersion :: Handler (Envelope '[] Version)
showVersion :: Handler Version
showVersion = return version1

runAsset :: RunAssetReq -> Handler ((CF.CashFlowFrame, Map.Map CutoffFields Balance),Maybe [PriceResult])
runAsset req = return $ wrapRunAsset req

runPool :: RunPoolReq -> Handler PoolRunResp
runPool (SingleRunPoolReq pt passumption mRates) = return $ patchCumulativeToPoolRun $ wrapRunPoolType pt passumption mRates

runPoolScenarios :: RunPoolReq -> Handler (Map.Map ScenarioName PoolRunResp)
runPoolScenarios (MultiScenarioRunPoolReq pt mAssumps mRates) = return $ Map.map (\assump -> wrapRunPoolType pt (Just assump) mRates) mAssumps

runDeal :: RunDealReq -> Handler RunResp
runDeal (SingleRunReq dt assump nonPerfAssump) =  return $ wrapRun dt assump nonPerfAssump

runDealScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealScenarios (MultiScenarioRunReq dt mAssumps nonPerfAssump) = return $ Map.map (\singleAssump -> wrapRun dt (Just singleAssump) nonPerfAssump) mAssumps

runMultiDeals :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runMultiDeals (MultiDealRunReq mDts assump nonPerfAssump) = return $ Map.map (\singleDealType -> wrapRun singleDealType assump nonPerfAssump) mDts

runDate :: RunDateReq -> Handler [Date]
runDate (RunDateReq sd dp) = return $ DU.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")

myServer :: ServerT API Handler
myServer =  return engineSwagger
      :<|> showVersion 
      :<|> runAsset
      :<|> runPool
      :<|> runPoolScenarios
      :<|> runDeal
      :<|> runDealScenarios
      :<|> runMultiDeals
      :<|> runDate
--      :<|> error "not implemented"


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} 
            deriving (Show,Generic)

instance FromJSON Config

app :: Application
app = serve (Proxy :: Proxy API) myServer


main :: IO ()
main = 
  do 
    writeSwaggerJSON
    config <- BS.readFile "config.yml"
    let mc = Y.decodeEither' config :: Either ParseException Config
    let (Config _p) = case mc of
                        Left exp -> Config 8081
                        Right c -> c
    print ("Engine start with version:"++ _version version1++";running at Port:"++ show _p)
    run _p app
--      $ errorMwDefJson
-- $(deriveJSON defaultOptions ''DealType)
