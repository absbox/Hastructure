{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main 
  where 

import Prelude ()
import Prelude.Compat
import System.Environment
import GHC.Conc (getNumCapabilities)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad (mapM)
import Control.Exception (Exception,throwIO,throw,SomeException,displayException)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
-- import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Map
import Data.Either (fromLeft)
import qualified Data.Set as S
import Data.Proxy
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Data.Maybe
import Data.Yaml as Y
import Data.OpenApi hiding (Server,contentType)
import qualified Data.Map as Map
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
--import Network.HTTP.Types.Status
import Network.HTTP.Types (status500,hContentType)
import Data.CaseInsensitive (mk)
import qualified Network.Wai as Wai
import qualified Data.Aeson as Aeson
import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)

import Types
import MainBase
import Util (boundedParMap,forceMapValues)
import ServerConfig
import qualified Deal as D
import qualified Deal.DealBase as DB
import qualified Deal.DealDate as DD
import qualified Deal.DealMod as DM
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

import Numeric.RootFinding

import qualified Data.Version as DV
import qualified Paths_Hastructure as Paths

import Debug.Trace
debug = flip Debug.Trace.trace

version1 :: Version 
version1 = Version (DV.showVersion Paths.version)


wrapRun :: [D.ExpectReturn] -> DealType -> Maybe AP.ApplyAssumptionType -> AP.NonPerfAssumption -> RunResp
wrapRun fs (MDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p, _osPflow) <- D.runDeal d (S.fromList fs)  mAssump mNonPerfAssump
      return (MDeal _d,_pflow,_rs,_p,_osPflow) 
wrapRun fs (RDeal d) mAssump mNonPerfAssump 
  = do 
      (_d,_pflow,_rs,_p,_osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (RDeal _d,_pflow,_rs,_p,_osPflow)
wrapRun fs (IDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p,_osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (IDeal _d,_pflow,_rs,_p,_osPflow)
wrapRun fs (LDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p, _osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (LDeal _d,_pflow,_rs,_p,_osPflow)
wrapRun fs (FDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p,  _osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (FDeal _d,_pflow,_rs,_p,_osPflow)
wrapRun fs (UDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p,  _osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump 
      return (UDeal _d,_pflow,_rs,_p,_osPflow)                                       
wrapRun fs (VDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p,  _osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (VDeal _d,_pflow,_rs,_p,_osPflow)                                       
wrapRun fs (PDeal d) mAssump mNonPerfAssump 
  = do
      (_d,_pflow,_rs,_p, _osPflow) <- D.runDeal d (S.fromList fs) mAssump mNonPerfAssump
      return (PDeal _d,_pflow,_rs,_p,_osPflow)


patchCumulativeToPoolRun :: RunPoolTypeRtn_ -> RunPoolTypeRtn_
patchCumulativeToPoolRun
  = Map.map
          (\(CF.CashFlowFrame _ txns,mAssetFlow) -> 
            (CF.CashFlowFrame (0,Lib.toDate "19000101",Nothing) (CF.patchCumulative (0,0,0,0,0,0) [] txns),mAssetFlow))

-- flag -> breakdown flag
wrapRunPoolType :: Bool -> PoolTypeWrap -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> RunPoolTypeRtn
wrapRunPoolType flag (MPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (LPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (IPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (RPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (FPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (VPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (PPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)
wrapRunPoolType flag (UPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.nonPerfWithRates mRates)


wrapRunAsset :: RunAssetReq -> RunAssetResp
wrapRunAsset (RunAssetReq d assets Nothing mRates Nothing) 
  = do 
      cfs <- traverse (\a -> MA.calcAssetUnion a d mRates) assets
      return (fst (P.aggPool Nothing [(cf,Map.empty) | cf <- cfs]), Nothing) 
wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates Nothing) 
  = do 
      cfs <- traverse (\a -> MA.projAssetUnion a d assumps mRates) assets
      return (fst (P.aggPool Nothing [(cf,Map.empty) | (cf,_) <- cfs])  , Nothing) 
wrapRunAsset (RunAssetReq d assets (Just (AP.PoolLevel assumps)) mRates (Just pm)) 
  = do 
      cfs <- traverse (\a -> MA.projAssetUnion a d assumps mRates)  assets
      pricingResult <- traverse (\a -> D.priceAssetUnion a d pm assumps mRates) assets
      let (assetCf,_) = P.aggPool Nothing cfs
      return (assetCf , Just pricingResult)
-- asset projection supports pool-level assumptions only; anything else (per-pool/per-index/
-- obligor strategies, or pricing without assumptions) is rejected without a run
wrapRunAsset _ = Left "runAsset: unsupported pool assumption shape"

-- Swagger API
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi
type EngineAPI = "version" :> Get '[JSON] Version
            :<|> "runAsset" :> ReqBody '[JSON] RunAssetReq :> Post '[JSON] RunAssetResp
            :<|> "runPool" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] PoolRunResp
            :<|> "runPoolByScenarios" :> ReqBody '[JSON] RunPoolReq :> Post '[JSON] (Map.Map ScenarioName PoolRunResp)
            :<|> "runDeal" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] RunResp
            :<|> "runDealByScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runMultiDeals" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runDealByRunScenarios" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map ScenarioName RunResp)
            :<|> "runByCombo" :> ReqBody '[JSON] RunDealReq :> Post '[JSON] (Map.Map String RunResp)
            :<|> "runByRootFinder" :> ReqBody '[JSON] RootFindReq :> Post '[JSON] (Either String RootFindResp)
            :<|> "runDate" :> ReqBody '[JSON] RunDateReq :> Post '[JSON] [Date]

engineAPI :: Proxy EngineAPI
engineAPI = Proxy

type API = SwaggerAPI :<|> EngineAPI

engineSwagger:: OpenApi
engineSwagger = toOpenApi engineAPI
                    & info.title .~ "Hastructure API"
                    & info.version .~  T.pack (_version version1)
                    & info.description ?~ (T.pack
                      "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs.\
                      \ Multi-run endpoints (runPoolByScenarios, runDealByScenarios, runMultiDeals, runDealByRunScenarios, runByCombo) are capped per request by the maxRunNum.<endpoint> keys in config.yml (default 1000); oversized requests are rejected with HTTP 400.")
                    & info.license ?~ "BSD 3"

-- showVersion :: Handler (Envelope '[] Version)
showVersion :: Handler Version
showVersion = return $! version1

runAsset :: RunAssetReq -> Handler RunAssetResp
runAsset req = return $! wrapRunAsset req

runPool :: RunPoolReq -> Handler PoolRunResp
runPool (SingleRunPoolReq f pt passumption mRates) 
  = return $!
      patchCumulativeToPoolRun <$> wrapRunPoolType f pt passumption mRates
runPool (MultiScenarioRunPoolReq _ _ _ _) = return $ Left "runPool: expected SingleRunPoolReq"

-- | Per-request run bounding for a multi-run endpoint: the resolved worker
-- cap ('boundedParMap') plus the endpoint's total-run limit (#345).
data MultiRunCap = MultiRunCap { mcWorkers :: Int, mcMaxRuns :: Int }

data ResolvedConfig = ResolvedConfig
  { rcPort    :: Int
  , rcWorkers :: Int
  , rcLimits  :: RunLimits
  }

mkRunCap :: ResolvedConfig -> (RunLimits -> Int) -> MultiRunCap
mkRunCap rc getLim = MultiRunCap (rcWorkers rc) (getLim (rcLimits rc))

-- | Reject the request with HTTP 400 when its total run count exceeds the
-- endpoint's configured cap; the message names the endpoint, the limit and
-- the `maxRunNum.<endpoint>` config key to raise.
enforceRunCap :: String -> MultiRunCap -> Int -> Handler ()
enforceRunCap ep cap n
  = case checkRunNumLimit ep (mcMaxRuns cap) n of
      Left msg -> throwError (badRequestJSON msg)
      Right () -> return ()

-- | Reject a request routed to an endpoint whose payload shape it does not
-- match (#329). Map-valued multi-run responses have no error slot, so the
-- rejection rides the HTTP status code instead; single-run endpoints return
-- the message as 'Left' in-band, matching the existing run-error convention.
-- The body uses the standard JSON error envelope (#'badRequestJSON').
badRequest :: String -> Handler a
badRequest msg = throwError (badRequestJSON msg)

runPoolScenarios :: MultiRunCap -> RunPoolReq -> Handler (Map.Map ScenarioName PoolRunResp)
runPoolScenarios cap (MultiScenarioRunPoolReq f pt mAssumps mRates)
  = do
      enforceRunCap "runPoolByScenarios" cap (Map.size mAssumps)
      return $! forceMapValues $ boundedParMap (mcWorkers cap)
                           (\assump -> patchCumulativeToPoolRun <$> wrapRunPoolType f pt (Just assump) mRates)
                           mAssumps
runPoolScenarios _ _
  = badRequest "runPoolByScenarios: expected MultiScenarioRunPoolReq"

runDeal :: RunDealReq -> Handler RunResp
runDeal (SingleRunReq f dt assump nonPerfAssump) = return $! wrapRun f dt assump nonPerfAssump
runDeal _ = return $ Left "runDeal: expected SingleRunReq"


-- Stressing default assumption from AssetPerfAssumption
stressDefaultAssetPerf :: Rate -> AP.AssetPerfAssumption -> AP.AssetPerfAssumption
stressDefaultAssetPerf r (AP.MortgageAssump (Just da) mp mr ms) 
  = AP.MortgageAssump (Just (AP.stressDefaultAssump r da)) mp mr ms
stressDefaultAssetPerf r (AP.LoanAssump (Just da) mp mr ms) 
  = AP.LoanAssump (Just (AP.stressDefaultAssump r da)) mp mr ms 
stressDefaultAssetPerf r (AP.InstallmentAssump (Just da) mp mr ms) 
  = AP.InstallmentAssump (Just (AP.stressDefaultAssump r da)) mp mr ms
stressDefaultAssetPerf r (AP.ReceivableAssump (Just da) mr ms) 
  = AP.ReceivableAssump (Just (AP.stressDefaultAssump r da)) mr ms
stressDefaultAssetPerf r (AP.LeaseAssump (Just (AP.DefaultByContinuation dr)) mg mr me) 
  = AP.LeaseAssump (Just (AP.DefaultByContinuation (min 1.0 dr * r))) mg mr me
stressDefaultAssetPerf r (AP.LeaseAssump (Just (AP.DefaultByTermination dr)) mg mr me) 
  = AP.LeaseAssump (Just (AP.DefaultByTermination (min 1.0 dr * r))) mg mr me
stressDefaultAssetPerf _ x = x

-- Stressing prepayment assumption from AssetPerfAssumption
stressPrepayAssetPerf :: Rate -> AP.AssetPerfAssumption -> AP.AssetPerfAssumption
stressPrepayAssetPerf r (AP.MortgageAssump da (Just mp) mr ms) 
  = AP.MortgageAssump da (Just (AP.stressPrepaymentAssump r mp)) mr ms
stressPrepayAssetPerf r (AP.MortgageDeqAssump da (Just mp) mr ms) 
  = AP.MortgageDeqAssump da (Just (AP.stressPrepaymentAssump r mp)) mr ms
stressPrepayAssetPerf r (AP.LoanAssump da (Just mp) mr ms)
  = AP.LoanAssump da (Just (AP.stressPrepaymentAssump r mp)) mr ms
stressPrepayAssetPerf r (AP.InstallmentAssump da (Just mp) mr ms)
  = AP.InstallmentAssump da (Just (AP.stressPrepaymentAssump r mp)) mr ms
stressPrepayAssetPerf _ x = x




-- Stressing default assumption
stressRevovlingPerf :: (AP.AssetPerfAssumption -> AP.AssetPerfAssumption)-> Maybe AP.RevolvingAssumption -> Maybe AP.RevolvingAssumption
stressRevovlingPerf f Nothing = Nothing
stressRevovlingPerf f (Just (AP.AvailableAssets rp applyAssumpType)) 
  = Just (AP.AvailableAssets rp (over (AP.applyAssumptionTypeAssetPerf . _1) f applyAssumpType))
stressRevovlingPerf f (Just (AP.AvailableAssetsBy m))
  = Just (AP.AvailableAssetsBy (Map.map (over (_2 . AP.applyAssumptionTypeAssetPerf . _1) f) m))

modifyDealType :: DM.ModifyType -> Double -> DealType -> DealType
modifyDealType dm f (MDeal d) = MDeal $ DM.modDeal dm f d
modifyDealType dm f (RDeal d) = RDeal $ DM.modDeal dm f d
modifyDealType dm f (IDeal d) = IDeal $ DM.modDeal dm f d
modifyDealType dm f (LDeal d) = LDeal $ DM.modDeal dm f d
modifyDealType dm f (FDeal d) = FDeal $ DM.modDeal dm f d
modifyDealType dm f (UDeal d) = UDeal $ DM.modDeal dm f d
modifyDealType dm f (VDeal d) = VDeal $ DM.modDeal dm f d
modifyDealType dm f (PDeal d) = PDeal $ DM.modDeal dm f d


queryClosingDate :: DealType -> Either String Date
queryClosingDate (MDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (RDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (IDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (LDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (FDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (UDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (VDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (PDeal _d) = DD.getClosingDate (DB.dates _d) 




getDealBondMap :: DealType -> Map.Map BondName L.Bond
getDealBondMap (MDeal d) = DB.bonds d
getDealBondMap (RDeal d) = DB.bonds d
getDealBondMap (IDeal d) = DB.bonds d
getDealBondMap (LDeal d) = DB.bonds d
getDealBondMap (FDeal d) = DB.bonds d
getDealBondMap (UDeal d) = DB.bonds d
getDealBondMap (VDeal d) = DB.bonds d
getDealBondMap (PDeal d) = DB.bonds d

getDealFeeMap :: DealType -> Map.Map FeeName F.Fee
getDealFeeMap (MDeal d) = DB.fees d
getDealFeeMap (RDeal d) = DB.fees d
getDealFeeMap (IDeal d) = DB.fees d
getDealFeeMap (LDeal d) = DB.fees d
getDealFeeMap (FDeal d) = DB.fees d
getDealFeeMap (UDeal d) = DB.fees d
getDealFeeMap (VDeal d) = DB.fees d
getDealFeeMap (PDeal d) = DB.fees d

doTweak :: Double -> RootFindTweak -> DealRunInput -> DealRunInput
-- unreachable: 'runRootFinderBy' rejects stress tweaks when pool assumptions are absent
doTweak _ (StressPoolDefault _) inp@(_, Nothing, _, _) = inp

doTweak r (StressPoolDefault _) (dt , Just assumps, nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving}, f) 
  = let
      stressed = over (AP.applyAssumptionTypeAssetPerf . _1 ) (stressDefaultAssetPerf (toRational r)) assumps
      stressedNonPerf = nonPerfAssump {AP.revolving = stressRevovlingPerf (stressDefaultAssetPerf (toRational r)) mRevolving }
    in
      (dt ,Just stressed, stressedNonPerf, f)

-- unreachable: 'runRootFinderBy' rejects stress tweaks when pool assumptions are absent
doTweak _ (StressPoolPrepayment _) inp@(_, Nothing, _, _) = inp

doTweak r (StressPoolPrepayment _) (dt , Just assumps, nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving}, f) 
  = let
      stressed = over (AP.applyAssumptionTypeAssetPerf . _1 ) (stressPrepayAssetPerf (toRational r)) assumps
      stressedNonPerf = nonPerfAssump {AP.revolving = stressRevovlingPerf (stressPrepayAssetPerf (toRational r)) mRevolving }
    in
      (dt ,Just stressed, stressedNonPerf, f)

doTweak r (MaxSpreadTo bn _) (dt , mAssump, rAssump, f)
  = (modifyDealType (DM.AddSpreadToBonds bn) r dt , mAssump, rAssump, f)

doTweak r (SplitFixedBalance bn1 bn2 _) (dt , mAssump, rAssump, f)
  = (modifyDealType (DM.SlideBalances bn1 bn2) r dt , mAssump, rAssump, f)


evalRootFindStop :: RootFindStop -> RunRespRight -> Double
evalRootFindStop (BondIncurLoss bn) (dt,_,_,_,osPflow) 
  = let 
      bondBal = L.getOutstandingAmount $ getDealBondMap dt Map.! bn
    in
      (fromRational . toRational) $ bondBal - 0.01

evalRootFindStop (BondIncurIntLoss bn threshold) (dt,_,_,_,osPflow) 
  = let 
      dueIntAmt = L.getTotalDueInt $ getDealBondMap dt Map.! bn
    in
      (fromRational . toRational) $ threshold -  (dueIntAmt-0.01)

evalRootFindStop (BondIncurPrinLoss bn threshold) (dt,_,_,_,osPflow) 
  = let 
      duePrinAmt = L.getCurBalance $ getDealBondMap dt Map.! bn
    in
      (fromRational . toRational) $ threshold - (duePrinAmt-0.01)

evalRootFindStop (BondPricingEqOriginBal bn otherBondFlag otherFeeFlag) (dt,_,_,pResult,osPflow) 
  = let 
      -- bnds
      otherBondsName = [] 
      -- check fees/other bonds
      otherBondOustanding True = sum $ L.getOutstandingAmount <$> Map.elems (getDealBondMap dt)
      otherBondOustanding False = 0.0
      feeOutstanding True = sum $ L.getOutstandingAmount <$> Map.elems (getDealFeeMap dt)
      feeOutstanding False = 0.0 
      bondBal = L.getOriginBalance $ getDealBondMap dt Map.! bn
      v = maybe bondBal getPriceValue $ Map.lookup bn pResult -- TODO shortcut to avoid error
    in
      if (otherBondOustanding otherBondFlag+feeOutstanding otherFeeFlag) > 0  then 
        -1
      else
        (fromRational . toRational) $ bondBal - v 

evalRootFindStop (BondMetTargetIrr bn target) (dt,_,_,pResult,osPflow) 
  = let 
      v = L.extractIrrResult $ pResult Map.! bn
    in 
      case v of 
        Nothing -> -1  -- `debug` ("No IRR found for bond:"++ show bn)
        Just irr -> (fromRational . toRational) $ irr - target -- `debug` ("IRR for bond:"++ show target ++" is "++ show irr)

-- unreachable: 'runRootFinderBy' rejects BalanceFormula stops before the search
evalRootFindStop (BalanceFormula _ _) _ = -1




rootFindAlgo :: DealRunInput -> RootFindTweak -> RootFindStop -> Double -> Double
rootFindAlgo (dt ,poolAssumps, runAssumps, f) tweak stop r 
  = let 
      (dt' ,poolAssumps', runAssumps', f) = doTweak r tweak (dt ,poolAssumps, runAssumps, f)
    in 
      case wrapRun f dt' poolAssumps' runAssumps' of
        Right runRespRight -> evalRootFindStop stop runRespRight -- `debug` ("RootFinder with f" ++ show r++ "with assumpt" ++ show poolAssumps')
        Left errorMsg -> -1

runRootFinderBy :: RootFindReq -> Handler (Either String RootFindResp)
runRootFinderBy (FirstLossReq _ _)
  = return $ Left "rootFinder: FirstLossReq is a legacy request type; compose RootFinderReq (absbox runFirstLoss does)"
runRootFinderBy (MaxSpreadToFaceReq _ _ _ _)
  = return $ Left "rootFinder: MaxSpreadToFaceReq is a legacy request type; compose RootFinderReq (absbox runRootFinder does)"
runRootFinderBy req@(RootFinderReq runInput@(_,mAssumps,_,_) tweak stop)
  | tweakNeedsPoolAssump tweak && isNothing mAssumps
  = return $ Left "rootFinder: stressDefault/stressPrepayment requires pool assumptions"
  | BalanceFormula{} <- stop
  = return $ Left "rootFinder: BalanceFormula stop is not implemented yet"
  | otherwise
  = return $! runRidders runInput tweak stop

-- | Bond-only tweaks modify the deal structure and need no pool assumptions;
-- the stress tweaks vary pool performance, so they do.
tweakNeedsPoolAssump :: RootFindTweak -> Bool
tweakNeedsPoolAssump (StressPoolDefault _)    = True
tweakNeedsPoolAssump (StressPoolPrepayment _) = True
tweakNeedsPoolAssump _                        = False

-- | The ridders search itself; reached only after the guards in
-- 'runRootFinderBy' (legacy shapes, missing assumptions, unimplemented stops).
runRidders :: DealRunInput -> RootFindTweak -> RootFindStop -> Either String RootFindResp
runRidders runInput tweak stop
  = let
      itertimes = 1000
      def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.000001}
      riddersFn = case tweak of
                    SplitFixedBalance _ _ (l,h) -> ridders def (min h 0.99, max l 0.00001)
                    StressPoolDefault (l,h)  -> ridders def (h ,max l 0.00)
                    StressPoolPrepayment (l,h) -> ridders def (h ,max l 0.00)
                    MaxSpreadTo _ (l,h) -> ridders def (h ,max l 0.00)
    in
      case riddersFn (rootFindAlgo runInput tweak stop) of
        Root r -> return $ RFResult r (doTweak r tweak runInput)
        NotBracketed -> Left "Not able to bracket the root"
        SearchFailed -> Left "Not able to find the root"

runDealScenarios :: MultiRunCap -> RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealScenarios cap (MultiScenarioRunReq f dt mAssumps nonPerfAssump) 
  = do
      enforceRunCap "runDealByScenarios" cap (Map.size mAssumps)
      return $! forceMapValues $ boundedParMap (mcWorkers cap) (\singleAssump -> wrapRun f dt (Just singleAssump) nonPerfAssump) mAssumps
runDealScenarios _ _
  = badRequest "runDealByScenarios: expected MultiScenarioRunReq"

runMultiDeals :: MultiRunCap -> RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runMultiDeals cap (MultiDealRunReq f mDts assump nonPerfAssump) 
  = do
      enforceRunCap "runMultiDeals" cap (Map.size mDts)
      return $! forceMapValues $ boundedParMap (mcWorkers cap) (\singleDealType -> wrapRun f singleDealType assump nonPerfAssump) mDts
runMultiDeals _ _
  = badRequest "runMultiDeals: expected MultiDealRunReq"

runDate :: RunDateReq -> Handler [Date]
runDate (RunDateReq sd dp md)
  = return $!
      case md of
        Nothing -> DU.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")
        Just d -> DU.genSerialDatesTill2 IE sd dp d

runDealByRunScenarios :: MultiRunCap -> RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealByRunScenarios cap (MultiRunAssumpReq f dt mAssump nonPerfAssumpMap)
  = do
      enforceRunCap "runDealByRunScenarios" cap (Map.size nonPerfAssumpMap)
      return $! forceMapValues $ boundedParMap (mcWorkers cap) (wrapRun f dt mAssump) nonPerfAssumpMap
runDealByRunScenarios _ _
  = badRequest "runDealByRunScenarios: expected MultiRunAssumpReq"

runDealByCombo :: MultiRunCap -> RunDealReq -> Handler (Map.Map String RunResp)
runDealByCombo cap (MultiComboReq f dMap assumpMap nonPerfAssumpMap)
  = do
      -- Size the cartesian product before materialising it: `Map.fromList`
      -- below only shrinks (duplicate ^-joined keys), so the product is an
      -- upper bound on the actual run count and an oversized request is
      -- rejected without allocating it.
      enforceRunCap "runByCombo" cap (Map.size dMap * Map.size assumpMap * Map.size nonPerfAssumpMap)
      let inputs = Map.fromList [ (intercalate "^" [dk,ak,nk], (d,a,n)) 
                            | (dk,d) <- Map.toList dMap
                            , (ak,a) <- Map.toList assumpMap
                            , (nk,n) <- Map.toList nonPerfAssumpMap ]
      return $! forceMapValues $ boundedParMap (mcWorkers cap) (\(d,a,n) -> wrapRun f d a n) inputs
runDealByCombo _ _
  = badRequest "runByCombo: expected MultiComboReq"

myServer :: ResolvedConfig -> ServerT API Handler
myServer rc =
    return engineSwagger
      :<|> showVersion
      :<|> runAsset
      :<|> runPool
      :<|> runPoolScenarios (mkRunCap rc maxRunPoolByScenarios)
      :<|> runDeal
      :<|> runDealScenarios (mkRunCap rc maxRunDealByScenarios)
      :<|> runMultiDeals (mkRunCap rc maxRunMultiDeals)
      :<|> runDealByRunScenarios (mkRunCap rc maxRunDealByRunScenarios)
      :<|> runDealByCombo (mkRunCap rc maxRunByCombo)
      :<|> runRootFinderBy
      :<|> runDate


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config
  { port      :: Int
  , workerNum :: Maybe Int
  , maxRunNum :: Maybe RunLimitsRaw
  } deriving (Show,Generic)

instance FromJSON Config

app :: ResolvedConfig -> Application
app rc = simpleCors $ serve (Proxy :: Proxy API) (myServer rc)

-- | Render any exception escaping the request pipeline in the SAME JSON
-- envelope that in-band 'Either ErrorRep' responses use (@{"Left":"…"}@), plus
-- the CORS header so browser clients can read it. Logging stays on Warp's
-- default logger — this only changes the rendering. Connection-level failures
-- (resets\/timeouts) keep Warp's default rendering behavior: they should not
-- receive an HTTP response at all.
errorBoundary :: SomeException -> Wai.Response
errorBoundary e =
  responseLBS status500
    [ (hContentType, "application/json")
    , (mk "Access-Control-Allow-Origin", "*")
    ]
    (Aeson.encode (Left (T.pack ("engine error: " ++ displayException e)) :: Either T.Text Value))

-- | Server settings: port from @config.yml@ plus the JSON error boundary.
warpSettings :: Int -> Settings
warpSettings p = setPort p $ setOnExceptionResponse errorBoundary defaultSettings

main :: IO ()
main =
  do
    writeSwaggerJSON
    raw <- BS.readFile "config.yml"
    curTime <- getCurrentTime
    cfg <- either (failConfig . show) pure (Y.decodeEither' raw :: Either ParseException Config)
    limits <- either failConfig pure (resolveRunLimits (fromMaybe emptyRunLimitsRaw (maxRunNum cfg)))
    caps <- getNumCapabilities
    mWorkers <- lookupEnv "HASTRUCTURE_WORKERS"
    let rc = ResolvedConfig (port cfg)
                            (resolveWorkerNum caps (workerNum cfg) mWorkers)
                            limits
    print (show curTime ++ ">> Engine start with version:"++ _version version1
           ++ ";running at Port:" ++ show (rcPort rc)
           ++ ";workers:" ++ show (rcWorkers rc)
           ++ ";maxRunNum:" ++ show (rcLimits rc))
    runSettings (warpSettings (rcPort rc)) (app rc)
  where
    failConfig :: String -> IO a
    failConfig msg = throwIO (userError ("invalid config.yml: " ++ msg))
