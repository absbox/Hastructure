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
--import Network.HTTP.Types.Status
import Servant.OpenApi
import Servant
import Servant.Types.SourceT (source)
import Servant.API.ContentTypes (contentType)

import Types
import MainBase
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

import Debug.Trace
debug = flip Debug.Trace.trace


version1 :: Version 
version1 = Version "0.50.3"


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

wrapRunPoolType :: Bool -> PoolTypeWrap -> Maybe AP.ApplyAssumptionType -> Maybe [RateAssumption] -> RunPoolTypeRtn
wrapRunPoolType flag (MPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (LPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (IPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (RPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (FPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (VPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (PPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})
wrapRunPoolType flag (UPool pt) assump mRates = D.runPoolType flag pt assump $ Just (AP.NonPerfAssumption{AP.interest = mRates})


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
                    & info.description ?~ "Hastructure is a white-label friendly Cashflow & Analytics Engine for MBS/ABS and REITs"
                    & info.license ?~ "BSD 3"

-- showVersion :: Handler (Envelope '[] Version)
showVersion :: Handler Version
showVersion = return version1

runAsset :: RunAssetReq -> Handler RunAssetResp
runAsset req = return $ wrapRunAsset req

runPool :: RunPoolReq -> Handler PoolRunResp
runPool (SingleRunPoolReq f pt passumption mRates) 
  = return $
      patchCumulativeToPoolRun <$> wrapRunPoolType f pt passumption mRates

runPoolScenarios :: RunPoolReq -> Handler (Map.Map ScenarioName PoolRunResp)
runPoolScenarios (MultiScenarioRunPoolReq f pt mAssumps mRates) 
  = return $ Map.map (\assump -> patchCumulativeToPoolRun <$> wrapRunPoolType f pt (Just assump) mRates) 
                      mAssumps

runDeal :: RunDealReq -> Handler RunResp
runDeal (SingleRunReq f dt assump nonPerfAssump) = return $ wrapRun f dt assump nonPerfAssump


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

queryDealType :: DealType -> Date -> DealStats -> Either String Rational
queryDealType (MDeal _d) = Q.queryCompound _d 
queryDealType (RDeal _d) = Q.queryCompound _d 
queryDealType (IDeal _d) = Q.queryCompound _d
queryDealType (LDeal _d) = Q.queryCompound _d
queryDealType (FDeal _d) = Q.queryCompound _d
queryDealType (UDeal _d) = Q.queryCompound _d
queryDealType (VDeal _d) = Q.queryCompound _d
queryDealType (PDeal _d) = Q.queryCompound _d

queryClosingDate :: DealType -> Either String Date
queryClosingDate (MDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (RDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (IDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (LDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (FDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (UDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (VDeal _d) = DD.getClosingDate (DB.dates _d) 
queryClosingDate (PDeal _d) = DD.getClosingDate (DB.dates _d) 


queryDealTypeBool :: DealType -> Date -> DealStats -> Either String Bool
queryDealTypeBool (MDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (RDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (IDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (LDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (FDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (UDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (VDeal _d) d s = Q.queryDealBool _d s d
queryDealTypeBool (PDeal _d) d s = Q.queryDealBool _d s d

testDealTypeBool :: DealType -> Date -> Pre -> Either String Bool
testDealTypeBool (MDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (RDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (IDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (LDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (FDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (UDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (VDeal _d) d p = Q.testPre d _d p 
testDealTypeBool (PDeal _d) d p = Q.testPre d _d p 

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
doTweak r (StressPoolDefault _) (dt , Just assumps, nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving}, f) 
  = let
      stressed = over (AP.applyAssumptionTypeAssetPerf . _1 ) (stressDefaultAssetPerf (toRational r)) assumps
      stressedNonPerf = nonPerfAssump {AP.revolving = stressRevovlingPerf (stressDefaultAssetPerf (toRational r)) mRevolving }
    in
      (dt ,Just stressed, stressedNonPerf, f)

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

evalRootFindStop (BalanceFormula ds targetBal) (dt,collectedFlow,logs,_,osPflow) 
  = let 
      _date = case find (\(EndRun d msg) -> True) (reverse logs) of
                Just (EndRun (Just d) _ ) -> d
                Nothing -> case queryClosingDate dt of
                             Right d' -> d'
                             Left err -> error $ "Error in BalanceFormula: " ++ err
      v = case queryDealType dt _date (Q.patchDateToStats _date ds)  of
            Right v' -> fromRational v'
            Left err -> error $ "Error in BalanceFormula: " ++ err
    in
      (fromRational . toRational) $ v - targetBal -- `debug` ("querydate" ++ show _date++"iteration" ++ show v ++ " target:" ++ show targetBal ++ ">> " ++ show ( v- targetBal))



rootFindAlgo :: DealRunInput -> RootFindTweak -> RootFindStop -> Double -> Double
rootFindAlgo (dt ,poolAssumps, runAssumps, f) tweak stop r 
  = let 
      (dt' ,poolAssumps', runAssumps', f) = doTweak r tweak (dt ,poolAssumps, runAssumps, f)
    in 
      case wrapRun f dt' poolAssumps' runAssumps' of
        Right runRespRight -> evalRootFindStop stop runRespRight -- `debug` ("RootFinder with f" ++ show r++ "with assumpt" ++ show poolAssumps')
        Left errorMsg -> -1

runRootFinderBy :: RootFindReq -> Handler (Either String RootFindResp)
runRootFinderBy (RootFinderReq req@(dt,Just assumps,nonPerfAssump@AP.NonPerfAssumption{AP.revolving = mRevolving},f) tweak stop)
  = return $
      let 
        itertimes = 1000
        def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.000001}
        riddersFn = case tweak of
                      SplitFixedBalance _ _ (l,h) -> ridders def (min h 0.99, max l 0.00001)
                      StressPoolDefault (l,h)  -> ridders def (h ,max l 0.00)
                      StressPoolPrepayment (l,h) -> ridders def (h ,max l 0.00)
                      MaxSpreadTo _ (l,h) -> ridders def (h ,max l 0.00)
      in
        case riddersFn (rootFindAlgo req tweak stop) of
          Root r -> return $ RFResult r (doTweak r tweak req)
          NotBracketed -> Left "Not able to bracket the root"
          SearchFailed -> Left "Not able to find the root"

runDealScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealScenarios (MultiScenarioRunReq f dt mAssumps nonPerfAssump) 
  = return $ Map.map (\singleAssump -> wrapRun f dt (Just singleAssump) nonPerfAssump) mAssumps

runMultiDeals :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runMultiDeals (MultiDealRunReq f mDts assump nonPerfAssump) 
  = return $ Map.map (\singleDealType -> wrapRun f singleDealType assump nonPerfAssump) mDts

runDate :: RunDateReq -> Handler [Date]
runDate (RunDateReq sd dp md)
  = return $ 
      case md of
        Nothing -> DU.genSerialDatesTill2 IE sd dp (Lib.toDate "20990101")
        Just d -> DU.genSerialDatesTill2 IE sd dp d

runDealByRunScenarios :: RunDealReq -> Handler (Map.Map ScenarioName RunResp)
runDealByRunScenarios (MultiRunAssumpReq f dt mAssump nonPerfAssumpMap)
  = return $ Map.map (wrapRun f dt mAssump) nonPerfAssumpMap

runDealByCombo :: RunDealReq -> Handler (Map.Map String RunResp)
runDealByCombo (MultiComboReq f dMap assumpMap nonPerfAssumpMap)
  = let 
      dList = Map.toList dMap
      aList = Map.toList assumpMap
      nList = Map.toList nonPerfAssumpMap
      r = [ (intercalate "^" [dk,ak,nk], wrapRun f d a n) | (dk,d) <- dList, (ak,a) <- aList, (nk,n) <- nList ]
      rMap = Map.fromList r
    in 
      return rMap

myServer :: ServerT API Handler
myServer =  return engineSwagger
      :<|> showVersion 
      :<|> runAsset
      :<|> runPool
      :<|> runPoolScenarios
      :<|> runDeal
      :<|> runDealScenarios
      :<|> runMultiDeals
      :<|> runDealByRunScenarios
      :<|> runDealByCombo
      :<|> runRootFinderBy
      :<|> runDate


writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty engineSwagger)

data Config = Config { port :: Int} 
            deriving (Show,Generic)

instance FromJSON Config

app :: Application
app = simpleCors $ serve (Proxy :: Proxy API) myServer

main :: IO ()
main = 
  do 
    writeSwaggerJSON
    config <- BS.readFile "config.yml"
    curTime <- getCurrentTime
    let mc = Y.decodeEither' config :: Either ParseException Config
    let (Config _p) = case mc of
                        Left exp -> Config 8081
                        Right c -> c
    print (show curTime ++ ">> Engine start with version:"++ _version version1++";running at Port:"++ show _p)
    run _p app
