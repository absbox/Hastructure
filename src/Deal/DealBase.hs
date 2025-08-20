{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Deal.DealBase (TestDeal(..),SPV(..),dealBonds,dealFees,dealAccounts,dealPool,PoolType(..),getIssuanceStats
                     ,getAllAsset,getAllAssetList,getAllCollectedFrame,getLatestCollectFrame,getAllCollectedTxns
                     ,getIssuanceStatsConsol,getAllCollectedTxnsList
                     ,getPoolIds,getBondByName, UnderlyingDeal(..),viewDealAllBonds,DateDesp(..),ActionOnDate(..)
                     ,sortActionOnDate,dealBondGroups
                     ,viewDealBondsByNames,poolTypePool,viewBondsInMap,bondGroupsBonds
                     ,increaseBondPaidPeriod,increasePoolCollectedPeriod
                     ,DealStatFields(..),getDealStatInt,isPreClosing,populateDealDates
                     ,bondTraversal,findBondByNames,updateBondInMap
                     ,_MultiPool,_ResecDeal,uDealFutureCf,uDealFutureScheduleCf
                     )                      
  where
import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified AssetClass.AssetBase as ACM
import qualified Call as C
import qualified InterestRate as IR
import Stmt
import Lib
import Util
import DateUtil
import Types
import Revolving
import Triggers

import Deal.DealCollection (CollectionRule(..))

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Data.DList as DL
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.IntMap (filterWithKey)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Pool as P
import qualified Types as CF

import Debug.Trace
import qualified Control.Lens as P
debug = flip trace


data DealComp = CompBond 
              | CompAccount 
              | CompFee 
              | CompPool 
              | CompTrigger 
              | CompLedger 
              | CompRateSwap 
              | CompRateCap 
              | CompCurrencySwap 
              | CompLiqProvider 
              deriving (Show,Eq,Ord,Generic,Read)

data ActionTypeOnDate = DoSettle
                      | DoAccrue
                      | DoUpdateRate

data ActionOnDate = EarnAccInt Date AccName              -- ^ sweep bank account interest
                  | ChangeDealStatusTo Date DealStatus   -- ^ change deal status
                  | AccrueFee Date FeeName               -- ^ accure fee
                  | ResetLiqProvider Date String         -- ^ reset credit for liquidity provider
                  | ResetLiqProviderRate Date String     -- ^ accure interest/premium amount for liquidity provider
                  | PoolCollection Date String           -- ^ collect pool cashflow and deposit to accounts
                  | RunWaterfall Date String             -- ^ execute waterfall on distribution date
                  | AccrueRunWaterfall Date String       -- ^ accrue all liabilities, execute waterfall on distribution date
                  | AccruePoolCollection Date String     -- ^ accrue all liabilities, collect pool cashflow and deposit to accounts
                  | DealClosed Date                      -- ^ actions to perform at the deal closing day, and enter a new deal status
                  | FireTrigger Date DealCycle String    -- ^ fire a trigger
                  | InspectDS Date [DealStats]           -- ^ inspect formulas
                  | CalcIRSwap Date String               -- ^ calc interest rate swap dates
                  | SettleIRSwap Date String             -- ^ settle interest rate swap dates
                  | AccrueCapRate Date String            -- ^ reset interest rate cap dates
                  | ResetBondRate Date String            -- ^ reset bond interest rate per bond's interest rate info
                  | StepUpBondRate Date String           -- ^ reset bond interest rate per bond's interest rate info
                  | ResetSrtRate Date String 
                  | ResetAccRate Date String 
                  | AccrueSrt Date String 
                  | MakeWhole Date Spread (Table Float Spread)
                  | IssueBond Date (Maybe Pre) String AccName L.Bond (Maybe DealStats) (Maybe DealStats)
                  | FundBond Date (Maybe Pre) String AccName Amount
                  | RefiBondRate Date AccountName BondName L.InterestInfo
                  | RefiBond Date AccountName L.Bond
                  | BuildReport StartDate EndDate        -- ^ build cashflow report between dates and balance report at end date
                  | StopRunFlag Date                     -- ^ stop the run with a message
                  | StopRunTest Date [Pre]               -- ^ stop the run with a condition
                  | HitStatedMaturity Date               -- ^ hit the stated maturity date
                  | TestCall Date                        -- ^ test call dates
                  deriving (Show,Generic,Read)

instance Ord ActionOnDate where
  compare a1 a2 = compare (getDate a1) (getDate a2)

instance Eq ActionOnDate where
  a1 == a2 = getDate a1 == getDate a2


instance TimeSeries ActionOnDate where
    getDate (RunWaterfall d _) = d
    getDate (ResetLiqProvider d _) = d
    getDate (PoolCollection d _) = d
    getDate (EarnAccInt d _) = d
    getDate (AccrueFee d _) = d
    getDate (DealClosed d) = d
    getDate (FireTrigger d _ _) = d
    getDate (ChangeDealStatusTo d _ ) = d
    getDate (InspectDS d _ ) = d
    getDate (CalcIRSwap d _ ) = d
    getDate (SettleIRSwap d _ ) = d
    getDate (AccrueCapRate d _ ) = d
    getDate (ResetBondRate d _) = d 
    getDate (StepUpBondRate d _) = d 
    getDate (ResetAccRate d _ ) = d 
    getDate (MakeWhole d _ _) = d 
    getDate (BuildReport sd ed) = ed
    getDate (IssueBond d _ _ _ _ _ _) = d
    getDate (RefiBondRate d _ _ _) = d
    getDate (RefiBond d _ _) = d
    getDate (ResetLiqProviderRate d _) = d
    getDate (TestCall d) = d
    getDate (FundBond d _ _ _ _) = d
    getDate (HitStatedMaturity d) = d
    getDate (StopRunTest d _) = d
    getDate x = error $ "Failed to match"++ show x


sortActionOnDate :: ActionOnDate -> ActionOnDate -> Ordering
sortActionOnDate a1 a2 
  | d1 == d2 = case (a1,a2) of
                  (PoolCollection {}, DealClosed {}) -> LT -- pool collection should be executed before deal closed
                  (DealClosed {}, PoolCollection {}) -> GT -- pool collection should be executed before deal closed
                  (BuildReport sd1 ed1 ,_) -> GT  -- build report should be executed last
                  (_ , BuildReport sd1 ed1) -> LT -- build report should be executed last
                  (TestCall _ ,_) -> GT  -- test call should be executed last
                  (_ , TestCall _) -> LT -- test call should be executed last
                  (CalcIRSwap _ _ ,SettleIRSwap _ _) -> LT  -- reset interest swap should be first
                  (SettleIRSwap _ _ ,CalcIRSwap _ _) -> GT  -- reset interest swap should be first
                  (_ , CalcIRSwap _ _) -> GT -- reset interest swap should be first
                  (CalcIRSwap _ _ ,_) -> LT  -- reset interest swap should be first
                  (_ , CalcIRSwap _ _) -> GT -- reset interest swap should be first
                  (StepUpBondRate {} ,_) -> LT  -- step up bond rate should be first
                  (_ , StepUpBondRate {}) -> GT -- step up bond rate should be first
                  (ResetBondRate {} ,_) -> LT  -- reset bond rate should be first
                  (_ , ResetBondRate {}) -> GT -- reset bond rate should be first
                  (EarnAccInt {} ,_) -> LT  -- earn should be first
                  (_ , EarnAccInt {}) -> GT -- earn should be first
                  (ResetLiqProvider {} ,_) -> LT  -- reset liq be first
                  (_ , ResetLiqProvider {}) -> GT -- reset liq be first
                  (PoolCollection {}, RunWaterfall {}) -> LT -- pool collection should be executed before waterfall
                  (RunWaterfall {}, PoolCollection {}) -> GT -- pool collection should be executed before waterfall
                  (_,_) -> EQ 
  | otherwise = compare d1 d2
  where 
    d1 = getDate a1 
    d2 = getDate a2 


type CutoffDate = Date
type ClosingDate = Date
type RevolvingDate = Date
type StatedDate = Date
type DistributionDates = DatePattern
type PoolCollectionDates = DatePattern


data DateDesp = PreClosingDates CutoffDate ClosingDate (Maybe RevolvingDate) StatedDate (Date,PoolCollectionDates) (Date,DistributionDates)
              -- <Pool Collection DP> <Waterfall DP> 
              --  (last collect,last pay), mRevolving end-date dp1-pool-pay dp2-bond-pay
              | CurrentDates (Date,Date) (Maybe Date) StatedDate (Date,PoolCollectionDates) (Date,DistributionDates)
              -- Dict based 
              | GenericDates (Map.Map DateType DatePattern)
              | AccruedGenericDates (Map.Map DateType DatePattern)
              deriving (Show,Eq, Generic,Ord)

type PoolCollectionActions = [ActionOnDate]
type BondDistributionActions = [ActionOnDate]
type CustomActions = [ActionOnDate]

populateDealDates :: DateDesp -> DealStatus -> Either ErrorRep (Date,Date,Date,PoolCollectionActions,BondDistributionActions,Date,CustomActions)
populateDealDates (PreClosingDates cutoff closing mRevolving end (firstCollect,poolDp) (firstPay,bondDp)) _
  = let 
      pa = [ PoolCollection _d "" | _d <- genSerialDatesTill2 IE firstCollect poolDp end ]
      ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE firstPay bondDp end ]
    in 
      return (cutoff,closing,firstPay,pa,ba,end, []) 

populateDealDates (CurrentDates (lastCollect,lastPay) mRevolving end (nextCollect,poolDp) (nextPay,bondDp)) _
  = let 
      futurePayDates = genSerialDatesTill2 IE nextPay bondDp end 
      ba = [ RunWaterfall _d "" | _d <- futurePayDates]
      futureCollectDates = genSerialDatesTill2 IE nextCollect poolDp end 
      pa = [ PoolCollection _d "" | _d <- futureCollectDates]
    in 
      return (lastCollect, lastPay,head futurePayDates, pa, ba, end, []) 

populateDealDates (GenericDates m) st 
  = let 
      requiredFields (PreClosing _) = (CutoffDate, ClosingDate, FirstPayDate, StatedMaturityDate , DistributionDates, CollectionDates) 
      requiredFields _  = (LastCollectDate, LastPayDate, NextPayDate, StatedMaturityDate, DistributionDates, CollectionDates) 
      isCustomWaterfallKey (CustomExeDates _) _ = True
      isCustomWaterfallKey _ _ = False
      custWaterfall = Map.toList $ Map.filterWithKey isCustomWaterfallKey m
    in 
      do 
        vals <- lookupTuple6 (requiredFields st) m
        case vals of
          (SingletonDate lastCollect, SingletonDate lastPayDate, SingletonDate nextPayDate , SingletonDate statedDate, bondDp, poolDp)
            -> let 
                  pa = [ PoolCollection _d "" | _d <- genSerialDatesTill2 EE lastCollect poolDp statedDate ]
                  ba = [ RunWaterfall _d "" | _d <- genSerialDatesTill2 IE nextPayDate bondDp statedDate ]
                  cu = [ RunWaterfall _d custName | (CustomExeDates custName, custDp) <- custWaterfall, _d <- genSerialDatesTill2 EE lastCollect custDp statedDate ]
                in 
                  return (lastCollect, lastPayDate, nextPayDate, pa, ba, statedDate, cu)
          _ 
            -> Left $ "Missing required dates in GenericDates in deal status" ++ (show st) ++ "but got"

populateDealDates (AccruedGenericDates m) st 
  = let 
      requiredFields (PreClosing _) = (CutoffDate, ClosingDate, FirstPayDate, StatedMaturityDate , DistributionDates, CollectionDates) 
      requiredFields _  = (LastCollectDate, LastPayDate, NextPayDate, StatedMaturityDate, DistributionDates, CollectionDates) 
      isCustomWaterfallKey (CustomExeDates _) _ = True
      isCustomWaterfallKey _ _ = False
      custWaterfall = Map.toList $ Map.filterWithKey isCustomWaterfallKey m
    in 
      do 
        vals <- lookupTuple6 (requiredFields st) m
        case vals of
          (SingletonDate lastCollect, SingletonDate lastPayDate, SingletonDate nextPayDate , SingletonDate statedDate, bondDp, poolDp)
            -> let 
                  pa = [ AccruePoolCollection _d "" | _d <- genSerialDatesTill2 EE lastCollect poolDp statedDate ]
                  ba = [ AccrueRunWaterfall _d "" | _d <- genSerialDatesTill2 IE nextPayDate bondDp statedDate ]
                  cu = [ AccrueRunWaterfall _d custName | (CustomExeDates custName, custDp) <- custWaterfall, _d <- genSerialDatesTill2 EE lastCollect custDp statedDate ]
                in 
                  return (lastCollect, lastPayDate, nextPayDate, pa, ba, statedDate, cu)
          _ 
            -> Left $ "Missing required dates in GenericDates in deal status" ++ (show st) ++ "but got"



class SPV a where
  getBondsByName :: a -> Maybe [String] -> Map.Map String L.Bond
  getActiveBonds :: a -> [String] -> [L.Bond]
  getBondBegBal :: a -> String -> Balance
  getBondStmtByName :: a -> Maybe [String] -> Map.Map String (Maybe Statement)
  getFeeByName :: a -> Maybe [String] -> Map.Map String F.Fee
  getAccountByName :: a -> Maybe [String] -> Map.Map String A.Account
  isResec :: a -> Bool
  getNextBondPayDate :: a -> Date
  getOustandingBal :: a -> Balance


type BalDealStatMap = Map.Map DealStatFields Balance
type RDealStatMap = Map.Map DealStatFields Rate
type BDealStatMap = Map.Map DealStatFields Bool
type IDealStatMap = Map.Map DealStatFields Int

data TestDeal a = TestDeal { name :: DealName
                            ,status :: DealStatus
                            ,dates :: DateDesp
                            ,accounts :: Map.Map AccountName A.Account
                            ,fees :: Map.Map FeeName F.Fee
                            ,bonds :: Map.Map BondName L.Bond
                            ,pool ::  PoolType a 
                            ,waterfall :: Map.Map W.ActionWhen W.DistributionSeq
                            ,collects :: [CollectionRule]
                            ,stats :: (BalDealStatMap,RDealStatMap,BDealStatMap,IDealStatMap)
                            ,liqProvider :: Maybe (Map.Map String CE.LiqFacility)
                            ,rateSwap :: Maybe (Map.Map String HE.RateSwap)
                            ,rateCap :: Maybe (Map.Map String HE.RateCap)
                            ,currencySwap :: Maybe (Map.Map String HE.CurrencySwap)
                            ,custom:: Maybe (Map.Map String CustomDataType)
                            ,triggers :: Maybe (Map.Map DealCycle (Map.Map String Trigger))
                            ,ledgers :: Maybe (Map.Map String LD.Ledger)
                            } deriving (Show,Generic,Eq,Ord)

data UnderlyingDeal a = UnderlyingDeal {
  deal :: TestDeal a
  ,futureCf :: Maybe CF.CashFlowFrame
  ,futureScheduleCf :: Maybe CF.CashFlowFrame
  ,issuanceStat :: Maybe (Map.Map CutoffFields Balance)
} deriving (Generic,Eq,Ord,Show)

uDealFutureScheduleCf :: Ast.Asset a => Lens' (UnderlyingDeal a) (Maybe CF.CashFlowFrame)
uDealFutureScheduleCf = lens getter setter
  where 
    getter = futureScheduleCf
    setter ud newCf = ud {futureScheduleCf = newCf}

uDealFutureCf :: Ast.Asset a => Lens' (UnderlyingDeal a) (Maybe CF.CashFlowFrame)
uDealFutureCf = lens getter setter
  where 
    getter = futureCf
    setter ud newCf = ud {futureCf = newCf}

data PoolType a = MultiPool (Map.Map PoolId (P.Pool a))
                | ResecDeal (Map.Map PoolId (UnderlyingDeal a))
                deriving (Generic, Eq, Ord, Show)

makePrisms ''PoolType


instance SPV (TestDeal a) where
  getBondsByName t bns
    = case bns of
        Nothing -> bonds t
        Just _bns -> Map.filterWithKey (\k _ -> S.member k (S.fromList _bns)) (bonds t)
  
  getActiveBonds t bns = 
    let 
      bnds = (bonds t Map.!) <$> bns
    in 
      filter (not . L.isPaidOff) bnds

  getBondStmtByName t bns
    = Map.map L.bndStmt bndsM
      where
      bndsM = Map.map L.consolStmt $ getBondsByName t bns

  getNextBondPayDate t
    = case populateDealDates (dates t) (status t) of
        Right _dates -> view _3 _dates 
        Left _ -> error "Failed to populate dates"

  getBondBegBal t bn 
    = case b of 
        Nothing -> 0
        Just bnd ->
          case L.bndStmt bnd of
            Nothing -> L.getCurBalance bnd  
            Just (Statement txns) 
              | DL.empty == txns  -> L.getCurBalance bnd  
              | otherwise -> getTxnBegBalance $ head (DL.toList txns)
      where
          b = find (\x -> ((L.bndName x) == bn)) (viewDealAllBonds t) 

  getFeeByName t fns
    = case fns of
         Nothing -> fees t
         Just _fns -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _fns)) (fees t)
  
  getAccountByName t ans
    = case ans of
         Nothing -> accounts t
         Just _ans -> Map.filterWithKey (\k _ ->  S.member k (S.fromList _ans)) (accounts t)
  
  isResec t = case pool t of
                 ResecDeal _ -> True
                 _ -> False

  getOustandingBal t@TestDeal{ bonds = bndMap, fees= feeMap, liqProvider = mliqMap, rateSwap = rsMap}
   = let 
      bndBal = sum $ getOutstandingAmount <$> Map.elems bndMap
      feeBal = sum $ getOutstandingAmount <$> Map.elems feeMap
      lqBalace m
        | not (Map.null m) = sum $ getOutstandingAmount <$> Map.elems m
        | otherwise = 0
      rsBalance m
        | not (Map.null m) = sum $ getOutstandingAmount <$> Map.elems m
        | otherwise = 0
     in 
      bndBal + feeBal + lqBalace (fromMaybe Map.empty mliqMap) + rsBalance (fromMaybe Map.empty rsMap)
  
isPreClosing :: TestDeal a -> Bool
isPreClosing t@TestDeal{ status = PreClosing _ } = True
isPreClosing _ = False


-- ^ list all bonds and bond groups in list
viewDealAllBonds :: TestDeal a -> [L.Bond]
viewDealAllBonds d = 
    let 
       bs = Map.elems (bonds d)
       view a@(L.Bond {} ) = [a]
       view a@(L.BondGroup bMap _) = Map.elems bMap
       view a@(L.MultiIntBond {}) = [a]
    in 
       concat $ view <$> bs

-- ^ flatten all bonds/bond groups in a map
viewBondsInMap :: TestDeal a -> Map.Map String L.Bond
viewBondsInMap t@TestDeal{ bonds = bndMap }
  = let 
      bnds = viewDealAllBonds t 
      bndNames = L.bndName <$> bnds
    in 
      Map.fromList $ zip bndNames bnds

-- ^ support bond group
viewDealBondsByNames :: Ast.Asset a => TestDeal a -> [BondName] -> [L.Bond]
viewDealBondsByNames _ [] = []
viewDealBondsByNames t@TestDeal{bonds= bndMap } bndNames
  = let 
      -- bonds and bond groups
      bnds = filter (\b -> L.bndName b `elem` bndNames) $ viewDealAllBonds t
      -- bndsFromGrp = $ Map.filter (\L.BondGroup {} -> True)  bndMap
      bndsFromGrp = Map.foldrWithKey
                      (\k (L.BondGroup bMap _) acc -> 
                        if k `elem` bndNames 
                        then 
                          acc ++ Map.elems bMap
                        else 
                          acc)
                      []
                      (view dealBondGroups t )
    in 
      bnds ++ bndsFromGrp

-- ^ find bonds with first match
-- TODO fix it using traversal
findBondByNames :: Map.Map String L.Bond -> [BondName] -> Either String [L.Bond]
findBondByNames bMap bNames
  = let 
      (firstMatch, notMatched) = Map.partitionWithKey (\k _ -> k `elem` bNames) bMap
      remainNames::[String] = bNames \\ Map.keys firstMatch
      listOfBondGrps::[Map.Map String L.Bond] = [ bM |  (bM,_) <-catMaybes $ (preview L._BondGroup) <$> Map.elems notMatched ]
      (secondMatch, notMatched2) = Map.partitionWithKey (\k _ -> k `elem` remainNames) $ Map.unions listOfBondGrps
    in 
      if Map.null notMatched2 then 
        Right $ Map.elems firstMatch ++ Map.elems secondMatch
      else
        Left $ "Failed to find bonds by names:"++ show (Map.keys notMatched2)

-- ^ not support bond group
dealBonds :: Ast.Asset a => Lens' (TestDeal a) (Map.Map BondName L.Bond)
dealBonds = lens getter setter 
  where 
    getter d = bonds d 
    setter d newBndMap = d {bonds = newBndMap}

-- ^ get & set bond group only
dealBondGroups :: Ast.Asset a => Lens' (TestDeal a) (Map.Map BondName L.Bond)
dealBondGroups = lens getter setter 
  where 
    getter d = Map.filter (has L._BondGroup) (bonds d)
    setter d newBndMap = d {bonds = Map.filter (has L._BondGroup) newBndMap}

bondGroupsBonds :: Lens' L.Bond (Map.Map BondName L.Bond)
bondGroupsBonds = lens getter setter 
  where 
    getter (L.BondGroup bMap _) = bMap
    getter _ = Map.empty
    setter (L.BondGroup b x) newBMap = L.BondGroup newBMap x
    setter x _ = x

updateBondInMap :: BondName -> (L.Bond -> L.Bond) -> Map.Map BondName L.Bond ->  Map.Map BondName L.Bond
updateBondInMap bName f bMap 
  = let 
      fn _bName (L.BondGroup subMap bt) = L.BondGroup (Map.adjust f _bName subMap) bt
      fn _bName bnd 
        | _bName == bName = f bnd
        | otherwise = bnd
    in 
      Map.mapWithKey fn bMap

dealAccounts :: Ast.Asset a => Lens' (TestDeal a) (Map.Map AccountName A.Account) 
dealAccounts = lens getter setter 
  where 
    getter d = accounts d 
    setter d newAccMap = d {accounts = newAccMap}

dealFees :: Ast.Asset a => Lens' (TestDeal a) (Map.Map FeeName F.Fee) 
dealFees = lens getter setter 
  where 
    getter d = fees d 
    setter d newFeeMap = d {fees = newFeeMap}

dealPool :: Ast.Asset a => Lens' (TestDeal a) (PoolType a)
dealPool = lens getter setter 
  where 
    getter d = pool d
    setter d newPool = d {pool = newPool}

poolTypePool :: Ast.Asset a => Lens' (PoolType a) (Map.Map PoolId (P.Pool a))
poolTypePool = lens getter setter
  where
    getter = \case MultiPool pm -> pm
    setter (MultiPool pm) newPm = MultiPool newPm

poolTypeUnderDeal :: Ast.Asset a => Lens' (PoolType a) (Map.Map PoolId (UnderlyingDeal a))
poolTypeUnderDeal = lens getter setter
  where 
    getter = \case ResecDeal dm -> dm
    setter (ResecDeal dm) newDm = ResecDeal newDm

getPoolIds :: Ast.Asset a => TestDeal a -> [PoolId]
getPoolIds t@TestDeal{pool = pt} 
  = case pt of
      MultiPool pm -> Map.keys pm
      ResecDeal pm -> Map.keys pm

-- ^ to handle with bond group, with flag to good deep if it is a bond group
getBondByName :: Ast.Asset a => TestDeal a -> Bool -> BondName -> Maybe L.Bond
getBondByName t False bName = Map.lookup bName (bonds t)
getBondByName t True bName = 
  let 
    bnds = viewDealAllBonds t
  in 
    find (\b -> L.bndName b == bName) bnds

-- ^ get issuance pool stat from pool map
getIssuanceStats :: Ast.Asset a => TestDeal a  -> Maybe [PoolId] -> Map.Map PoolId (Map.Map CutoffFields Balance)
getIssuanceStats t@TestDeal{pool = pt} mPoolId
  = case pt of
      ResecDeal uDeals -> 
        let 
          selecteduDeals = case mPoolId of 
                            Nothing -> uDeals
                            Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) uDeals
        in
          Map.map (fromMaybe Map.empty . issuanceStat) selecteduDeals 
      MultiPool pm -> let 
                        selectedPools = case mPoolId of 
                                          Nothing -> pm
                                          Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) pm
                      in
                        Map.map (fromMaybe Map.empty . P.issuanceStat) selectedPools

getIssuanceStatsConsol :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map CutoffFields Balance
getIssuanceStatsConsol t mPns 
  = let 
      ms = getIssuanceStats t mPns
    in 
      Map.unionsWith (+) $ Map.elems ms

getAllAsset :: TestDeal a -> Maybe [PoolId] -> Map.Map PoolId [a]
getAllAsset t@TestDeal{pool = pt} mPns = 
  let 
    assetMap = case pt of 
                 MultiPool pm -> Map.map P.assets pm
                 ResecDeal _ -> Map.empty
                 -- ResecDeal pm -> Map.mapWithKey (\(UnderlyingBond (bn,hpct,sd), d) -> getAllAsset d Nothing) pm
  in
    case mPns of 
      Nothing -> assetMap 
      Just pns -> Map.filterWithKey (\k _ -> k `elem` pns ) assetMap
    
getAllAssetList :: Ast.Asset a => TestDeal a -> [a]
getAllAssetList t = concat $ Map.elems (getAllAsset t Nothing)

getAllCollectedFrame :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId CF.CashFlowFrame
getAllCollectedFrame t@TestDeal{pool = poolType} mPid = 
  let 
    mCf = case poolType of 
            MultiPool pm -> Map.map (view (P.poolFutureCf . _Just . _1 )) pm -- `debug` ("MultiPool" ++ show pm)
            ResecDeal uds -> Map.map (fromMaybe CF.emptyCashflow . futureCf) uds
  in 
    case mPid of 
      Nothing -> mCf  -- `debug` ("Nothing when collecting cfs"++show mCf)
      Just pids -> Map.filterWithKey (\k _ -> k `elem` pids) mCf -- `debug` ("Just when collecting cfs"++show mCf)

getLatestCollectFrame :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId (Maybe CF.TsRow)
getLatestCollectFrame t mPns = Map.map (\case
                                          (CF.CashFlowFrame (_,_,_) []) -> Nothing
                                          (CF.CashFlowFrame (_,_,_) txns) -> Just $ last txns
                                          )
                                        (getAllCollectedFrame t mPns)

getAllCollectedTxns :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> Map.Map PoolId [CF.TsRow]
getAllCollectedTxns t mPns = Map.map (view CF.cashflowTxn) (getAllCollectedFrame t mPns)

getAllCollectedTxnsList :: Ast.Asset a => TestDeal a -> Maybe [PoolId] -> [CF.TsRow]
getAllCollectedTxnsList t mPns 
  = concat listOfTxns
    where 
      listOfTxns = Map.elems $ getAllCollectedTxns t mPns

increasePoolCollectedPeriod :: TestDeal a -> TestDeal a
increasePoolCollectedPeriod t@TestDeal{stats = (balMap,rateMap,boolMap,intMap)} 
  = let 
      intMap' = Map.insertWith (+) PoolCollectedPeriod 1 intMap
    in 
      t {stats = (balMap,rateMap,boolMap,intMap')}

increaseBondPaidPeriod :: TestDeal a -> TestDeal a
increaseBondPaidPeriod t@TestDeal{stats = (balMap,rateMap,boolMap,intMap)} 
  = let 
      intMap' = Map.insertWith (+) BondPaidPeriod 1 intMap
    in 
      t {stats = (balMap,rateMap,boolMap,intMap')}

getDealStatInt :: TestDeal a -> DealStatFields -> Maybe Int
getDealStatInt t@TestDeal{stats = (balMap,rateMap,boolMap,intMap)} f 
  = Map.lookup f intMap

bondTraversal :: Traversal' (TestDeal a) L.Bond
bondTraversal f t@TestDeal{bonds = bndMap} =
  (\newBndMap -> t {bonds = newBndMap}) <$> traverse f bndMap

data UnderBond b = UnderBond BondName Rate (TestDeal b)

opts :: JSONKeyOptions
opts = defaultJSONKeyOptions

instance ToJSONKey DealStatFields where
  toJSONKey = genericToJSONKey opts
instance FromJSONKey DealStatFields where
  fromJSONKey = genericFromJSONKey opts


$(concat <$> traverse (deriveJSON defaultOptions) [''TestDeal, ''UnderlyingDeal, ''PoolType, ''DateDesp, ''ActionOnDate])
