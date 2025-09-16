{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Assumptions (BondPricingInput(..),IrrType(..)
                    ,AssumptionInput(..),ApplyAssumptionType(..)
                    ,lookupAssumptionByIdx,lookupRate,AssetPerfAssumption(..)
                    ,ExtraStress(..),RevolvingAssumption(..)
                    ,AssetPrepayAssumption(..),AssetDefaultAssumption(..),RecoveryAssumption(..)
                    ,getRateAssumption,projRates,lookupRate0
                    ,LeaseAssetGapAssump(..)
                    ,LeaseAssetRentAssump(..)
                    ,NonPerfAssumption(..),AssetPerf
                    ,AssetDelinquencyAssumption(..)
                    ,AssetDelinqPerfAssumption(..),AssetDefaultedPerfAssumption(..)
                    ,IssueBondEvent(..)
                    ,TagMatchRule(..),ObligorStrategy(..),RefiEvent(..),InspectType(..)
                    ,FieldMatchRule(..),CallOpt(..)
                    ,_MortgageAssump,_MortgageDeqAssump,_LeaseAssump,_LoanAssump,_InstallmentAssump
                    ,_ReceivableAssump,_FixedAssetAssump  
                    ,stressDefaultAssump,applyAssumptionTypeAssetPerf,TradeType(..)
                    ,LeaseEndType(..),LeaseDefaultType(..),stressPrepaymentAssump,StopBy(..)
                    ,readCallOptions
                    ,evalFloaterRate,applyFloatRate ,applyFloatRate2
                    )
where

import Call as C
import Lib (Ts(..),TsPoint(..),toDate,mkRateTs)
import Liability (Bond,InterestInfo(..))
import Util
import DateUtil
import qualified Data.Map as Map 
import Data.List
import qualified Data.Set as Set
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Types
import qualified Data.Time as T
import Data.Fixed
import Data.Ratio
import Revolving
import GHC.Generics
import AssetClass.AssetBase
import Debug.Trace
import qualified InterestRate as IR
import Control.Lens hiding (Index) 

debug = flip trace

type AssetPerf = (AssetPerfAssumption,AssetDelinqPerfAssumption,AssetDefaultedPerfAssumption)
type StratPerfByIdx = ([Int],AssetPerf)

lookupAssumptionByIdx :: [StratPerfByIdx] -> Int -> Either ErrorRep AssetPerf
lookupAssumptionByIdx sbi i
  = case find (\(indxs,_) -> Set.member i  (Set.fromList indxs) ) sbi of
        Just (_, aps ) ->  return aps
        Nothing -> Left ("Lookup assumption by ID: Can't find idx"++ show i ++"in starfication list"++ show sbi)

type ObligorTagStr = String

data TagMatchRule = TagEq                  -- ^ match exactly
                  | TagSubset              -- ^ match subset
                  | TagSuperset            -- ^ match superset
                  | TagAny                 -- ^ match any tag hit
                  | TagNot  TagMatchRule   -- ^ Negative match
                  deriving (Show, Generic, Read)

data FieldMatchRule = FieldIn String [String]
                    | FieldCmp String Cmp Double
                    | FieldInRange String RangeType Double Double
                    | FieldNot FieldMatchRule
                    deriving (Show, Generic, Read)

data ObligorStrategy = ObligorById [String] AssetPerf
                     | ObligorByTag [ObligorTagStr] TagMatchRule AssetPerf
                     | ObligorByField [FieldMatchRule] AssetPerf
                     | ObligorByDefault AssetPerf
                     deriving (Show, Generic, Read)

data ApplyAssumptionType = PoolLevel AssetPerf               -- ^ assumption apply to all assets in the pool
                         | ByIndex [StratPerfByIdx]          -- ^ assumption which only apply to a set of assets in the pool
                         | ByName (Map.Map PoolId AssetPerf) -- ^ assumption for a named pool
                         | ByPoolId (Map.Map PoolId ApplyAssumptionType) -- ^ assumption for a pool
                         | ByObligor [ObligorStrategy]       -- ^ assumption for a set of obligors
                         | ByDealName (Map.Map DealName (ApplyAssumptionType, NonPerfAssumption)) -- ^ assumption for a named deal 
                         deriving (Show, Generic)


applyAssumptionTypeAssetPerf :: Traversal' ApplyAssumptionType AssetPerf
applyAssumptionTypeAssetPerf f = go
  where
    go (PoolLevel x) = PoolLevel <$> f x
    go (ByIndex strats) = ByIndex <$> traverse (\(idxs,aps) -> (idxs,) <$> f aps) strats
    go (ByName m) = ByName <$> traverse f m
    go (ByObligor os) = ByObligor <$> traverse (\case
                                                  ObligorById ids ap -> ObligorById ids <$> f ap
                                                  ObligorByTag tags m ap -> ObligorByTag tags m <$> f ap
                                                  ObligorByField fs ap -> ObligorByField fs <$> f ap
                                                  ObligorByDefault ap -> ObligorByDefault <$> f ap
                                              ) os
    go (ByPoolId m) = ByPoolId <$> traverse go m
    go (ByDealName m) = ByDealName <$> traverse (\(a,b) -> (,) <$> go a <*> pure b) m


type RateFormula = DealStats
type BalanceFormula = DealStats

data IssueBondEvent = IssueBondEvent (Maybe Pre) BondName AccName Bond (Maybe BalanceFormula) (Maybe RateFormula)
                    | FundingBondEvent (Maybe Pre) BondName AccName Balance 
                    deriving (Show, Generic, Read)

data RefiEvent = RefiRate AccountName BondName InterestInfo
               | RefiBond AccountName Bond
               | RefiEvents [RefiEvent]
               deriving (Show, Generic, Read)

data InspectType = InspectPt DatePattern DealStats
                 | InspectRpt DatePattern [DealStats]
                 deriving (Show, Generic, Read)

data CallOpt = LegacyOpts [C.CallOption]                 -- ^ legacy support
             | CallPredicate [Pre]                       -- ^ default test call for each pay day, keep backward compatible
             | CallOnDates DatePattern [Pre]             -- ^ test call at end of day
             deriving (Show, Generic, Read, Ord, Eq)

data StopBy = StopByDate Date		     -- ^ stop by date
	    | StopByPre DatePattern [Pre]    -- ^ stop by precondition
	    deriving (Show, Generic, Read)


data NonPerfAssumption = NonPerfAssumption {
  -- stopRunBy :: Maybe Date                                    -- ^ optional stop day,which will stop cashflow projection
  stopRunBy :: Maybe StopBy                                    -- ^ optional stop day,which will stop cashflow projection
  ,projectedExpense :: Maybe [(FeeName,Ts)]                  -- ^ optional expense projection
  ,callWhen :: Maybe [CallOpt]                               -- ^ optional call options set, once any of these were satisfied, then clean up waterfall is triggered
  ,revolving :: Maybe RevolvingAssumption                    -- ^ optional revolving assumption with revoving assets
  ,interest :: Maybe [RateAssumption]                        -- ^ optional interest rates assumptions
  ,inspectOn :: Maybe [InspectType]                          -- ^ optional tuple list to inspect variables during waterfall run
  ,buildFinancialReport :: Maybe DatePattern                 -- ^ optional dates to build financial reports
  ,pricing :: Maybe BondPricingInput                         -- ^ optional bond pricing input( discount curve etc)
  ,fireTrigger :: Maybe [(Date,DealCycle,String)]            -- ^ optional fire a trigger
  ,makeWholeWhen :: Maybe (Date,Spread,Table Float Spread)
  ,issueBondSchedule :: Maybe [TsPoint IssueBondEvent]                            
  ,refinance :: Maybe [TsPoint RefiEvent]
} deriving (Show, Generic)

data AssumptionInput = Single ApplyAssumptionType  NonPerfAssumption                          -- ^ one assumption request
                     | Multiple (Map.Map String ApplyAssumptionType)  NonPerfAssumption       -- ^ multiple assumption request in a single request
                     deriving (Show,Generic)

data AssetDefaultAssumption = DefaultConstant Rate              -- ^ using constant default rate
                            | DefaultCDR Rate                   -- ^ using annualized default rate
                            | DefaultVec [Rate]                 -- ^ using default rate vector
                            | DefaultVecPadding [Rate]          -- ^ using default rate vector, but padding with last rate till end
                            | DefaultByAmt (Balance,[Rate])
                            | DefaultAtEnd                      -- ^ default 100% at end
                            | DefaultAtEndByRate Rate Rate      -- ^ life time default rate and default rate at end
                            | DefaultStressByTs Ts AssetDefaultAssumption
                            | DefaultByTerm [[Rate]]
                            deriving (Show,Generic,Read)

-- ^ stress the default assumption by a factor
stressDefaultAssump :: Rate -> AssetDefaultAssumption -> AssetDefaultAssumption
stressDefaultAssump x (DefaultConstant r) = DefaultConstant $ min 1.0 (r*x)
stressDefaultAssump x (DefaultCDR r) = DefaultCDR $ min 1.0 (r*x)
stressDefaultAssump x (DefaultVec rs) = DefaultVec $ capWith 1.0 ((x*) <$> rs)
stressDefaultAssump x (DefaultVecPadding rs) = DefaultVecPadding $ capWith 1.0 ((x*) <$> rs)
stressDefaultAssump x (DefaultByAmt (b,rs)) = DefaultByAmt (max (mulBR b x) 0.0, rs)
stressDefaultAssump x (DefaultAtEndByRate r1 r2) = DefaultAtEndByRate (min 1.0 (r1*x)) (min 1.0 (r2*x))
stressDefaultAssump x (DefaultStressByTs ts a) = DefaultStressByTs ts (stressDefaultAssump x a)
stressDefaultAssump x (DefaultByTerm rss) = DefaultByTerm $ ((capWith 1.0) <$> (map (map (* x)) rss))

stressPrepaymentAssump :: Rate -> AssetPrepayAssumption -> AssetPrepayAssumption
stressPrepaymentAssump x (PrepaymentConstant r) = PrepaymentConstant $ min 1.0 (r*x)
stressPrepaymentAssump x (PrepaymentCPR r) = PrepaymentCPR $ min 1.0 (r*x)
stressPrepaymentAssump x (PrepaymentVec rs) = PrepaymentVec $ capWith 1.0 ((x*) <$> rs)
stressPrepaymentAssump x (PrepaymentVecPadding rs) = PrepaymentVecPadding $ capWith 1.0 ((x*) <$> rs)
stressPrepaymentAssump x (PrepayByAmt (b,rs)) = PrepayByAmt (max (mulBR b x) 0, rs)
stressPrepaymentAssump x (PrepayStressByTs ts a) = PrepayStressByTs ts (stressPrepaymentAssump x a)
stressPrepaymentAssump x (PrepaymentPSA r) = PrepaymentPSA $ min 1.0 (r*x)
stressPrepaymentAssump x (PrepaymentByTerm rss) = PrepaymentByTerm $ (capWith 1.0 <$> (map (map (* x)) rss))


data AssetPrepayAssumption = PrepaymentConstant Rate
                           | PrepaymentCPR Rate 
                           | PrepaymentVec [Rate] 
                           | PrepaymentVecPadding [Rate] 
                           | PrepayByAmt (Balance,[Rate])
                           | PrepayStressByTs Ts AssetPrepayAssumption
                           | PrepaymentPSA Rate
                           | PrepaymentByTerm [[Rate]]
                           deriving (Show,Generic,Read)

data AssetDelinquencyAssumption = DelinqCDR Rate (Lag,Rate)                 -- ^ Annualized Rate to Delinq status , period lag become defaulted, loss rate, period lag become loss
                                | DelinqByAmt (Balance,[Rate]) (Lag,Rate)
                                | Dummy3
                                deriving (Show,Generic,Read)

data RecoveryAssumption = Recovery (Rate,Int)                    -- ^ recovery rate, recovery lag
                        | RecoveryTiming (Rate,[Rate])           -- ^ recovery rate, with distribution of recoveries
                        | RecoveryByDays Rate [(Int, Rate)]      -- ^ recovery rate, with distribution of recoveries by offset dates
                        deriving (Show,Generic,Read)

data LeaseAssetGapAssump = GapDays Int                           -- ^ days between leases, when creating dummy leases
                         | GapDaysByCurve Ts                     -- ^ days depends on the size of leases, when a default a default days for size greater
                         deriving (Show,Generic,Read)

data LeaseAssetRentAssump = BaseAnnualRate Rate
                          | BaseCurve Ts 
                          | BaseByVec [Rate]
                          deriving (Show,Generic,Read)

data LeaseDefaultType = DefaultByContinuation Rate
                       | DefaultByTermination Rate
                       deriving (Show,Generic,Read)


data LeaseEndType = CutByDate Date 
                  | StopByExtTimes Int 
                  | EarlierOf Date Int
                  | LaterOf Date Int
                  deriving (Show,Generic,Read)

data ExtraStress = ExtraStress {
                     defaultFactors :: Maybe Ts                 -- ^ stress default rate via a time series based factor curve
                     ,prepaymentFactors :: Maybe Ts             -- ^ stress prepayment rate via a time series based factor curve
                     ,poolHairCut :: Maybe [(PoolSource, Rate)] -- ^ haircut on pool income source
                   } deriving (Show,Generic,Read)

type ExtendCashflowDates = DatePattern

data AssetDefaultedPerfAssumption = DefaultedRecovery Rate Int [Rate]
                                  | DummyDefaultAssump
                                  deriving (Show,Generic,Read)

data AssetDelinqPerfAssumption = DummyDelinqAssump
                               deriving (Show,Generic,Read)



data AssetPerfAssumption = MortgageAssump    (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption)  (Maybe ExtraStress)
                         | MortgageDeqAssump (Maybe AssetDelinquencyAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | LeaseAssump       (Maybe LeaseDefaultType) LeaseAssetGapAssump LeaseAssetRentAssump LeaseEndType
                         | LoanAssump        (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | InstallmentAssump (Maybe AssetDefaultAssumption) (Maybe AssetPrepayAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | ReceivableAssump  (Maybe AssetDefaultAssumption) (Maybe RecoveryAssumption) (Maybe ExtraStress)
                         | FixedAssetAssump  Ts Ts (Maybe Int)  -- util rate, price, (Maybe extend periods)
                         deriving (Show,Generic,Read)


data RevolvingAssumption = AvailableAssets RevolvingPool ApplyAssumptionType
                         | AvailableAssetsBy (Map.Map String (RevolvingPool, ApplyAssumptionType))
                         deriving (Show,Generic)

type HistoryCash = [(Date,Amount)]
type CurrentHolding = Balance -- as of the deal date
type PricingDate = Date
type AmountToBuy = Balance


data TradeType = ByCash Balance 
               | ByBalance Balance
               deriving (Show,Generic)

data IrrType = HoldingBond HistoryCash CurrentHolding (Maybe (Date, BondPricingMethod))
              | BuyBond Date BondPricingMethod TradeType (Maybe (Date, BondPricingMethod))
              deriving (Show,Generic)


data BondPricingInput = DiscountCurve PricingDate Ts                               
                      -- ^ PV curve used to discount bond cashflow and a PV date where cashflow discounted to 
                      | RunZSpread Ts (Map.Map BondName (PricingDate,Rational))    
                      -- ^ PV curve as well as bond trading price with a deal used to calc Z - spread
                      | DiscountRate PricingDate Rate
                      -- | OASInput Date BondName Balance [Spread] (Map.Map String Ts)                        -- ^ only works in multiple assumption request 
                      | IrrInput (Map.Map BondName IrrType)        
                      -- ^ IRR calculation for a list of bonds
                      deriving (Show,Generic)


getIndexFromRateAssumption :: RateAssumption -> Index 
getIndexFromRateAssumption (RateCurve idx _) = idx
getIndexFromRateAssumption (RateFlat idx _) = idx

-- ^ lookup rate from rate assumption with index and spread
lookupRate :: [RateAssumption] -> Floater -> Date -> Either String IRate 
lookupRate rAssumps (index,spd) d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> Right $ spd + fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> Right $ r + spd
      Nothing -> Left $ "Failed to find Index " ++ show index ++ "in list "++ show rAssumps

-- ^ lookup rate from rate assumption with index
lookupRate0 :: [RateAssumption] -> Index -> Date -> Either String IRate 
lookupRate0 rAssumps index d
  = case find (\x -> getIndexFromRateAssumption x == index ) rAssumps of 
      Just (RateCurve _ ts) -> Right $ fromRational (getValByDate ts Inc d)
      Just (RateFlat _ r) -> Right r
      Nothing -> Left $ "Failed to find Index " ++ show index ++ " from Rate Assumption" ++ show rAssumps


getRateAssumption :: [RateAssumption] -> Index -> Maybe RateAssumption
getRateAssumption assumps idx
  = find (\case
           (RateCurve _idx _) -> idx == _idx 
           (RateFlat _idx _) -> idx == _idx
           _ -> False)
         assumps

-- | project rates used by rate type ,with interest rate assumptions and observation dates
projRates :: IRate ->IR.RateType -> Maybe [RateAssumption] -> [Date] -> Either String [IRate]
projRates sr _ _ [] = Left "No dates provided for rate projection"
projRates sr (IR.Fix _ r) _ ds = Right $ replicate (length ds) sr 
projRates sr (IR.Floater _ idx spd r dp rfloor rcap mr) Nothing ds = Left $ "Looking up rate error: No rate assumption found for index "++ show idx
projRates sr (IR.Floater _ idx spd r dp rfloor rcap mr) (Just assumps) ds 
  = case getRateAssumption assumps idx of
      Nothing -> Left ("Failed to find index rate " ++ show idx ++ " from "++ show assumps)
      Just _rateAssumption -> 
        Right $
          let 
            resetDates = genSerialDatesTill2 NO_IE (head ds) dp (last ds)
            ratesFromCurve = case _rateAssumption of
                                (RateCurve _ ts) -> (\x -> spd + (fromRational x) ) <$> (getValByDates ts Inc resetDates)
                                (RateFlat _ v)   -> (spd +) <$> replicate (length resetDates) v
            ratesUsedByDates =  getValByDates
                                  (mkRateTs $ zip ((head ds):resetDates) (sr:ratesFromCurve))
                                  Inc
                                  ds 
          in 
            case (rfloor,rcap) of 
              (Nothing, Nothing) -> fromRational <$> ratesUsedByDates  
              (Just fv, Just cv) -> capWith cv $ floorWith fv $ fromRational <$> ratesUsedByDates 
              (Just fv, Nothing) -> floorWith fv $ fromRational <$> ratesUsedByDates 
              (Nothing, Just cv) -> capWith cv $ fromRational <$> ratesUsedByDates 

projRates _ rt rassump ds = Left ("Invalid rate type: "++ show rt++" assump: "++ show rassump)

-- ^ split call option assumption , 
-- lefts are for waterfall payment days
-- rights are for date-based calls
splitCallOpts :: CallOpt -> ([Pre],[Pre])
splitCallOpts (CallPredicate ps) = (ps,[])
splitCallOpts (LegacyOpts copts) = 
    let 
      cFn (C.PoolBalance bal) = If L (CurrentPoolBalance Nothing) bal
      cFn (C.BondBalance bal) = If L CurrentBondBalance bal
      cFn (C.PoolFactor r) = IfRate L (Types.PoolFactor Nothing) (fromRational r)
      cFn (C.BondFactor r) = IfRate L Types.BondFactor (fromRational r)
      cFn (C.OnDate d) = IfDate E d
      cFn (C.AfterDate d) = IfDate G d
      cFn (C.And _opts) = Types.All [ cFn o | o <- _opts  ]
      cFn (C.Or _opts) = Types.Any [ cFn o | o <- _opts  ]
      cFn (C.Pre p) = p
    in 
      ([ cFn copt | copt <- copts ],[])
-- legacyCallOptConvert (AP.CallOptions opts) = concat [ legacyCallOptConvert o | o <- opts ]
splitCallOpts (CallOnDates dp ps) = ([],ps)


readCallOptions :: [CallOpt] -> ([Pre],[Pre])
readCallOptions [] = ([],[])
readCallOptions opts = 
  let 
    result = splitCallOpts <$> opts
  in 
    (concat (fst <$> result), concat (snd <$> result))

evalFloaterRate :: Date -> [RateAssumption] -> IR.RateType -> IRate 
evalFloaterRate _ _ (IR.Fix _ r) = r 
evalFloaterRate d ras (IR.Floater _ idx spd _r _ mFloor mCap mRounding)
  = let 
      ra = getRateAssumption ras idx 
      flooring (Just f) v = max f v 
      flooring Nothing v = v 
      capping (Just f) v = min f v 
      capping Nothing  v = v 
    in 
      case ra of 
        Nothing -> error "Failed to find index rate in assumption"
        Just (RateFlat _ v) -> capping mCap $ flooring mFloor $ v + spd 
        Just (RateCurve _ curve) -> capping mCap $ flooring mFloor $ fromRational $ getValByDate curve Inc d + toRational spd

applyFloatRate :: InterestInfo -> Date -> [RateAssumption] -> IRate
applyFloatRate (Liability.Floater _ idx spd p dc mf mc) d ras
  = case (mf,mc) of
      (Nothing,Nothing) -> _rate
      (Just f,Nothing) -> max f _rate
      (Just f,Just c) -> min c $ max f _rate
      (Nothing,Just c) -> min c _rate
    where
      idx_rate = case ra of 
        Just (RateCurve _idx _ts) -> fromRational $ getValByDate _ts Exc d
        Just (RateFlat _idx _r) ->   _r
        Nothing -> 0.0
      ra = getRateAssumption ras idx
      _rate = idx_rate + spd -- `debug` ("idx"++show idx_rate++"spd"++show spd)

applyFloatRate (Liability.CapRate ii _rate) d ras = min _rate (applyFloatRate ii d ras)
applyFloatRate (Liability.FloorRate ii _rate) d ras = max _rate (applyFloatRate ii d ras)
applyFloatRate (Liability.Fix r _ ) d ras = r
applyFloatRate (Liability.WithIoI ii _) d ras = applyFloatRate ii d ras

applyFloatRate2 :: IR.RateType -> Date -> [RateAssumption] -> Either String IRate
applyFloatRate2 (IR.Fix _ r) _ _ = Right r
applyFloatRate2 (IR.Floater _ idx spd _r _ mFloor mCap mRounding) d ras
  = let 
      flooring (Just f) v = max f v 
      flooring Nothing v = v 
      capping (Just f) v = min f v 
      capping Nothing  v = v 
    in 
      do 
        rateAtDate <- lookupRate0 ras idx d 
        return $ flooring mFloor $ capping mCap $ rateAtDate + spd


-- ^ Given a list of rates, calcualte whether rates was reset

makePrisms ''AssetPerfAssumption 
makePrisms ''AssetDefaultAssumption

$(deriveJSON defaultOptions ''CallOpt)
$(deriveJSON defaultOptions ''TradeType)
$(deriveJSON defaultOptions ''IrrType)
$(deriveJSON defaultOptions ''BondPricingInput)
$(deriveJSON defaultOptions ''IssueBondEvent)
$(deriveJSON defaultOptions ''RefiEvent)



$(concat <$> traverse (deriveJSON defaultOptions) [''LeaseDefaultType, ''LeaseEndType,''FieldMatchRule,''TagMatchRule, ''ObligorStrategy,''ApplyAssumptionType, ''AssetPerfAssumption, ''StopBy
  , ''AssetDefaultedPerfAssumption, ''AssetDelinqPerfAssumption, ''NonPerfAssumption, ''AssetDefaultAssumption
  , ''AssetPrepayAssumption, ''RecoveryAssumption, ''ExtraStress
  , ''LeaseAssetGapAssump, ''LeaseAssetRentAssump, ''RevolvingAssumption, ''AssetDelinquencyAssumption,''InspectType])
