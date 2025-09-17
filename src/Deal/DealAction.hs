{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deal.DealAction (performActionWrap,performAction,calcDueFee
                       ,testTrigger,RunContext(..),updateLiqProvider
                       ,calcDueInt,priceAssetUnion
                       ,priceAssetUnionList,inspectVars,inspectListVars,accrueRC,accrueDeal) 
  where

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Pool as P
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified AssetClass.AssetBase as ACM
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan
import AssetClass.Installment
import AssetClass.MixedAsset

import qualified Call as C
import qualified InterestRate as IR
import qualified Analytics as AN

import Deal.DealBase
import Deal.DealQuery
import Deal.DealDate

import Stmt
import Lib
import Util
import DateUtil
import Types
import Revolving
import Triggers

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Control.Lens as LS
import Data.List
import qualified Data.DList as DL
import Data.Fixed
import Data.Time.Clock
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Applicative
import Debug.Trace
import Cashflow (CashFlowFrame(CashFlowFrame))
import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Lens.Extras (is)
import Control.Monad

debug = flip trace

-- ^ Test triggers
testTrigger :: Ast.Asset a => TestDeal a -> Date -> Trigger -> Either ErrorRep Trigger
testTrigger t d trigger@Trigger{trgStatus=st,trgCurable=curable,trgCondition=cond,trgStmt = tStmt} 
  | not curable && st = return trigger
  | otherwise = let 
                  (memo, newStM) = testPre2 d t cond
                in 
                  do 
                    newSt <- newStM
                    return trigger { trgStatus = newSt
                                    , trgStmt = Stmt.appendStmt (TrgTxn d newSt (Stmt.Tag memo)) tStmt }


-- actual payout amount to bond with due mounts
allocAmtToBonds :: W.PayOrderBy -> Amount -> [(L.Bond,Amount)] -> [(L.Bond,Amount)]
allocAmtToBonds W.ByProRataCurBal amt bndsWithDue 
  = zip (fst <$> bndsWithDue) $ prorataFactors (snd <$> bndsWithDue) amt 
allocAmtToBonds theOrder amt bndsWithDue =
  let 
    sortFn W.ByName = (\(b1,_) (b2,_) -> compare (L.bndName b1) (L.bndName b2)) 
    sortFn W.ByCurrentRate = (\(b1,_) (b2,_) -> compare (L.bndRate b2) (L.bndRate b1)) 
    sortFn W.ByMaturity = (\(b1@L.Bond{L.bndOriginInfo=bo1},_) (b2@L.Bond{L.bndOriginInfo=bo2},_) -> compare (L.maturityDate bo1) (L.maturityDate bo2))
    sortFn W.ByStartDate = (\(b1@L.Bond{L.bndOriginInfo=bo1},_) (b2@L.Bond{L.bndOriginInfo=bo2},_) -> compare (L.originDate bo1) (L.originDate bo2))
    sortFn (W.ByCustomNames names) = (\(b1,_) (b2,_) -> compare (elemIndex (L.bndName b1) names) (elemIndex (L.bndName b2) names))
    sortFn (W.ReverseSeq orderBy) = flip (sortFn orderBy)
    orderedBonds = sortBy (sortFn theOrder) bndsWithDue
    orderedAmt = snd <$> orderedBonds
  in 
    zip 
      (fst <$> orderedBonds)
      $ paySeqLiabilitiesAmt amt orderedAmt


calcDueFee :: Ast.Asset a => TestDeal a -> Date -> F.Fee -> Either ErrorRep F.Fee

-- ^ one-off fee, can be accrued multiple times
calcDueFee t calcDay f@(F.Fee fn (F.FixFee amt) fs fd fdDay fa _ _)
  | isJust fdDay = return f  -- Nothing change if it has been calculated before
  | calcDay >= fs && isNothing fdDay = return $ f { F.feeDue = amt, F.feeDueDate = Just calcDay} 
  | otherwise = return f

-- ^ annualised fee: patch start date as last fee due date
calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase r) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = return f

-- ^ annualized % fee base on pool balance amount
calcDueFee t calcDay f@(F.Fee fn (F.AnnualRateFee feeBase _r) fs fd (Just fdDay) fa lpd _)
  = let 
      patchedDs = patchDatesToStats t fdDay calcDay feeBase
    in 
      do
        r <- queryCompound t calcDay _r 
        baseBal <- queryCompound t calcDay patchedDs
        let newDue = baseBal * r 
        return f { F.feeDue= fd + fromRational newDue, F.feeDueDate = Just calcDay }

-- ^ percentage fee base on a formula rate
calcDueFee t calcDay f@(F.Fee fn (F.PctFee ds _r ) fs fd fdDay fa lpd _)
  | calcDay <= fs = return f
  | otherwise 
    = do
        r <-  queryCompound t calcDay _r
        baseBal <- queryCompound t calcDay (patchDateToStats calcDay ds)
        return f { F.feeDue = fd + fromRational (baseBal * r), F.feeDueDate = Just calcDay }

-- ^ time series based fee, can be accrued multiple times
calcDueFee t calcDay f@(F.Fee fn (F.FeeFlow ts)  fs fd _ fa mflpd _)
  = let 
      (currentNewDue,futureDue) = splitTsByDate ts calcDay 
      cumulativeDue = sumValTs currentNewDue
      newFeeDue =  cumulativeDue + fd  
    in 
      return f{ F.feeDue = newFeeDue ,F.feeDueDate = Just calcDay ,F.feeType = F.FeeFlow futureDue}

-- ^ fee based on a recurring date pattern, exempt by reAccruableFeeType check
calcDueFee t calcDay f@(F.Fee fn (F.RecurFee p amt)  fs fd mLastAccDate fa _ _)
  | periodGaps == 0 = return f 
  | otherwise = return f { F.feeDue = amt * fromIntegral periodGaps + fd
                        , F.feeDueDate = Just (T.addDays 1 calcDay) }
  where
    accDates = case mLastAccDate of 
                      Nothing -> genSerialDatesTill2 NO_IE (T.addDays 1 fs) p calcDay 
                      Just lastAccDate -> genSerialDatesTill2 NO_IE lastAccDate p calcDay 
    periodGaps = length accDates 

-- ^ fee based on an integer number, exempt by reAccruableFeeType check
calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd Nothing fa lpd _)
  | calcDay >= fs = calcDueFee t calcDay f {F.feeDueDate = Just fs }
  | otherwise = return f 

-- ^ fee based on an integer number, exempt by reAccruableFeeType check
calcDueFee t calcDay f@(F.Fee fn (F.NumFee p s amt) fs fd (Just fdDay) fa lpd _)
  | fdDay == calcDay = return f 
  | periodGap == 0 = return f 
  | otherwise = do 
                  baseCount <- queryCompound t calcDay (patchDateToStats calcDay s)
                  let newFeeDueAmt = (fromRational baseCount) * amt * fromIntegral periodGap -- `debug` ("amt"++show amt++">>"++show baseCount++">>"++show periodGap)
                  return f { F.feeDue = fd+newFeeDueAmt , F.feeDueDate = Just calcDay } 
  where 
    dueDates = projDatesByPattern p fdDay (pred calcDay)
    periodGap = length dueDates  -- `debug` ("Due Dates"++ show dueDates)

-- ^ fee based on target balance difference
calcDueFee t calcDay f@(F.Fee fn (F.TargetBalanceFee dsDue dsPaid) fs fd _ fa lpd _)
  = do 
      let dsDueD = patchDateToStats calcDay dsDue 
      let dsPaidD = patchDateToStats calcDay dsPaid
      dueAmt <- max 0 <$> (liftA2) (-) (queryCompound t calcDay dsDueD) (queryCompound t calcDay dsPaidD)
      return f { F.feeDue = fromRational dueAmt, F.feeDueDate = Just calcDay} 

-- ^ fee based on a collection period
calcDueFee t@TestDeal{ pool = pool } calcDay f@(F.Fee fn (F.ByCollectPeriod amt) fs fd fdday fa lpd _)
  = let  
      txnsDates = getDate <$> getAllCollectedTxnsList t (Just [PoolConsol])
      pastPeriods = case fdday of 
                      Nothing ->  subDates II fs calcDay txnsDates
                      Just lastFeeDueDay -> subDates EI lastFeeDueDay calcDay txnsDates
      dueAmt = fromRational $ mulBInt amt (length pastPeriods)
    in 
      return $ f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}

-- ^ fee based on a table lookup, exempt by reAccruableFeeType check
calcDueFee t calcDay f@(F.Fee fn (F.AmtByTbl _ ds tbl) fs fd Nothing fa lpd _)
  = calcDueFee t calcDay f {F.feeDueDate = Just fs }

calcDueFee t calcDay f@(F.Fee fn (F.AmtByTbl _ ds tbl) fs fd (Just fdday) fa lpd _)
  | fdday == calcDay = return f
  | otherwise = 
      do
        lookupVal <- queryCompound t calcDay (patchDateToStats calcDay ds)
        let dueAmt = fromMaybe 0.0 $ lookupTable tbl Up (fromRational lookupVal >=)
        return f {F.feeDue = dueAmt + fd, F.feeDueDate = Just calcDay}

-- ^ fee based on a pool period number
calcDueFee t calcDay f@(F.Fee fn (F.FeeFlowByPoolPeriod pc) fs fd fdday fa lpd stmt)
  = do 
      currentPoolPeriod <- queryCompound t calcDay (DealStatInt PoolCollectedPeriod)
      feePaidAmt <- queryCompound t calcDay (FeePaidAmt [fn])
      let dueAmt = fromMaybe 0 $ getValFromPerCurve pc Past Inc (succ (floor (fromRational currentPoolPeriod)))
      return f {F.feeDue = max 0 (dueAmt - fromRational feePaidAmt) + fd, F.feeDueDate = Just calcDay}

-- ^ fee based on a bond period number
calcDueFee t calcDay f@(F.Fee fn (F.FeeFlowByBondPeriod pc) fs fd fdday fa lpd stmt)
  = do 
      currentBondPeriod <- queryCompound t calcDay (DealStatInt BondPaidPeriod)
      feePaidAmt <- queryCompound t calcDay (FeePaidAmt [fn])
      let dueAmt = fromMaybe 0 $ getValFromPerCurve pc Past Inc (succ (floor (fromRational currentBondPeriod)))
      return f {F.feeDue = max 0 (dueAmt - fromRational feePaidAmt) + fd, F.feeDueDate = Just calcDay} 


disableLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Just endDate } 
  | d > endDate = liq{CE.liqCredit = ByAvailAmount 0}
  | otherwise = liq
disableLiqProvider _ d liq@CE.LiqFacility{CE.liqEnds = Nothing }  = liq


-- refresh available balance
---- for Replenish Support and ByPct
updateLiqProvider :: Ast.Asset a => TestDeal a -> Date -> CE.LiqFacility -> CE.LiqFacility
updateLiqProvider t d liq@CE.LiqFacility{CE.liqType = liqType, CE.liqCredit = curCredit}
  = disableLiqProvider t d $ liq { CE.liqCredit = newCredit } 
    where 
      -- TODO ,need to remove due int and due fee
      newCredit = case liqType of 
                    --  CE.ReplenishSupport _ b -> max b <$> curCredit
                    CE.ByPct ds _r ->  case (* _r) <$> (queryCompound t d (patchDateToStats d ds)) of
                                          Left y -> error "Shouldn't happen"
                                          Right x -> updateSupportAvailType (min (fromRational x)) curCredit
                    _ -> curCredit

-- ^TODO : to be replace from L.accrueInt
-- Not possible to use L.accrueInt, since the interest may use formula to query on deal's stats
calcDueInt :: Ast.Asset a => TestDeal a -> Date -> L.Bond -> Either ErrorRep L.Bond
calcDueInt t d b@(L.BondGroup bMap pt) 
  = do 
      m <- mapM (calcDueInt t d) bMap 
      return $ L.BondGroup m pt

-- first time to accrue interest\
-- use default date to start to accrue
calcDueInt t@TestDeal{ status = st, dates = dealDates} d b@(L.Bond bn bt oi io _ bal r dp _ di Nothing _ _ _ ) 
  | bal+di == 0 && (bt /= L.IO) = return b
  | otherwise = 
        do 
          sd <- getClosingDate dealDates
          b' <- calcDueInt t d (b {L.bndDueIntDate = Just sd }) 
          return b'

-- Interest Only Bond with Reference Balance
calcDueInt t d b@(L.Bond _ L.IO oi (L.RefBal refBal ii) _ bal r dp dInt dioi (Just lastIntDueDay) _ _ _ ) 
  = do 
      balUsed <- queryCompound t d refBal -- `debug`  ("Hit acc int"++show d ++" bond name"++ L.bndName b)
      let newDueInt = IR.calcInt (fromRational balUsed) lastIntDueDay d r 
                        (fromMaybe DC_ACT_365F (L.getDayCountFromInfo ii)) -- `debug` ("Balused" ++ show (fromRational balUsed) ++ "lastIntDueDay"++show lastIntDueDay ++ "d"++show d ++ "r"++show r)
      return b { L.bndDueInt = newDueInt + dInt, L.bndDueIntDate = Just d }

-- Z bond
calcDueInt t d b@(L.Bond bn L.Z bo bi _ bond_bal bond_rate _ _ _ _ lstIntPay _ _) 
  = return $ b {L.bndDueInt = 0 }

-- Won't accrue interest for Equity bond
calcDueInt t d b@(L.Bond _ L.Equity _ _ _ _ _ _ _ _ _ _ _ _)
  = return b 

-- accrued with interest over interest
calcDueInt t d b@(L.Bond bn bt bo (L.WithIoI intInfo ioiIntInfo) _ bond_bal bond_rate _ intDue ioiIntDue (Just int_due_date) lstIntPay _ _ )
  = 
    let
      ioiRate = case ioiIntInfo of 
                  L.OverCurrRateBy factor -> bond_rate * fromRational (1+factor)
                  L.OverFixSpread spd -> bond_rate + spd
      newIoiInt = IR.calcInt intDue int_due_date d ioiRate DC_ACT_365F
      ioiInt = newIoiInt + ioiIntDue -- add ioi int due with new accrued ioi int
      newBond = b { L.bndDueIntOverInt = ioiInt, L.bndInterestInfo = intInfo }
    in 
      do 
        newBondWithIntInfo <- calcDueInt t d newBond
        return newBondWithIntInfo { L.bndInterestInfo = L.WithIoI intInfo ioiIntInfo}

-- TODO: to enable override rate & balance
-- accure interest by rate
calcDueInt t d b@(L.MultiIntBond {}) = return $ L.accrueInt d b

calcDueInt t d b@(L.Bond {}) = return $ L.accrueInt d b


-- ^ modify due principal for bond
calcDuePrin :: Ast.Asset a => TestDeal a -> Date -> L.Bond -> Either ErrorRep L.Bond
calcDuePrin t d b@(L.BondGroup bMap pt) 
  = do 
      m <- sequenceA $ Map.map (calcDuePrin t d) bMap
      return $ L.BondGroup m pt

calcDuePrin t d b =
  let 
    bondBal = L.bndBalance b
  in 
    do
      tBal <- calcBondTargetBalance t d b
      return $ b {L.bndDuePrin = max 0 (bondBal - tBal) }

-- ^ accure rate cap 
accrueRC :: Ast.Asset a => TestDeal a -> Date -> [RateAssumption] -> HE.RateCap -> Either ErrorRep HE.RateCap
accrueRC t d rs rc@HE.RateCap{HE.rcNetCash = amt, HE.rcStrikeRate = strike,HE.rcIndex = index
                        ,HE.rcStartDate = sd, HE.rcEndDate = ed, HE.rcNotional = notional
                        ,HE.rcLastStlDate = mlsd
                        ,HE.rcStmt = mstmt} 
  | d > ed || d < sd = return rc 
  | otherwise = do
                  r <- AP.lookupRate0 rs index d
                  balance <- case notional of
                               HE.Fixed bal -> Right . toRational $ bal
                               HE.Base ds -> queryCompound t d (patchDateToStats d ds)
                               HE.Schedule ts -> return $ getValByDate ts Inc d

                  let accRate = max 0 $ r - fromRational (getValByDate strike Inc d) -- `debug` ("Rate from curve"++show (getValByDate strike Inc d))
                  let addAmt = case mlsd of 
                                 Nothing -> IR.calcInt (fromRational balance) sd d accRate DC_ACT_365F
                                 Just lstD -> IR.calcInt (fromRational balance) lstD d accRate DC_ACT_365F

                  let newAmt = amt + addAmt  -- `debug` ("Accrue AMT"++ show addAmt)
                  let newStmt = appendStmt (IrsTxn d newAmt addAmt 0 0 0 SwapAccrue) mstmt 
                  return $ rc { HE.rcLastStlDate = Just d ,HE.rcNetCash = newAmt, HE.rcStmt = newStmt }


-- ^ accrue all liabilities of deal to date d
accrueDeal :: Ast.Asset a => Date -> [RateAssumption] -> TestDeal a -> Either ErrorRep (TestDeal a)
accrueDeal d ras t@TestDeal{fees = feeMap, bonds = bondMap, liqProvider = liqMap
                            , rateSwap = rsMap, rateCap = rcMap, accounts = accMap}
  = let
      liqMap' = (Map.map (CE.accrueLiqProvider d)) <$> liqMap
      rsMap' = (Map.map (HE.accrueIRS d)) <$> rsMap
    in 
      do
        bondMap' <- sequenceA (Map.map (calcDueInt t d) bondMap) 
        feeMap' <- sequenceA $ Map.map (\v -> if (F.reAccruableFeeType (F.feeType v)) then calcDueFee t d v else pure v) feeMap
        rcMap' <- traverse (Map.traverseWithKey (\_ -> accrueRC t d ras)) rcMap
        return t { fees = feeMap' ,
                   bonds = bondMap',
                   liqProvider = liqMap',
                   rateSwap = rsMap',
                   rateCap = rcMap'
                   }



priceAssetUnion :: ACM.AssetUnion -> Date -> PricingMethod  -> AP.AssetPerf -> Maybe [RateAssumption] 
                -> Either ErrorRep PriceResult
priceAssetUnion (ACM.MO m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.LO m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.IL m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.LS m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc 
priceAssetUnion (ACM.RE m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.PF m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc
priceAssetUnion (ACM.FA m) d pm aps mras = Ast.priceAsset m d pm aps mras Inc

priceAssetUnionList :: [ACM.AssetUnion] -> Date -> PricingMethod  -> AP.ApplyAssumptionType -> Maybe [RateAssumption] 
                    -> Either ErrorRep [PriceResult]
priceAssetUnionList assetList d pm (AP.PoolLevel assetPerf) mRates 
  = sequenceA [ priceAssetUnion asset d pm assetPerf mRates | asset <- assetList ]

-- | this would used in `static` revolving ,which assumes the revolving pool will decrease
splitAssetUnion :: [Rate] -> ACM.AssetUnion -> [ACM.AssetUnion]
splitAssetUnion rs (ACM.MO m) = [ ACM.MO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LO m) = [ ACM.LO a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.IL m) = [ ACM.IL a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.LS m) = [ ACM.LS a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.RE m) = [ ACM.RE a | a <- Ast.splitWith m rs]
splitAssetUnion rs (ACM.FA m) = [ ACM.FA a | a <- Ast.splitWith m rs]

-- ^ return assets bought and pool after bought
buyRevolvingPool :: Date -> Rate -> RevolvingPool -> ([ACM.AssetUnion],RevolvingPool)
buyRevolvingPool _ 0 rp = ([],rp)
buyRevolvingPool _ r rp@(StaticAsset assets) 
  = let 
      splitRatios 
        | r >= 1 = [1.0,0]
        | otherwise = [r,1-r]
      splitedAssets = splitAssetUnion splitRatios <$> assets
      assetBought = head <$> splitedAssets
      assetRemains = last <$> splitedAssets 
    in 
      (assetBought ,StaticAsset assetRemains)

buyRevolvingPool _ r rp@(ConstantAsset assets)
  = let 
      splitedAssets = splitAssetUnion [r,0] <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought ,rp)

buyRevolvingPool d r rp@(AssetCurve aus)
  = let
      splitRatios 
        | r >= 1 = [1.0,0]
        | otherwise = [r,1-r]
      assets = lookupAssetAvailable rp d 
      splitedAssets = splitAssetUnion splitRatios <$> assets
      assetBought = head <$> splitedAssets
    in 
      (assetBought, rp)


data RunContext a = RunContext{
                  runPoolFlow:: Map.Map PoolId CF.PoolCashflow
                  ,revolvingAssump:: Maybe (Map.Map String (RevolvingPool ,AP.ApplyAssumptionType))
                  ,revolvingInterestRateAssump:: Maybe [RateAssumption]
                  }
                  deriving (Show)

updateOriginDate2 :: Date -> ACM.AssetUnion -> ACM.AssetUnion
updateOriginDate2 d (ACM.LO m) = ACM.LO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.MO m) = ACM.MO $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.IL m) = ACM.IL $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.LS m) = ACM.LS $ updateOriginDate m (Ast.calcAlignDate m d)
updateOriginDate2 d (ACM.RE m) = ACM.RE $ updateOriginDate m (Ast.calcAlignDate m d)


                     
sumSupport :: [SupportAvailType] -> SupportAvailType
sumSupport [] = ByAvailAmount 0
sumSupport [ByAvailAmount b1] = ByAvailAmount b1
sumSupport (ByAvailAmount b1 : ByAvailAmount b2 : xs) = sumSupport (ByAvailAmount (b1 + b2) : xs)
sumSupport (Unlimit : xs) = Unlimit
sumSupport x = error $ "sumSupport: unsupported type " ++ show x


-- ^ get available supports in balance
evalExtraSupportBalance :: Ast.Asset a => Date -> TestDeal a -> W.ExtraSupport -> Either ErrorRep SupportAvailType
evalExtraSupportBalance d t (W.WithCondition pre s) 
  = do
      flag <- testPre d t pre
      if flag then 
        evalExtraSupportBalance d t s
      else
        return $ ByAvailAmount 0
evalExtraSupportBalance d t@TestDeal{accounts=accMap} (W.SupportAccount an _) 
  = do 
      acc <- lookupM an accMap
      return $ ByAvailAmount $ A.accBalance acc 
evalExtraSupportBalance d t@TestDeal{liqProvider=Just liqMap} (W.SupportLiqFacility liqName) 
  = do
      support <- lookupM liqName liqMap
      case Map.lookup liqName liqMap of
        Nothing -> Left $ "Liquidity facility not found:" ++ show liqName
        Just liq -> return $ CE.liqCredit liq  
evalExtraSupportBalance d t (W.MultiSupport supports) 
  = sumSupport <$> (sequenceA [ (evalExtraSupportBalance d t sp) | sp <- supports ])


-- ^ draw support from a deal , return updated deal,and remaining oustanding amount
drawExtraSupport :: Date -> Amount -> W.ExtraSupport -> TestDeal a -> Either ErrorRep (TestDeal a, Amount)
-- ^ draw account support and book ledger
drawExtraSupport d amt (W.SupportAccount an (Just (dr, ln))) t@TestDeal{accounts=accMap, ledgers= Just ledgerMap}
  = do 
      acc <- lookupM an accMap
      let drawAmt = min (A.accBalance acc) amt
      let oustandingAmt = amt - drawAmt
      newAccMap <- adjustM (A.draw d drawAmt Types.SupportDraw) an accMap
      return (t {accounts = newAccMap 
                ,ledgers = Just $ Map.adjust (LD.entryLogByDr (dr,drawAmt) d Nothing) ln ledgerMap} 
              , oustandingAmt)

-- ^ draw account support
drawExtraSupport d amt (W.SupportAccount an Nothing) t@TestDeal{accounts=accMap} 
  = do
      acc <- lookupM an accMap
      let drawAmt = min (A.accBalance acc) amt
      let oustandingAmt = amt - drawAmt
      newAccMap <- adjustM (A.draw d drawAmt Types.SupportDraw) an accMap
      return (t {accounts = newAccMap } , oustandingAmt) 

-- ^ draw support from liquidity facility
drawExtraSupport d amt (W.SupportLiqFacility liqName) t@TestDeal{liqProvider= Just liqMap}
  = do
      theLiqProvider <- lookupM liqName liqMap 
      let drawAmt = case CE.liqCredit theLiqProvider of 
                      Unlimit -> amt -- `debug` ("From amt"++ show amt)
                      ByAvailAmount b -> min amt b -- `debug` ("From Just"++ show b++">>"++show amt)
      let oustandingAmt = amt - drawAmt -- `debug` ("Draw Amt"++show drawAmt++">>"++ show amt ++">>>")
      newLiqMap <- adjustM (CE.draw d drawAmt SupportDraw) liqName liqMap
      return (t {liqProvider = Just newLiqMap} , oustandingAmt)

-- ^ draw multiple supports by sequence
drawExtraSupport d amt (W.MultiSupport supports) t
  = foldM
      (\(deal,remainAmt) support -> drawExtraSupport d remainAmt support deal) 
      (t, amt) 
      supports

inspectListVars :: Ast.Asset a => TestDeal a -> Date -> [DealStats] -> Either ErrorRep [ResultComponent]
inspectListVars t d dss = sequenceA [ inspectVars t d ds | ds <- dss]                     

inspectVars :: Ast.Asset a => TestDeal a -> Date -> DealStats -> Either ErrorRep ResultComponent
inspectVars t d ds =                     
  case getDealStatType ds of 
    RtnRate -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectRate d ds $ fromRational q
    RtnBool -> do 
                 q <- queryDealBool t (patchDateToStats d ds) d
                 return $ InspectBool d ds q 
    RtnInt  -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectInt d ds $ round . fromRational $ q
    _       -> do 
                 q <- queryCompound t d (patchDateToStats d ds)
                 return $ InspectBal d ds $ fromRational q 

showInspection :: ResultComponent -> String
showInspection (InspectRate d ds r) = show r
showInspection (InspectBool d ds r) = show r
showInspection (InspectInt d ds r) = show r
showInspection (InspectBal d ds r) = show r
showInspection x = error $ "not implemented for showing ResultComponent " ++ show x


calcAvailFund :: Ast.Asset a => TestDeal a -> Date -> A.Account -> Maybe W.ExtraSupport -> Either ErrorRep SupportAvailType
calcAvailFund t d acc Nothing = return $ ByAvailAmount $ A.accBalance acc
calcAvailFund t d acc (Just support) = (\x -> sumSupport ((ByAvailAmount (A.accBalance acc)):[x]) ) <$> evalExtraSupportBalance d t support

-- ^ Deal, Date , cap balance, due balance
applyLimit :: Ast.Asset a => TestDeal a -> Date -> Balance -> Balance -> Maybe Limit -> Either ErrorRep Balance
applyLimit t d availBal dueBal Nothing = return $ min availBal dueBal
applyLimit t d availBal dueBal (Just limit) = 
    (min dueBal) <$>
      case limit of 
        DueCapAmt amt -> return $ min amt availBal
        DS ds -> do 
                   v <- queryCompound t d (patchDateToStats d ds)
                   return (min (fromRational v) availBal)
        DuePct pct -> return $ min availBal $ mulBR dueBal pct 

        x -> Left $ "Date:"++show d ++" Unsupported limit found:"++ show x


-- Get paid out amount after constrain of support and limit
-- Return (PaidOutAmount, PaidOut from Account, PaidOut from Support)
calcAvailAfterLimit :: Ast.Asset a => TestDeal a -> Date -> A.Account -> Maybe W.ExtraSupport 
                    -> Balance -> Maybe Limit -> Either ErrorRep (Amount, Amount, Amount)
-- No support , No limit -> use min(Account Balance, Due Amount)
calcAvailAfterLimit t d acc Nothing dueAmt Nothing 
  = return $ (min (A.accBalance acc) dueAmt, min (A.accBalance acc) dueAmt, 0)

-- No support , with Limit -> 
calcAvailAfterLimit t d acc Nothing dueAmt (Just limit) 
  = let 
      afterDueAmt = min (A.accBalance acc) dueAmt
    in 
      do
        txnAmt <- case limit of
                    DueCapAmt amt -> return $ min amt afterDueAmt
                    DS ds -> do 
                               v <- queryCompound t d (patchDateToStats d ds)
                               return $ min (fromRational v) afterDueAmt
                    DuePct pct -> return $ min (mulBR afterDueAmt pct) afterDueAmt
                    _ -> Left ("Failed to find <limit> type"++ show limit)
        return (txnAmt, txnAmt, 0)

-- with support , with Limit -> Get Account Balance
calcAvailAfterLimit t d acc (Just support) dueAmt mLimit 
  = let 
      accBal = A.accBalance acc
    in 
      do
        availSupport <- evalExtraSupportBalance d t support
        let totalSupport = sumSupport [availSupport, ByAvailAmount accBal]
        case totalSupport of 
          Unlimit -> return (dueAmt,dueAmt,0)
          ByAvailAmount availFund -> 
            do
              txnAmt <- applyLimit t d availFund dueAmt mLimit
              return (txnAmt, min txnAmt accBal, max (txnAmt - accBal) 0)


updateSupport :: Ast.Asset a => Date -> Maybe W.ExtraSupport -> Balance -> TestDeal a -> Either ErrorRep (TestDeal a)
updateSupport _ Nothing _ t = return t
updateSupport d (Just support) bal t = 
  do 
    (deal,amt) <- drawExtraSupport d bal support t
    return deal

performActionWrap :: Ast.Asset a => Date -> (TestDeal a, RunContext a, DL.DList ResultComponent) 
                  -> W.Action -> Either ErrorRep (TestDeal a, RunContext a, DL.DList ResultComponent)


performActionWrap d (t, rc, logs) (W.BuyAsset ml pricingMethod accName pId) 
  = performActionWrap d (t, rc, logs) (W.BuyAssetFrom ml pricingMethod accName (Just "Consol") pId)

performActionWrap d 
                  (t@TestDeal{ accounts = accsMap , pool = pt}
                  ,rc@RunContext{runPoolFlow = pFlowMap
                                ,revolvingAssump= Nothing
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAssetFrom ml pricingMethod accName mRevolvingPoolName pId) 
  = Left $ "Can't execute buy action when there is no Revolving Pool/Assumption defined"
performActionWrap d 
                  (t@TestDeal{ accounts = accsMap , pool = pt}
                  ,rc@RunContext{runPoolFlow = pFlowMap
                                ,revolvingAssump= Just rMap
                                ,revolvingInterestRateAssump = mRates}
                  ,logs)
                  (W.BuyAssetFrom ml pricingMethod accName mRevolvingPoolName pId) 
  = let 
      revolvingPoolName = fromMaybe "Consol" mRevolvingPoolName
      (assetForSale::RevolvingPool, perfAssumps::AP.ApplyAssumptionType) =  rMap Map.! revolvingPoolName  -- `debug` ("Getting pool"++ revolvingPoolName) 

      _assets = lookupAssetAvailable assetForSale d
      assets = updateOriginDate2 d <$> _assets  -- `debug` ("Asset on revolv"++ show _assets)
                
      pIdToChange = fromMaybe PoolConsol pId --`debug` ("purchase date"++ show d++ "\n" ++ show assetBought)
    in
      do
        acc <- lookupM accName accsMap
        let accBal = A.accBalance acc
        limitAmt <- case ml of 
                      Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                      Just (DueCapAmt amt) -> return (toRational amt)
                      Just (DuePct pct) -> return $ toRational (mulBR accBal pct)
                      Nothing -> return (toRational accBal)
        let availBal = min (fromRational limitAmt) accBal  -- `debug` ("Date"++ show d ++" Value on r -asset "++ show valuationOnAvailableAssets)
        valOnAvailableAssets <- priceAssetUnionList assets d pricingMethod perfAssumps mRates 
        let valuationOnAvailableAssets = sum $ getPriceValue <$> valOnAvailableAssets
        let purchaseAmt = case assetForSale of 
                            (StaticAsset _) -> min availBal valuationOnAvailableAssets -- `debug` ("Valuation on rpool"++show valuationOnAvailableAssets)
                            ConstantAsset _ -> availBal 
                            AssetCurve _ -> min availBal valuationOnAvailableAssets   
        let purchaseRatio = divideBB purchaseAmt valuationOnAvailableAssets -- `debug` ("In Buy >>> Date"++ show d ++ " Purchase Amt"++show purchaseAmt++">> avail value on availAsset"++ show  valuationOnAvailableAssets )
        let (assetBought,poolAfterBought) = buyRevolvingPool d (toRational purchaseRatio) assetForSale  -- `debug` ("In Buy >>> date "++ show d ++ "purchase ratio"++ show purchaseRatio)
        let boughtAssetBal =  sum $ curBal <$> assetBought  -- `debug` ("In Buy >>> Asset bought 0 \n"++ show assetBought++ "pflow map\n"++ show pFlowMap++" p id to change\n"++ show pIdToChange)
        -- update runtime balance
        let newPt = case pt of 
                      MultiPool pm -> MultiPool $ Map.adjust
                                                    (over P.poolIssuanceStat (Map.adjust (+ boughtAssetBal) RuntimeCurrentPoolBalance))  
                                                    pIdToChange
                                                    pm
                      ResecDeal _ -> error "Not implement on buy resec deal"

        newAccMap <- adjustM (A.draw d purchaseAmt (PurchaseAsset revolvingPoolName boughtAssetBal)) accName accsMap -- `debug` ("Asset bought total bal"++ show boughtAssetBal)
        (cfBought ,_)<- projAssetUnionList [updateOriginDate2 d ast | ast <- assetBought ] d perfAssumps mRates  -- `debug` ("Date: " ++ show d ++ "Asset bought"++ show [updateOriginDate2 d ast | ast <- assetBought ])
        let newPcf = Map.adjust (\(cfOrigin@(CF.CashFlowFrame st trs), mAflow) -> 
                                let 
                                  dsInterval = getDate <$> trs 
                                  boughtCfDates = getDate <$> view CF.cashflowTxn cfBought 
                                  newAggDates = case (dsInterval,boughtCfDates) of 
                                                  ([],[]) -> []
                                                  (_,[]) -> []
                                                  ([],_) -> boughtCfDates
                                                  (oDs,bDs) -> 
                                                    let 
                                                      lastOdate = last oDs
                                                      lastBdate = last bDs
                                                    in 
                                                      if lastOdate > lastBdate then 
                                                        []
                                                      else 
                                                        sliceDates (SliceAfter lastOdate) bDs
                                  -- TODO: the cfOrigin may not have correct beg balance ,which doesn't match all the amortization of cashflow txn
                                  mergedCf = CF.mergePoolCf2 cfOrigin cfBought 
                                in 
                                  ((over CF.cashflowTxn (`CF.aggTsByDates` (dsInterval ++ newAggDates)) mergedCf), (++ [cfBought]) <$> mAflow)
				) 
                            pIdToChange
                            pFlowMap

        let newRc = rc {runPoolFlow = newPcf  -- `debug` ("In Buy>>>"++show d ++ "New run pool >> \n"++ show newPcf)
                        ,revolvingAssump = Just (Map.insert revolvingPoolName (poolAfterBought, perfAssumps) rMap)} 
        return (t { accounts = newAccMap , pool = newPt}, newRc, logs)

performActionWrap d 
                  (t
                  ,rc@RunContext{runPoolFlow=pcf
                                ,revolvingAssump=Nothing
                                ,revolvingInterestRateAssump=mRates}
                  ,logs)
                  (W.BuyAsset ml pricingMethod accName _)
  = Left $ "Date:"++ show d ++"Missing revolving Assumption(asset assumption & asset to buy)" ++ name t

performActionWrap d 
                  (t
                  ,rc@RunContext{runPoolFlow=pcf
                                ,revolvingAssump=Nothing
                                ,revolvingInterestRateAssump=mRates}
                  ,logs)
                  (W.BuyAssetFrom {})
  = Left $ "Date:"++ show d ++"Missing revolving Assumption(asset assumption & asset to buy)" ++ name t
-- TODO need to set a limit to sell
performActionWrap d 
                  (t@TestDeal{accounts = accMap, pool = pt}  
                  ,rc@RunContext{runPoolFlow = pcf}
                  ,logs)
                  (W.LiquidatePool lm an mPid)
 = let
     liqFunction = \(p@P.Pool{ P.issuanceStat = m} ) 
                     -> over (P.poolFutureScheduleCf . _Just . _1) (CF.extendCashFlow d) $ 
                        over (P.poolFutureCf . _Just . _1 ) (CF.extendCashFlow d) $ 
                        p { P.issuanceStat = Just (Map.insert RuntimeCurrentPoolBalance 0 (fromMaybe Map.empty m)) }

     poolMapToLiq = case (pt, mPid) of 
                      (MultiPool pm, Nothing) -> pm
                      (MultiPool pm,Just pids) -> let
                                                    selectedPids = S.fromList pids
                                                  in 
                                                    Map.filterWithKey (\k v -> S.member k selectedPids) pm

                      (ResecDeal _,_) -> error "Not implement on liquidate resec deal"



     liqAmtByPool = Map.mapWithKey (\k p -> P.pricingPoolFlow d p (pcf Map.! k) lm) poolMapToLiq -- `debug` ("pool id to liq"++ show poolMapToLiq)
     liqAmt = sum $ Map.elems liqAmtByPool

     -- Update collected cashflow
     newPt = case (pt, mPid) of 
               (MultiPool pm, Nothing) -> MultiPool $ Map.map liqFunction pm
               (MultiPool pm, Just pids) -> let
                                              selectedPids = S.fromList pids
                                              selectedPoolMap = Map.filterWithKey (\k v -> S.member k selectedPids) pm
                                            in 
                                              MultiPool $ Map.union (Map.map liqFunction selectedPoolMap) pm
               (ResecDeal _,_) -> error "Not implement on liquidate resec deal"

     liqComment = LiquidationProceeds (fromMaybe [] mPid)
     accMapAfterLiq = Map.adjust (A.deposit liqAmt d liqComment) an accMap
     newPfInRc = foldr (Map.adjust (set (_1 . CF.cashflowTxn) [])) pcf  (Map.keys poolMapToLiq)
     -- Update current balance to zero 
   in
     return (t {accounts = accMapAfterLiq , pool = newPt} , rc {runPoolFlow = newPfInRc}, logs)


performActionWrap d (t, rc, logs) (W.WatchVal ms dss)
  = (inspectListVars t d dss) >>= (\vs -> Right (t, rc, DL.snoc logs (InspectWaterfall d ms dss (showInspection <$> vs)))) 


performActionWrap d (t, rc, logs) (W.ActionWithPre p actions) 
  = do 
      flag <- testPre d t p 
      if flag then 
        foldM (performActionWrap d) (t,rc,logs) actions
      else
        return (t, rc, logs)


performActionWrap d (t, rc, logs) (W.ActionWithPre2 p actionsTrue actionsFalse) 
  = do 
      flag <- testPre d t p
      if flag then
        foldM (performActionWrap d) (t,rc,logs) actionsTrue
      else
        foldM (performActionWrap d) (t,rc,logs) actionsFalse


performActionWrap d (t, rc, logs) (W.ChangeStatus mPre newSt) 
  = let 
      newLog = DealStatusChangeTo d (status t) newSt "By Waterfall"
    in 
      case mPre of
        Nothing -> return (t {status=newSt} , rc, DL.snoc logs newLog)
        Just p -> 
          do 
            flag <- testPre d t p
            if flag then
              return (t {status=newSt} , rc, DL.snoc logs newLog)
            else 
              return (t, rc, logs)

-- ^ go down to performAction
performActionWrap d (t, rc, logs) a 
  = do 
      dealAfterExe <- performAction d t a 
      return (dealAfterExe, rc, logs)

performAction :: Ast.Asset a => Date -> TestDeal a -> W.Action -> Either ErrorRep (TestDeal a)
performAction d t@TestDeal{accounts=accMap, ledgers = Nothing} (W.TransferAndBook _ _ _ _ _) 
  = Left $ "Date:"++ show d ++" Missing ledger map to book " ++ name t 
performAction d t@TestDeal{accounts=accMap, ledgers = Just ledgerM} 
                (W.TransferAndBook mLimit an1 an2 (dr, lName) mComment)
  = do 
      sourceAcc <- lookupM an1 accMap
      targetAcc <- lookupM an2 accMap
      (transferAmt,accDrawAmt,_) <- calcAvailAfterLimit t d sourceAcc Nothing (A.accBalance sourceAcc) mLimit
      (sourceAcc', targetAcc') <- A.transfer (sourceAcc,targetAcc) d transferAmt 
      let newLedgerM = Map.adjust (LD.entryLogByDr (dr, transferAmt) d Nothing) lName ledgerM
      return t {accounts = Map.insert an1 sourceAcc' (Map.insert an2 targetAcc' accMap)
               , ledgers = Just newLedgerM}  

performAction d t@TestDeal{accounts=accMap} (W.Transfer mLimit an1 an2 mComment)
  = do 
      sourceAcc <- lookupM an1 accMap
      targetAcc <- lookupM an2 accMap
      (transferAmt,_,_) <- calcAvailAfterLimit t d sourceAcc Nothing (A.accBalance sourceAcc) mLimit
      (sourceAcc', targetAcc') <- A.transfer (sourceAcc,targetAcc) d transferAmt
      return t {accounts = Map.insert an1 sourceAcc' (Map.insert an2 targetAcc' accMap)}  

performAction d t@TestDeal{accounts=accMap} (W.TransferMultiple sourceAccList targetAcc mComment)
  = foldM (\acc (mLimit, sourceAccName) -> 
            performAction d acc (W.Transfer mLimit sourceAccName targetAcc mComment))
          t
          sourceAccList

-- ^ book ledger 
performAction d t@TestDeal{ledgers= Nothing} (W.BookBy _) = Left $ "Date:"++ show d ++" Missing ledger map to book " ++ name t
performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.Till ledger dr ds)) =
  do
    targetAmt <- queryCompound t d ds
    ledgerI <- lookupM ledger ledgerM
    let (bookDirection, amtToBook) = LD.bookToTarget ledgerI (dr, fromRational targetAmt)
    let newLedgerM = Map.adjust (LD.entryLogByDr (bookDirection,amtToBook) d Nothing) ledger ledgerM
    return $ t {ledgers = Just newLedgerM } 

performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.ByDS ledger dr ds)) =
  do
    amtToBook <- queryCompound t d ds
    let newLedgerM = Map.adjust (LD.entryLogByDr (dr,(fromRational amtToBook)) d Nothing) ledger ledgerM
    return $ t {ledgers = Just newLedgerM } 

-- ^ it will book ledgers by order with mandatory caps which describes by a <formula> 
-- ^ ds -> value to book 
-- ^ ledgersList -> list of ledgers to book 
performAction d t@TestDeal{ledgers= Just ledgerM} (W.BookBy (W.PDL dr ds ledgersList)) =
  let
    ledgerNames = fst <$> ledgersList
  in 
    do
      amtToBook <- queryCompound t d ds
      ledgCaps <- sequenceA [ queryCompound t d ledgerCap | ledgerCap <- snd <$> ledgersList ]
      let amtBookedToLedgers = paySeqLiabilitiesAmt (fromRational amtToBook) (fromRational <$> ledgCaps) 
      let newLedgerM = foldr 
                         (\(ln,amt) acc -> Map.adjust (LD.entryLogByDr (dr,amt) d Nothing) ln acc)
                         ledgerM
                         (zip ledgerNames amtBookedToLedgers) --`debug` ("amts to book"++ show amtBookedToLedgers)
      return $ t {ledgers = Just newLedgerM}

-- ^ pay fee sequentially, but not accrued
performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFeeBySeq mLimit an fns mSupport) =
  let 
    q = DueTotalOf [DueArrears,DueFee]
    q' = getDueBal d (Just q)
  in
    do 
      acc <- lookupM an accMap
      feesToPay <- lookupVs fns feeMap
      (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport (sum (map q' feesToPay)) mLimit
      (feesPaid, remainAmt) <- paySeqM d paidOutAmt q' (pay d q) (Right []) feesToPay
      newAccMap <- adjustM (A.draw d accPaidOut (SeqPayFee fns)) an accMap 
      let dealAfterAcc = t {accounts = newAccMap
                           ,fees = Map.fromList (zip fns feesPaid) <> feeMap}
      updateSupport d mSupport supportPaidOut dealAfterAcc
  
-- ^ pay out fee in pro-rata fashion
performAction d t@TestDeal{fees=feeMap, accounts=accMap} (W.PayFee mLimit an fns mSupport) =
  let
    q = DueTotalOf [DueArrears,DueFee]
    qFn = getDueBal d (Just q)
  in 
    do 
      acc <- lookupM an accMap
      feesToPay <- lookupVs fns feeMap
      let totalFeeDue = sum $ map qFn feesToPay
      (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalFeeDue mLimit
      (feesPaid, remainAmt) <- payProM d paidOutAmt qFn (pay d q) feesToPay
      newAccMap <- adjustM (A.draw d accPaidOut (SeqPayFee fns)) an accMap 
      let dealAfterAcc = t {accounts = newAccMap
                           ,fees = Map.fromList (zip fns feesPaid) <> feeMap}
      updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t (W.AccrueAndPayIntBySeq mLimit an bnds mSupport)
  = do
      dealWithBondDue <- performAction d t (W.CalcBondInt bnds)
      performAction d dealWithBondDue (W.PayIntBySeq mLimit an bnds mSupport)

performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} 
                (W.PayIntOverIntBySeq mLimit an bnds mSupport)
  = let 
      q = DueArrears
      qFn = getDueBal d (Just q)
    in 
      do 
        bndsList <- lookupVs bnds bndMap
        let dueAmts = qFn <$> bndsList
        acc <- lookupM an accMap
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport (sum dueAmts) mLimit
        (bondsPaid,_) <- paySeqM d paidOutAmt qFn (pay d q) (Right []) bndsList
        newAccMap <- adjustM (A.draw d accPaidOut (PayInt bnds)) an accMap 
        let dealAfterAcc = t {accounts = newAccMap ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}
        updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap, accounts=accMap, liqProvider=liqMap} 
              (W.PayIntBySeq mLimit an bnds mSupport)
   = let 
       q = DueTotalOf [DueArrears, DueInterest Nothing]
       qFn = getDueBal d (Just q)
     in 
       do 
         acc <- lookupM an accMap
         bndsList <- lookupVs bnds bndMap
         let dueAmts = qFn <$> bndsList
         let totalDue = sum dueAmts
         (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
         (bondsPaid,_) <- paySeqM d paidOutAmt qFn (pay d q) (Right []) bndsList
         newAccMap <- adjustM (A.draw d accPaidOut (PayInt bnds)) an accMap 
         let dealAfterAcc = t {accounts = newAccMap
                              ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}
         updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
              (W.PayIntOverInt mLimit an bnds mSupport)
   = let
       q = DueArrears
       qFn = getDueBal d (Just q)
     in 
       do
         acc <- lookupM an accMap
         bndsList <- lookupVs bnds bndMap
         let dueAmts = qFn <$> bndsList
         let totalDue = sum dueAmts
         (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
         (bondsPaid,_) <- payProM d paidOutAmt qFn (pay d q) bndsList
         newAccMap <- adjustM (A.draw d accPaidOut (PayInt bnds)) an accMap
         let dealAfterAcc = t {accounts = newAccMap
                              ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}

         updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
              (W.PayInt mLimit an bnds mSupport)
  = let
      q = DueTotalOf [DueArrears, DueInterest Nothing]
      qFn = getDueBal d (Just q)
    in
      do
        acc <- lookupM an accMap
        bndsList <- lookupVs bnds bndMap
        let dueAmts = qFn <$> bndsList
        let totalDue = sum dueAmts
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
        (bondsPaid,_) <- payProM d paidOutAmt qFn (pay d q) bndsList
        newAccMap <- adjustM (A.draw d accPaidOut (PayInt bnds)) an accMap    
        let dealAfterAcc = t {accounts = newAccMap ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap}
        updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t@TestDeal{bonds=bndMap,accounts=accMap,ledgers= Just ledgerM} 
                (W.PayIntAndBook mLimit an bnds mSupport (dr, lName))
  = let
      q = DueTotalOf [DueArrears, DueInterest Nothing]
      qFn = getDueBal d (Just q)
    in
      do
        acc <- lookupM an accMap
        bndsList <- lookupVs bnds bndMap
        let dueAmts = qFn <$> bndsList
        let totalDue = sum dueAmts
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
        (bondsPaid,_) <- payProM d paidOutAmt qFn (pay d q) bndsList
        let newLedgerM = Map.adjust (LD.entryLogByDr (dr,paidOutAmt) d Nothing) lName ledgerM
        newAccMap <- adjustM (A.draw d accPaidOut (PayInt bnds)) an accMap
     
        let dealAfterAcc = t {accounts = newAccMap
                             ,bonds = Map.fromList (zip bnds bondsPaid) <> bndMap
                             ,ledgers = Just newLedgerM}

        updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t (W.AccrueAndPayInt mLimit an bnds mSupport) =
  do
    dealWithBondDue <- performAction d t (W.CalcBondInt bnds)
    performAction d dealWithBondDue (W.PayInt mLimit an bnds mSupport)

performAction d t (W.CalcAndPayFee mLimit ans fees mSupport) =
  do
    dealWithFeeDue <- performAction d t (W.CalcFee fees)
    performAction d dealWithFeeDue (W.PayFee mLimit ans fees mSupport)

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntResidual mLimit an bndName) =
  do 
    acc <- lookupM an accMap
    let availBal = A.accBalance acc
    limitAmt <- applyLimit t d availBal availBal mLimit
    newAccMap <- adjustM (A.draw d limitAmt (PayYield bndName)) an accMap
    newBondMap <- adjustM (pay d DueResidual limitAmt) bndName bndMap
    return $ t {accounts = newAccMap , bonds = newBondMap }


-- TODO check for multi interest bond
-- TODO support need to patch
performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntByRateIndex mLimit an bndNames idx mSupport)
  = let
      q = DueInterest (Just idx)
      qFn = getDueBal d (Just q)
    in 
      do 
        acc <- lookupM an accMap
        bndsList' <- lookupVs bndNames bndMap
        let bndsList = filter (is L._MultiIntBond) $ bndsList'
        let bndNames_ = L.bndName <$> bndsList
        let totalDue = sum $ map qFn bndsList
        (actualPaidOut,_,_) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit 
        (paidBonds,_) <- payProM d actualPaidOut qFn (pay d q) bndsList 
        newAccMap <- adjustM (A.draw d actualPaidOut (PayInt bndNames_)) an accMap
        return $ t {accounts = newAccMap , bonds =  Map.fromList (zip bndNames_ paidBonds) <> bndMap}

-- TODO support need to patch
performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntByRateIndexBySeq mLimit an bndNames idx mSupport)
  = let 
      q = DueInterest (Just idx)
      qFn = getDueBal d (Just q)
    in
      do 
        bndsList' <- lookupVs bndNames bndMap
        let bndsList = filter (is L._MultiIntBond) $ bndsList'
        let bndNames_ = L.bndName <$> bndsList
        acc <- lookupM an accMap
        let totalDue = sum $ map qFn bndsList
        (actualPaidOut,_,_) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
        (paidBonds,_) <- paySeqM d actualPaidOut qFn (pay d q) (Right []) bndsList
        newAccMap <- adjustM (A.draw d actualPaidOut (PayInt bndNames_)) an accMap
        return $ t {accounts = newAccMap
                    , bonds =  Map.fromList (zip bndNames_ paidBonds) <> bndMap}


performAction d t@TestDeal{fees=feeMap,accounts=accMap} (W.PayFeeResidual mlimit an feeName) =
  do 
    acc <- lookupM an accMap
    paidOutAmt <- applyLimit t d (A.accBalance acc) (A.accBalance acc) mlimit
    newAccMap <- adjustM (A.draw d paidOutAmt (PayFeeYield feeName)) an accMap
    feeMapAfterPay <- adjustM (pay d DueResidual paidOutAmt) feeName feeMap
    return $ t {accounts = newAccMap, fees = feeMapAfterPay}

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
                (W.PayPrinBySeq mLimit an bnds mSupport) 
  = let 
      q = DuePrincipal
      qFn = getDueBal d (Just q)
    in 
      do
        acc <- lookupM an accMap
        bndsList <- lookupVs bnds bndMap
        let bndsToPay = filter (not . L.isPaidOff) bndsList
        let bndsToPayNames = L.bndName <$> bndsToPay
        bndsWithDue <- traverse (calcDuePrin t d) bndsToPay
        let bndsDueAmts = qFn <$> bndsWithDue
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport (sum bndsDueAmts) mLimit
        (bondsPaid, remainAmt) <- paySeqM d paidOutAmt qFn (pay d q) (Right []) bndsWithDue
        newAccMap <- adjustM (A.draw d accPaidOut (PayPrin bndsToPayNames)) an accMap 
        let dealAfterAcc = t {accounts = newAccMap
                             ,bonds = Map.fromList (zip bndsToPayNames bondsPaid) <> bndMap}
        updateSupport d mSupport supportPaidOut dealAfterAcc

performAction d t@TestDeal{bonds=bndMap,accounts=accMap} 
                (W.PayPrinGroup mLimit an bndGrpName by mSupport) 
  = let 
      bg@(L.BondGroup bndsMap pt) = bndMap Map.! bndGrpName
      bndsToPayNames = L.bndName <$> Map.elems bndsMap
    in
      do
        acc <- lookupM an accMap
        bndsWithDueMap <- sequenceA $ Map.map (calcDuePrin t d) bndsMap
        bgGap <- queryCompound t d (BondBalanceGapAt d bndGrpName)
        let bndsDueAmtsMap = Map.map (\x -> (x, L.bndDuePrin x)) bndsWithDueMap
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport (fromRational bgGap) mLimit
        let payOutPlan = allocAmtToBonds by paidOutAmt (Map.elems bndsDueAmtsMap) -- TODO: bond map is not complete 
        bndLstAfterPay <- sequenceA $ map (\(bnd, _amt) -> pay d DuePrincipal _amt bnd) payOutPlan 
        let bndMapAfterPay = (lstToMapByFn L.bndName bndLstAfterPay) <> bndsMap
        newAccMap <- adjustM (A.draw d accPaidOut (PayGroupPrin bndsToPayNames)) an accMap
        let dealAfterAcc = t {accounts = newAccMap
                             ,bonds = Map.insert bndGrpName (L.BondGroup bndMapAfterPay pt) bndMap}

        updateSupport d mSupport supportPaidOut dealAfterAcc


-- ^ accure interest and payout interest to a bond group with sequence input "by"
performAction d t@TestDeal{bonds=bndMap} (W.AccrueAndPayIntGroup mLimit an bndName by mSupport)
  = do 
      dAfterAcc <- performAction d t (W.AccrueIntGroup [bndName])
      performAction d dAfterAcc (W.PayIntGroup mLimit an bndName by mSupport)

-- ^ accrue interest for a group of bonds
performAction d t@TestDeal{bonds=bndMap} (W.AccrueIntGroup bndNames)
  = do 
      let bondGrp = Map.filterWithKey (\k _ -> S.member k (S.fromList bndNames)) bndMap
      bondGrpAccrued <- mapM (calcDueInt t d) bondGrp
      return t {bonds = bondGrpAccrued <> bndMap}

-- ^ pay interest for a group of bonds with sequence input "by"
performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayIntGroup mLimit an bndGrpName by mSupport)
  = let 
      L.BondGroup bndsMap pt = bndMap Map.! bndGrpName
      bndsToPayNames = L.bndName <$> Map.elems bndsMap
      q = DueTotalOf [DueArrears, DueInterest Nothing]
      qFn = getDueBal d (Just q)
    in
      do
        acc <- lookupM an accMap
        bndsWithDueMap <- mapM (calcDueInt t d) bndsMap
        let bndsDueAmtsMap = Map.map (\x -> (x, qFn x)) bndsWithDueMap
        let totalDue = sum $ snd <$> Map.elems bndsDueAmtsMap -- `debug` (">date"++show d++" due amt"++show bndsDueAmtsMap)
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit

        let payOutPlan = allocAmtToBonds by paidOutAmt (Map.elems bndsDueAmtsMap) -- TODO: bond map is not complete 
        let payOutPlanWithBondName = [ (L.bndName bnd,amt) | (bnd,amt) <- payOutPlan] -- `debug` (">date"++show d++"payOutPlan"++ show payOutPlan)
        bndMapAfterPay <- mapM (\(bnd, amt) ->  pay d q amt bnd) payOutPlan
        newAccMap <- adjustM (A.draw d accPaidOut (PayGroupInt bndsToPayNames)) an accMap 
        let dealAfterAcc = t {accounts = newAccMap
                             ,bonds = Map.insert 
                                        bndGrpName 
                                        (L.BondGroup ((lstToMapByFn L.bndName bndMapAfterPay) <> bndsMap) pt)
                                        bndMap}

        updateSupport d mSupport supportPaidOut dealAfterAcc


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrinWithDue an bnds Nothing) 
  = let
      bndsToPay = getActiveBonds t bnds
      bndsToPayNames = L.bndName <$> bndsToPay
      bndsDueAmts = L.bndDuePrin <$> bndsToPay
    in 
      do
        acc <- lookupM an accMap
        let actualPaidOut = min (A.accBalance acc) $ sum bndsDueAmts
        (bndsPaid, remainAmt) <- payProM d actualPaidOut L.bndDuePrin (pay d DuePrincipal) bndsToPay
        let bndMapUpdated = (Map.fromList $ zip bndsToPayNames bndsPaid) <> bndMap
    	accMapAfterPay <- adjustM (A.draw d actualPaidOut (PayPrin bnds)) an accMap
        return $ t {accounts = accMapAfterPay, bonds = bndMapUpdated}


performAction d t@TestDeal{bonds=bndMap,accounts=accMap} (W.PayPrin mLimit an bnds mSupport)
  = let 
      bndsToPay = getActiveBonds t bnds
      q = DuePrincipal
      qFn = L.bndDuePrin
    in
      do
        acc <- lookupM an accMap
        bndsWithDue <- traverse (calcDuePrin t d) bndsToPay
        let bndsDueAmts = qFn <$> bndsWithDue
        let bndsToPayNames = L.bndName <$> bndsWithDue
        let totalDue = sum bndsDueAmts
        (paidOutAmt,accPaidOut,supportPaidOut) <- calcAvailAfterLimit t d acc mSupport totalDue mLimit
        (bondsPaid, remainAmt) <- payProM d paidOutAmt qFn (pay d q) bndsWithDue
        newAccMap <- adjustM (A.draw d accPaidOut (PayPrin bndsToPayNames)) an accMap 
        let dealAfterAcc = t {accounts = newAccMap
                             ,bonds = Map.fromList (zip bndsToPayNames bondsPaid) <> bndMap}

        updateSupport d mSupport supportPaidOut dealAfterAcc

-- ^ pay principal without any limit
performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.PayPrinResidual an bnds) = 
  let
    bndsToPay = [ b { L.bndDuePrin = L.bndBalance b}  | b <- getActiveBonds t bnds ]
    bndsToPayNames = L.bndName <$> bndsToPay
    bndsDueAmts = map L.getCurBalance bndsToPay
  in 
    do 
      acc <- lookupM an accMap
      let availBal = A.accBalance acc
      let actualPaidOut = min availBal $ sum bndsDueAmts -- `debug` ("bonds totoal due ->"++show(bndsDueAmts))
      let bndsAmountToBePaid = zip bndsToPay (prorataFactors bndsDueAmts actualPaidOut)
      bndsPaid <- sequenceA $ map (\(l,amt) -> pay d DuePrincipal amt l) bndsAmountToBePaid  
      let bndMapUpdated =  (Map.fromList $ zip bndsToPayNames bndsPaid) <> bndMap
      accMapAfterPay <- adjustM (A.draw d actualPaidOut (PayPrin bnds)) an accMap
      return $ t {accounts = accMapAfterPay, bonds = bndMapUpdated} -- `debug` ("Bond Prin Pay Result"++show(bndMapUpdated))

performAction d t@TestDeal{accounts=accMap, bonds=bndMap} (W.FundWith mlimit an bnd) = 
  do
    fundAmt_ <- case mlimit of 
                  Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                  Just (DueCapAmt amt) -> return $ toRational amt
                  _ -> Left $ "Date:"++show d ++"Not valid limit for funding with bond"++ show bnd
    let fundAmt = fromRational fundAmt_
    let accMapAfterFund = Map.adjust (A.deposit fundAmt d (FundWith bnd fundAmt)) an accMap
    bndToFund <- lookupM bnd bndMap
    bndFunded <- draw d fundAmt (FundWith bnd fundAmt) bndToFund
    return $ t {accounts = accMapAfterFund, bonds= Map.fromList [(bnd,bndFunded)] <> bndMap } 

-- ^ write off bonds and book 
performAction d t@TestDeal{bonds = bndMap, ledgers = Just ledgerM } 
              (W.WriteOffAndBook mLimit bnd (dr,lName))
  = do 
      bndToWriteOff <- lookupM bnd bndMap
      let bndBal = L.bndBalance bndToWriteOff
      writeAmt <- applyLimit t d bndBal bndBal mLimit
      let newLedgerM = Map.adjust (LD.entryLogByDr (dr,writeAmt) d (Just (WriteOff bnd writeAmt))) lName ledgerM
      bndWritedOff <- writeOff d DuePrincipal writeAmt bndToWriteOff
      return $ t {bonds = Map.fromList [(bnd,bndWritedOff)] <> bndMap, ledgers = Just newLedgerM}

performAction d t@TestDeal{bonds=bndMap} (W.WriteOff mlimit bnd)
  = do 
      bndToWriteOff <- lookupM bnd bndMap
      writeAmt <- case mlimit of
                    Just (DS ds) -> queryCompound t d (patchDateToStats d ds)
                    Just (DueCapAmt amt) -> return $ toRational amt
                    Nothing -> return $ toRational . L.bndBalance $ bndToWriteOff
                    x -> Left $ "Date:"++show d ++"not supported type to determine the amount to write off"++ show x

      let writeAmtCapped = min (fromRational writeAmt) $ L.bndBalance bndToWriteOff
      bndWritedOff <- writeOff d DuePrincipal writeAmtCapped $ bndToWriteOff
      return $ t {bonds = Map.fromList [(bnd,bndWritedOff)] <> bndMap}

performAction d t@TestDeal{bonds=bndMap, ledgers = Just ledgerM} 
              (W.WriteOffBySeqAndBook mLimit bnds (dr,lName))
  = do
      bndsToWriteOff <- mapM (calcDueInt t d . (bndMap Map.!)) bnds
      let totalBondBal = sum $ L.bndBalance <$> bndsToWriteOff
      -- total amount to be write off
      writeAmt <- applyLimit t d totalBondBal totalBondBal mLimit
      (bndWrited, _) <- paySeqM d writeAmt L.bndBalance (writeOff d DuePrincipal) (Right []) bndsToWriteOff 
      let bndMapUpdated = lstToMapByFn L.bndName bndWrited
      let newLedgerM = Map.adjust (LD.entryLogByDr (dr,writeAmt) d Nothing) lName ledgerM
      return t {bonds = bndMapUpdated <> bndMap, ledgers = Just newLedgerM}


performAction d t@TestDeal{bonds=bndMap } (W.WriteOffBySeq mLimit bnds)
  = do 
      bondsToWriteOff <- mapM (calcDueInt t d . (bndMap Map.!)) bnds
      let totalBondBal = sum $ L.bndBalance <$> bondsToWriteOff
      writeAmt <- applyLimit t d totalBondBal totalBondBal mLimit
      (bndWrited, _) <- paySeqM d writeAmt L.bndBalance (writeOff d DuePrincipal) (Right []) bondsToWriteOff 
      let bndMapUpdated = lstToMapByFn L.bndName bndWrited
      return t {bonds = bndMapUpdated <> bndMap }

performAction d t@TestDeal{fees=feeMap} (W.CalcFee fns) 
  = do
      newFeeMap <- mapM (calcDueFee t d) $ getFeeByName t (Just fns)
      return t {fees = newFeeMap <> feeMap }


-- TODO need to check bond names exists
-- TODO wont' persert the bond shape for a bond group
performAction d t@TestDeal{bonds=bndMap} (W.CalcBondInt bns) 
  = do 
      newBondMap <- mapM (calcDueInt t d) $ getBondsByName t (Just bns)
      return t {bonds = newBondMap <> bndMap}

-- ^ set due prin mannually
performAction d t@TestDeal{bonds=bndMap} (W.CalcBondPrin2 mLimit bnds) 
  = do 
      bndsToPay <- lookupVs bnds bndMap
      let bndsToPayNames = L.bndName <$> bndsToPay
      bndsDueAmts <- traverse (L.bndDuePrin <$>) $ (calcDuePrin t d) <$> bndsToPay
      let totalDue = sum bndsDueAmts
      bookCap <- applyLimit t d totalDue totalDue mLimit
      let bndsAmountToBook = zip bndsToPayNames $ prorataFactors bndsDueAmts bookCap
      let newBndMap = foldr 
                        (\(bn,amt) acc -> Map.adjust (\b -> b {L.bndDuePrin = amt})  bn acc) 
                        bndMap 
                        bndsAmountToBook -- `debug` ("Calc Bond Prin"++ show bndsAmountToBePaid)
      return $ t {bonds = newBndMap} 

performAction d t@TestDeal{bonds=bndMap, accounts = accMap} (W.CalcBondPrin mLimit accName bnds mSupport) 
  = do 
      bndsToPay <- lookupVs bnds bndMap
      let bndsToPayNames = L.bndName <$> bndsToPay
      acc <- lookupM accName accMap 
      let accBal = A.accBalance acc
      bndsDueAmts <- traverse (L.bndDuePrin <$>) $ (calcDuePrin t d) <$> bndsToPay
      (payAmount,_,_) <- calcAvailAfterLimit t d acc mSupport (sum bndsDueAmts) mLimit 
      let bndsAmountToBePaid = zip bndsToPayNames $ prorataFactors bndsDueAmts payAmount
      let newBndMap = foldr 
                        (\(bn,amt) acc -> Map.adjust (\b -> b {L.bndDuePrin = amt}) bn acc) 
                        bndMap 
                        bndsAmountToBePaid
      return $ t {bonds = newBndMap}

      
-- ^ draw cash and deposit to account
performAction d t@TestDeal{accounts=accs, liqProvider = Nothing} (W.LiqSupport mLimit pName CE.LiqToAcc ans)
  = Left $ "Date:"++show d++"Can't support account as no liq provider defined in deal"++ show (name t)
performAction d t@TestDeal{accounts=accs, liqProvider = Just _liqProvider} (W.LiqSupport mLimit pName CE.LiqToAcc ans)
  | length ans == 1 
      = let 
          liq = _liqProvider Map.! pName 
          [an] = ans
        in 
          do 
            transferAmt <- case (CE.liqCredit liq, mLimit) of 
                             (Unlimit, Nothing) -> Left $ "Date:"++show d ++"Can't deposit unlimit cash to an account in LiqSupport(Account):"++ show pName ++ ":"++ show an
                             (ByAvailAmount av, Nothing) -> Right . toRational $ av
                             (Unlimit, Just (DS ds)) -> queryCompound t d (patchDateToStats d ds) -- `debug` ("hit with ds"++ show ds)
                             (ByAvailAmount av, Just (DS ds)) -> (min (toRational av)) <$> queryCompound t d (patchDateToStats d ds) 
                             (_ , Just _x) -> Left $ "Date:"++show d ++"Not support limit in LiqSupport(Account)"++ show _x 
            let dAmt = fromRational transferAmt
	    newLiqMap <- adjustM (draw d dAmt LiquidationDraw) pName _liqProvider
            return t { accounts = Map.adjust (A.deposit dAmt d (LiquidationSupport pName)) an accs
                     , liqProvider = Just newLiqMap }
  | otherwise = Left $ "Date:"++show d ++"There should only one account for LiqToAcc of LiqSupport"


-- TODO : add pay fee by sequence
-- performAction d t@TestDeal{fees=feeMap,liqProvider = Just _liqProvider} (W.LiqSupport mLimit pName CE.LiqToFee fns)
--   = do 
--       liq <- lookupM pName _liqProvider
--       totalDueFee <- queryCompound t d (CurrentDueFee fns)
--       supportAmt <- applyLimit t d (fromRational totalDueFee) (fromRational totalDueFee) mLimit
-- 
--       let transferAmt = case CE.liqCredit liq of 
--                           Unlimit -> supportAmt
--                           (ByAvailAmount v) -> min supportAmt v
-- 
--       let newFeeMap = payInMap d transferAmt F.feeDue (pay d (DueTotalOf [DueArrears,DueFee])) fns ByProRata feeMap
--       newLiqMap <- adjustM (draw d transferAmt LiquidationDraw) pName _liqProvider 
--       return $ t { fees = newFeeMap, liqProvider = Just newLiqMap }

-- TODO : add pay int by sequence
-- TODO : may not work for bond group
performAction d t@TestDeal{bonds=bndMap,liqProvider = Nothing} (W.LiqSupport mLimit pName CE.LiqToBondInt bns)
  = Left $ "Date:"++show d++"Can't support bond interest as no liq provider defined in deal"++ show (name t)
performAction d t@TestDeal{bonds=bndMap,liqProvider = Just _liqProvider} 
                (W.LiqSupport mLimit pName CE.LiqToBondInt bns)
  = do 
      liq <- lookupM pName _liqProvider
      totalDueInt <- queryCompound t d (CurrentDueBondInt bns)
      supportAmt <- applyLimit t d (fromRational totalDueInt) (fromRational totalDueInt) mLimit

      let transferAmt = case CE.liqCredit liq of 
                          Unlimit -> supportAmt
                          (ByAvailAmount v) -> min supportAmt v

      -- let newBondMap = payInMap d transferAmt L.getTotalDueInt (L.payInt d) bns ByProRata bndMap
      newBondMap <- payInMapM d transferAmt L.getTotalDueInt (pay d (DueInterest Nothing)) bns ByProRata bndMap
      newLiqMap <- adjustM (draw  d transferAmt  LiquidationDraw) pName _liqProvider 
      return $ t { bonds = newBondMap, liqProvider = Just newLiqMap }


-- ^ payout due interest / due fee / oustanding balance to liq provider
performAction d t@TestDeal{accounts=accs,liqProvider = Nothing} (W.LiqRepay mLimit rpt an pName)
  = Left $ "Date:"++show d++"Can't repay to liq provider as no liq provider defined in deal"++ show (name t)
performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqRepay mLimit rpt an pName)
  = 
    let 
      liqDueAmts CE.LiqBal = [ CE.liqBalance $ _liqProvider Map.! pName]
      liqDueAmts CE.LiqInt =  [ CE.liqDueInt $ _liqProvider Map.! pName ]
      liqDueAmts CE.LiqPremium = [ CE.liqDuePremium $ _liqProvider Map.! pName]
      liqDueAmts (CE.LiqRepayTypes lrts) = concat $ liqDueAmts <$> lrts

      overDrawnBalance = case (CE.liqCredit $ _liqProvider Map.! pName) of
                           Unlimit -> 0
                           ByAvailAmount v -> negate v
      
      dueBreakdown 
        | overDrawnBalance > 0 = overDrawnBalance:liqDueAmts rpt
        | otherwise = liqDueAmts rpt

      liqTotalDues = sum dueBreakdown
      
      cap = min liqTotalDues $ A.accBalance $ accs Map.! an
    in
      do
        transferAmt <- applyLimit t d cap cap mLimit
        let paidOutsToLiq = paySeqLiabilitiesAmt transferAmt dueBreakdown

        let rptsToPair = case rpt of 
                            CE.LiqRepayTypes lrts -> lrts
                            x  -> [x]

        let paidOutWithType
              | overDrawnBalance > 0 = zip (CE.LiqOD:rptsToPair) paidOutsToLiq 
              | otherwise = zip rptsToPair paidOutsToLiq -- `debug` ("rpts To pair"++ show rptsToPair)

        newAccMap <- adjustM (A.draw d transferAmt (LiquidationSupport pName)) an accs 
        let newLiqMap = foldl
                          (\acc (_rpt,_amt) -> Map.adjust (CE.repay _amt d _rpt ) pName acc)
                          _liqProvider
                          paidOutWithType
        return $ t { accounts = newAccMap, liqProvider = Just newLiqMap }  

-- ^ pay yield to liq provider
performAction d t@TestDeal{accounts=accs,liqProvider = Nothing } (W.LiqYield limit an pName)
  = Left $ "Date:"++show d++"Can't pay yield to liq provider as no liq provider defined in deal"++ show (name t)
performAction d t@TestDeal{accounts=accs,liqProvider = Just _liqProvider} (W.LiqYield limit an pName)
  = do 
      acc <- lookupM an accs
      let cap = A.accBalance acc
      transferAmt <- case limit of 
                      Nothing -> return (toRational cap)
                      Just (DS ds) -> (min (toRational cap)) <$> (queryCompound t d (patchDateToStats d ds)) 
                      _ -> Left $ "Date:"++show d ++"Not implement the limit"++ show limit++"For Pay Yield to liqProvider"
      
      newAccMap <- adjustM (A.draw d (fromRational transferAmt) (LiquidationSupport pName)) an accs
      let newLiqMap = Map.adjust (CE.repay (fromRational transferAmt) d CE.LiqResidual) pName _liqProvider 
      return t { accounts = newAccMap, liqProvider = Just newLiqMap }

performAction d t@TestDeal{liqProvider = Nothing} (W.LiqAccrue liqNames)
  = Left $ "Date:"++show d++"Can't accrue liq provider as no liq provider defined in deal"++ show (name t)
performAction d t@TestDeal{liqProvider = Just _liqProvider} (W.LiqAccrue liqNames)
  = let 
      updatedLiqProvider = mapWithinMap ((updateLiqProvider t d) . (CE.accrueLiqProvider d)) liqNames _liqProvider
    in 
      return $ t {liqProvider = Just updatedLiqProvider}

performAction d t@TestDeal{rateSwap = Nothing } (W.SwapAccrue sName)
  = Left $ "Date:"++show d++"Can't accrue swap at location:"++ show sName++"as no swap defined in deal"++ show (name t)
performAction d t@TestDeal{rateSwap = Just rtSwap } (W.SwapAccrue sName)
  = do
      refBal <- case HE.rsNotional (rtSwap Map.! sName) of 
                  (HE.Fixed b) -> return b
                  (HE.Base ds) -> fromRational <$> queryCompound t d (patchDateToStats d ds)
                  (HE.Schedule ts) -> Right . fromRational $ getValByDate ts Inc d

      let newRtSwap = Map.adjust 
                        (HE.accrueIRS d)
                        sName
                        (Map.adjust (set HE.rsRefBalLens refBal) sName rtSwap)
      return $ t { rateSwap = Just newRtSwap } 


performAction d t@TestDeal{rateCap = Nothing, accounts = accsMap } (W.CollectRateCap accName sName)
  = Left $ "Date:"++show d++"Can't collect rate cap at location:"++ show sName++"as no rate cap defined in deal"++ show (name t)
performAction d t@TestDeal{rateCap = Just rcM, accounts = accsMap } (W.CollectRateCap accName sName)
  = let 
      receiveAmt = max 0 $ HE.rcNetCash $ rcM Map.! sName
      newRcSwap = Map.adjust (HE.receiveRC d) sName rcM -- `debug` ("REceiv AMT"++ show receiveAmt)
      newAccMap = Map.adjust (A.deposit receiveAmt d (SwapInSettle sName)) accName accsMap
    in 
      return $ t { rateCap = Just newRcSwap, accounts = newAccMap }



-- TODO lookup check is not necessiary
performAction d t@TestDeal{rateSwap = Nothing, accounts = accsMap } (W.SwapReceive accName sName)
  = Left $ "Date:"++show d++"Can't receive swap at location:"++ show sName++"as no swap defined in deal"++ show (name t)
performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapReceive accName sName)
  = do
      acc <- lookupM accName accsMap
      rSwap <- lookupM sName rtSwap
      return $
        let 
          receiveAmt = max 0 $ HE.rsNetCash $ rSwap
          newRtSwap = Map.adjust (HE.receiveIRS d) sName rtSwap
          newAccMap = Map.adjust (A.deposit receiveAmt d (SwapInSettle sName)) accName accsMap
        in
          t { rateSwap = Just newRtSwap, accounts = newAccMap }

performAction d t@TestDeal{rateSwap = Nothing, accounts = accsMap } (W.SwapPay accName sName)
  = Left $ "Date:"++show d++"Can't pay swap at location:"++ show sName++"as no swap defined in deal"++ show (name t)
performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapPay accName sName)
  = do 
      acc <- lookupM accName accsMap
      rSwap <- lookupM sName rtSwap
      if (HE.rsNetCash (rtSwap Map.! sName)) < 0 then
        let 
           payoutAmt = negate $ HE.rsNetCash $ rSwap
           availBal = A.accBalance $ acc
           amtToPay = min payoutAmt availBal
           newRtSwap = Map.adjust (HE.payoutIRS d amtToPay) sName rtSwap
	    in 
	      do 
            newAccMap <- adjustM (A.draw d amtToPay (SwapOutSettle sName)) accName accsMap
            return $ t { rateSwap = Just newRtSwap, accounts = newAccMap }
      else
        return t


performAction d t@TestDeal{rateSwap = Nothing, accounts = accsMap } (W.SwapSettle accName sName)
  = Left $ "Date:"++show d++"Can't settle swap at location:"++ show sName++"as no swap defined in deal"++ show (name t)
performAction d t@TestDeal{rateSwap = Just rtSwap, accounts = accsMap } (W.SwapSettle accName sName)
  = do
      t2 <- performAction d t (W.SwapReceive accName sName)
      performAction d t2 (W.SwapPay accName sName)

performAction d t@TestDeal{ triggers = Nothing } (W.RunTrigger loc tNames)
  = Left $ "Date:"++show d++"Can't run trigger at location:"++ show loc++"as no trigger defined in deal"++ show (name t)
performAction d t@TestDeal{ triggers = Just trgM } (W.RunTrigger loc tNames)
  = do 
      triggerM <- lookupM loc trgM
      triggerList <- lookupVs tNames triggerM 
      tList <- mapM (testTrigger t d) triggerList
      return $
          let 
            newTrgMap = Map.fromList $ zip tNames tList
          in 
            t { triggers = Just (Map.insert loc newTrgMap trgM) }


performAction d t (W.Placeholder mComment) = return t 

performAction d t action =  Left $ "failed to match action>>"++show action++">>Deal"++show (name t)
