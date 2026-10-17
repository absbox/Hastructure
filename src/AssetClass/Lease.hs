{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AssetClass.Lease
  (Lease(..),projCashflow,updateOriginDate,calcEndDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as AP
import Asset
import Types hiding (getOriginDate)
import Lib
import Util
import DateUtil

import qualified Data.Map as Map
import Data.List
import Data.Aeson hiding (json)
import Data.Decimal
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Data.Maybe
import AssetClass.AssetBase
import qualified Analytics as AN

import Control.Lens hiding (element)
import Control.Lens.TH

import Debug.Trace
import qualified Assumptions as A
debug = flip trace

type PeriodAmount = Balance
type CapRate = Rate
type RentChangeRate = Rate
type RentChangeCurve = Ts
type TermChangeRate = Rate
type DayGap = Int
type LastAccuredDate = Date


getNewRental :: AP.LeaseAssetRentAssump -> Date -> Date -> LeaseRateCalc -> (AP.LeaseAssetRentAssump, LeaseRateCalc)
-- by day rate
getNewRental (AP.BaseAnnualRate r) sd ed (ByDayRate dr dp) 
  = (AP.BaseAnnualRate r
    , ByDayRate (mulBR dr (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)) dp)
getNewRental (AP.BaseCurve rc) sd ed (ByDayRate dr dp) 
  = (AP.BaseCurve rc
    , ByDayRate (mulBR dr (1 + yearCountFraction DC_ACT_365F sd ed * getValByDate rc Exc ed)) dp)
getNewRental (AP.BaseByVec rs) sd ed (ByDayRate dr dp) 
  = let
      (newDr,nextRs) = case Data.List.uncons rs of 
                         Just (r,_rs) -> (mulBR dr (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)
                                          , _rs)
                         Nothing -> (dr,[0.0])
    in
      (AP.BaseByVec nextRs, ByDayRate newDr dp)

-- by period rental
getNewRental (AP.BaseAnnualRate r) sd ed (ByPeriodRental rental per) 
  = (AP.BaseAnnualRate r
    , ByPeriodRental (mulBR rental (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)) per)
getNewRental (AP.BaseCurve rc) sd ed (ByPeriodRental rental per) 
  = (AP.BaseCurve rc
    , ByPeriodRental (mulBR rental (1 + yearCountFraction DC_ACT_365F sd ed * (fromRational (getValByDate rc Exc ed)))) per)
getNewRental (AP.BaseByVec rs) sd ed (ByPeriodRental rental per)
  = let
      (newRental,nextRs) = case Data.List.uncons rs of 
                             Just (r,_rs) -> (mulBR rental (1 + yearCountFraction DC_ACT_365F sd ed * fromRational r)
                                              , _rs)
                             Nothing -> (rental,[0.0])
    in
      (AP.BaseByVec nextRs, ByPeriodRental newRental per)

calcEndDate :: Date -> Int -> LeaseRateCalc -> Date 
calcEndDate sd periods (ByDayRate _ dp) = case genSerialDates dp Exc sd periods of 
                                            [] -> sd 
                                            ds -> last ds
calcEndDate sd periods (ByPeriodRental _ per) = case genDates sd per periods of 
                                                  [] -> sd 
                                                  ds -> last ds

calcGapDays :: AP.LeaseAssetGapAssump -> Date -> Int
calcGapDays (AP.GapDays days) _ = days
calcGapDays (AP.GapDaysByCurve ts) d = round $ fromRational $ getValByDate ts Exc d 

-- ^ Generate next lease with new rental / term changes/ day gap
nextLease :: Lease -> (AP.LeaseAssetRentAssump, TermChangeRate, DayGap) -> (Lease, Date ,(AP.LeaseAssetRentAssump, TermChangeRate, DayGap))
nextLease l@(RegularLease (LeaseInfo sd ot rental ob) bal rt _) (rAssump,tc,gd) 
  = let
        leaseEndDate = case getPaymentDates l 0 of [] -> sd; ds -> last ds
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate

        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = calcEndDate nextStartDate ot rental
        (newRassump, nextRental) = getNewRental rAssump sd nextStartDate rental
        newBal =  -1
    in 
      (RegularLease (LeaseInfo nextStartDate nextOriginTerm nextRental ob) 
                    newBal nextOriginTerm Current
      ,nextEndDate
      ,(newRassump,tc,gd)
      )

nextLease l@(StepUpLease (LeaseInfo sd ot rental ob) lsteupInfo bal rt _) (rAssump,tc,gd) 
  = let 
        leaseEndDate = case getPaymentDates l 0 of [] -> sd; ds -> last ds
        nextStartDate = T.addDays (succ (toInteger gd)) leaseEndDate -- `debug` ("Gap Day ->"++ show gd)
        nextOriginTerm = round $ mulIR ot (1+tc) 
        nextEndDate = calcEndDate nextStartDate ot rental
        (newRassump, nextRental) = getNewRental rAssump sd nextStartDate rental
        newBal = -1
    in
      (StepUpLease (LeaseInfo nextStartDate nextOriginTerm nextRental ob) 
                    lsteupInfo newBal nextOriginTerm Current
      ,nextEndDate
      ,(newRassump,tc,gd)
      ) --  `debug` ("leaseEndDate>>"++show leaseEndDate++">>>"++show (succ (toInteger gd)))

-- | create a new lease base on the lease in 1st argument, with new rental/term, a gap days, till the end date
nextLeaseTill :: Lease -> (AP.LeaseAssetRentAssump, TermChangeRate, DayGap) -> Date -> AP.LeaseEndType -> [Lease] -> [Lease]
nextLeaseTill l (rsc,tc,mg) lastDate (AP.CutByDate ed) accum 
  | lastDate >= ed = accum 
  | otherwise = nextLeaseTill new_lease newAssump new_lastDate (AP.CutByDate ed) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate, newAssump) = nextLease l (rsc,tc,mg)

nextLeaseTill l (rsc,tc,mg) lastDate (AP.StopByExtTimes n) accum 
  | n == 0 = accum 
  | otherwise = nextLeaseTill new_lease newAssump new_lastDate (AP.StopByExtTimes (pred n)) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate, newAssump) = nextLease l (rsc,tc,mg) 

nextLeaseTill l (rsc,tc,mg) lastDate (AP.EarlierOf ed n) accum 
  | lastDate >= ed = accum 
  | n == 0 = accum
  | otherwise = nextLeaseTill new_lease newAssump new_lastDate (AP.EarlierOf ed (pred n)) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate, newAssump) = nextLease l (rsc,tc,mg)

nextLeaseTill l (rsc,tc,mg) lastDate (AP.LaterOf ed n) accum 
  | lastDate >= ed && n == 0 = accum 
  | otherwise = nextLeaseTill new_lease newAssump new_lastDate (AP.LaterOf ed (pred n)) (accum++[new_lease])
                where 
                 (new_lease,new_lastDate, newAssump) = nextLease l (rsc,tc,mg)

-- ^ calculate the daily rate for a step up lease
calcPmts :: LeaseStepUp -> [Rate] -> Amount -> Either String [Amount] 
calcPmts (FlatRate _r) fs amt = Right (scanl mulBR amt (replicate (length fs) _r))
calcPmts (ByFlatAmount _amt) fs amt = Right (scanl (+) amt (replicate (length fs) _amt))
calcPmts (ByRateCurve rs) fs amt 
  | length rs /= length fs = Left "ByRateCurve: the rate curve should be the same length as remain pay dates"
  | otherwise = Right $ scanl mulBR amt rs
calcPmts (ByAmountCurve amts) fs amt 
  | length amts /= length fs = Left "ByAmountCurve: the rate curve should be the same length as remain pay dates"
  | otherwise = Right $ scanl (+) amt amts

-- ^ return a lease contract with opening balance and a payment cashflow on each payment date
patchBalance :: Lease -> Either String (Lease,[Amount]) 
patchBalance l@(RegularLease (LeaseInfo sd ot (ByDayRate dr dp) ob) bal rt st)
  = let 
      cf_dates = sd:getPaymentDates l 0
      pmts = lastN rt $ [ fromRational (mulBInt dr ds) | ds <- getIntervalDays cf_dates ]
      new_bal = sum pmts 
    in
      Right (RegularLease (LeaseInfo sd ot (ByDayRate dr dp) ob) new_bal rt st, pmts)

patchBalance l@(RegularLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) bal rt st)
  = let 
      -- cf_dates = lastN (succ rt) $ getPaymentDates l 0
      -- intervals = daysInterval cf_dates
      pmts = lastN rt $ replicate ot rental
      new_bal = sum pmts -- `debug` ("cf_date" ++ show cf_dates)
    in 
      do 
        return (RegularLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) new_bal rt st, pmts) -- `debug` ("daily payments" ++ show pmts)


patchBalance l@(StepUpLease (LeaseInfo sd ot (ByDayRate dr p) ob) lsu bal rt st)
  = let 
      cfDates = sd:getPaymentDates l 0
      intervals = daysInterval cfDates
      factors = replicate (pred ot) 1.0
    in 
      do 
        dailyRentals <- calcPmts lsu factors dr
        let pmts = lastN rt $ [ fromRational (mulBInteger r d) | (d,r) <- zip intervals dailyRentals ] 
        let new_bal = sum pmts -- `debug` ("cf_date" ++ show cf_dates)
        return (StepUpLease (LeaseInfo sd ot (ByDayRate dr p) ob) lsu new_bal rt st, pmts) -- `debug` ("daily payments" ++ show pmts)

patchBalance l@(StepUpLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) lsu bal rt st)
  = let 
      factors = replicate (pred ot) 1.0
    in 
      do 
        periodRentals <- calcPmts lsu factors rental
        let pmts = lastN rt periodRentals
        let new_bal = sum pmts
        return (StepUpLease (LeaseInfo sd ot (ByPeriodRental rental per) ob) lsu new_bal rt st, pmts) -- `debug` ("daily payments" ++ show pmts)


allocDefaultToLeaseFlow :: [Rate] -> (Rate,Balance) -> [CF.TsRow] -> [CF.TsRow] -> [CF.TsRow]
-- allocDefaultToLeaseFlow :: [Decimal] -> (Decimal,Decimal) -> [CF.TsRow] -> [CF.TsRow] -> [CF.TsRow]
allocDefaultToLeaseFlow defaultRates (begFactor,begBal) rs [] = reverse rs
allocDefaultToLeaseFlow (defaultRate:defaultRates) (begFactor,begBal) rs (txn@(CF.LeaseFlow d b r def):txns)
  = let 
      defaultAmt = mulBR begBal defaultRate
      nextFactor = begFactor * (1-defaultRate)
      newRental = mulBR r nextFactor
      rentalDiff = r - newRental
      nextBal = (begBal - rentalDiff - newRental) -- TODO: hardcode to fix rounding issue
    in 
      allocDefaultToLeaseFlow defaultRates (nextFactor,nextBal) ((CF.LeaseFlow d nextBal newRental rentalDiff):rs) txns

calcDefaultRates :: Rate -> CF.CashFlowFrame -> [Rate]
calcDefaultRates r cf
  = let 
      -- cfBegDate:cfDates = CF.getAllDatesCashFlowFrame cf
      ds = CF.getAllDatesCashFlowFrame cf
    in
      Util.toPeriodRateByInterval r <$> getIntervalDays ds 

applyDefaults :: Maybe AP.LeaseDefaultType -> (CF.CashFlowFrame,[CF.CashFlowFrame]) -> ([CF.TsRow],[[CF.TsRow]])
applyDefaults Nothing (CF.CashFlowFrame _ txn1,cfs) = (txn1,view CF.cashflowTxn <$> cfs)
-- applyDefaults (Just (AP.DefaultByContinuation r)) (CF.CashFlowFrame _ txn1,cfs)
--  = (txn1,(view CF.cashflowTxn) <$> cfs)
applyDefaults (Just (AP.DefaultByTermination r)) (cf1,cfs)
 = let 
     cf1Factors = calcDefaultRates r cf1
     cfsFactors::[[Rate]] = calcDefaultRates r <$> cfs 
   in 
      (allocDefaultToLeaseFlow cf1Factors (1.0, (CF.getBegBalCashFlowFrame cf1)) [] (view CF.cashflowTxn cf1) 
        , (\(fs,cf) -> allocDefaultToLeaseFlow fs (1.0, (CF.getBegBalCashFlowFrame cf)) [] (view CF.cashflowTxn cf)) <$> (zip cfsFactors cfs)
      )

applyDefaults (Just (AP.DefaultByContinuation r)) (cf1,cfs)
  = let 
      cf1Defaults = calcDefaultRates r cf1
      cfsDefaults::[[Rate]] = calcDefaultRates r <$> cfs

      cf1Factor = foldr (*) 1.0 $ (1 -) <$> cf1Defaults
      cfsFactors = (\df -> foldr (*) 1.0  ((1 -) <$> df)) <$> cfsDefaults

      cfFactors = cf1Factor : (init cfsFactors)

      cfs' = zipWith CF.splitCf cfsFactors cfs -- `debug` ("Cfs"++  show (cfsFactors))
   in 
      (allocDefaultToLeaseFlow cf1Defaults (1.0, (CF.getBegBalCashFlowFrame cf1)) [] (view CF.cashflowTxn cf1)
        , (\(fs,cf) -> allocDefaultToLeaseFlow fs (1.0, (CF.getBegBalCashFlowFrame cf)) [] (view CF.cashflowTxn cf)) <$> (zip cfsDefaults cfs')
      )


instance Asset Lease where 
    calcCashflow l d _ =
      do 
        (l',pmts) <- patchBalance l
        let bal = getCurrentBal l' -- `debug` ("payments"++ show pmts)
        let pDates = lastN (getRemainTerms l) $ getPaymentDates l 0 
        let bals = tail $ scanl (-) bal pmts  -- `debug` ("pDates "++ show pDates)
        let defaults = replicate (length pDates) 0.0 -- `debug` ("bals"++ show bals++ ">> d"++ show d)
        return $ CF.CashFlowFrame (case bals of [] -> bal; (b:_) -> b ,max d (getOriginDate l), Nothing) $ cutBy Inc Future d (zipWith4 CF.LeaseFlow pDates bals pmts defaults)

    getOriginInfo (StepUpLease lInfo lsteupInfo bal rt st) =  lInfo
    getOriginInfo (RegularLease lInfo bal rt st) = lInfo
      
    getOriginDate (StepUpLease (LeaseInfo sd _ _ _) _ _ _ _) = sd
    getOriginDate (RegularLease (LeaseInfo sd _ _ _) _ _ _)  = sd

    getPaymentDates l ot
      = case originRental (getOriginInfo l) of
          ByDayRate _ dp -> genSerialDates dp Exc (getOriginDate l) (ot + getTotalTerms l)
          ByPeriodRental _ per -> genDates (getOriginDate l) per (ot + getTotalTerms l)
    
    getRemainTerms (StepUpLease _ _ _ rt _) = rt
    getRemainTerms (RegularLease _ _ rt _)  = rt

    getTotalTerms (RegularLease (LeaseInfo _ ot _ _) _ _ _) = ot
    getTotalTerms (StepUpLease (LeaseInfo _ ot _ _) _ _ _ _) = ot
    
    updateOriginDate (StepUpLease (LeaseInfo sd ot rental ob) lsu bal rt st) nd 
      = StepUpLease (LeaseInfo nd ot rental ob) lsu bal rt st
    updateOriginDate (RegularLease (LeaseInfo sd ot rental ob) bal rt st) nd 
      = RegularLease (LeaseInfo nd ot rental ob) bal rt st
      
    -- resetToOrig (StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal rt st) 
    --   = fst . patchBalance $ StepUpLease (LeaseInfo sd ot dp dr ob) lsu bal ot st
    -- resetToOrig (RegularLease (LeaseInfo sd ot dp dr ob) bal rt st) 
    --   = fst . patchBalance $ RegularLease (LeaseInfo sd ot dp dr ob) bal ot st

    projCashflow l asOfDay (AP.LeaseAssump mDefault gapAssump rentAssump endType,_,_) mRates
      = let 
          pdates = getPaymentDates l 0  -- `debug` ("8")-- `debug` ("RCURVE"++show rcCurve)
          -- get the gap days between leases
          pickGapDays (AP.GapDays days) = days
          pickGapDays (AP.GapDaysByCurve cv) = getIntValOnByDate cv asOfDay 
        
          newLeases = nextLeaseTill 
                        l
                        (rentAssump , 0.0 , pickGapDays gapAssump) 
                        (case pdates of [] -> getOriginDate l; _ -> last pdates) 
                        endType
                        []
          stressRentals  = 0
        in
          do
            currentCf <- calcCashflow l asOfDay mRates
            newCfs <- sequenceA [ calcCashflow l asOfDay mRates | l <- newLeases ] --  `debug` ("Current CF\n "++ show currentCf)
            let (curCf,newTxns) = applyDefaults mDefault (currentCf, newCfs)
            -- let allTxns = view CF.cashflowTxn currentCf ++ (concat $ (view CF.cashflowTxn) <$> newCfs)
            let allTxns = curCf ++ concat newTxns
            let begBal = CF.buildBegBal allTxns
            return $ (CF.CashFlowFrame (begBal,max asOfDay (getOriginDate l),Nothing) allTxns, Map.empty)  
        

    projCashflow a b c d = Left $ "Failed to match when proj lease with assumption >>" ++ show a ++ show b ++ show c ++ show d
    
    getCurrentBal l = case l of 
                        StepUpLease _ _ bal _ _ -> bal
                        RegularLease _ bal _ _-> bal

    -- getOriginRate (StepUpLease (LeaseInfo _ _ _ dr _) _ _ _ _) = fromRational $ toRational dr
    -- getOriginRate (RegularLease (LeaseInfo _ _ _ dr _) _ _ _) = fromRational $ toRational dr
    getOriginRate _ = 0.0

    isDefaulted (StepUpLease _ _ _ rt Current) = False
    isDefaulted (RegularLease _ _  rt Current) = False
    isDefaulted _ = True

    getOriginBal l = 
      let 
            _sd = case l of 
                RegularLease (LeaseInfo sd _ _ _) bal _ _ -> sd 
                StepUpLease (LeaseInfo sd _ _ _) _ bal _ _  -> sd 
      in  
        case calcCashflow l _sd Nothing of
            Right (CF.CashFlowFrame _ []) -> 0
            Right (CF.CashFlowFrame _ (t:_)) -> CF.mflowBegBalance t
            Left _ -> 0

    splitWith (RegularLease (LeaseInfo sd ot dr ob) bal rt st ) rs
      = [ RegularLease (LeaseInfo sd ot dr ob) (mulBR bal ratio) rt st | ratio <- rs ] 
    splitWith (StepUpLease (LeaseInfo sd ot dr ob) stup bal rt st ) rs
      = [ StepUpLease (LeaseInfo sd ot dr ob) stup (mulBR bal ratio) rt st | ratio <- rs]

