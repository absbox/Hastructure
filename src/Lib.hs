{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Amount,Rate,Dates,Period(..),Balance
    ,StartDate,EndDate,daysBetween,daysBetweenI
    ,Spread,Date
    ,paySeqLiabilities,prorataFactors
    ,afterNPeriod,Ts(..),monthsBetween
    ,periodRateFromAnnualRate
    ,Floor,Cap,TsPoint(..)
    ,toDate,toDates,genDates,nextDate
    ,getValOnByDate,getIntValOnByDate,sumValTs,subTsBetweenDates,splitTsByDate
    ,tryGetValOnByDate,tryGetIntValOnByDate,trySumValTs,trySubTsBetweenDates,trySplitTsByDate
    ,paySeqLiabilitiesAmt,getIntervalDays,getIntervalFactors
    ,zipWith8,zipWith9,zipWith10,zipWith11,zipWith12
    ,weightedBy, mkTs ,mkRateTs,paySeqLiabResi ,centiToDecimal
    ) where

import qualified Data.Time as T
import qualified Data.Time.Format as TF
import Data.List
-- import qualified Data.Scientific as SCI
import qualified Data.Map as M ()
import qualified Data.IntSet as IntSet
import Language.Haskell.TH ()
import Data.Aeson.TH ()
import Data.Aeson.Types ()
import Data.Aeson ()
-- import Text.Regex.TDFA
import Data.Fixed (Centi)
import Data.Ratio ()
import Types
import Data.Decimal


periodRateFromAnnualRate :: Period -> IRate -> IRate
periodRateFromAnnualRate Annually annual_rate  = annual_rate
periodRateFromAnnualRate Monthly annual_rate  = annual_rate / 12
periodRateFromAnnualRate Quarterly annual_rate  = annual_rate / 4
periodRateFromAnnualRate SemiAnnually annual_rate  = annual_rate / 2
periodRateFromAnnualRate Daily annual_rate  = annual_rate / 365
periodRateFromAnnualRate Weekly annual_rate  = annual_rate / 52.143
periodRateFromAnnualRate BiWeekly annual_rate  = annual_rate / 26.0715


getIntervalDays :: [Date] -> [Int]
getIntervalDays [] = []
getIntervalDays (d:ds) = zipWith daysBetweenI (init (d:ds)) ds


-- get fractional years from a set of dates
getIntervalFactors :: [Date] -> [Rate]
getIntervalFactors ds = (\x -> toRational x / 365) <$> getIntervalDays ds -- `debug` ("Interval Days"++show(ds))


centiToDecimal :: Centi -> Decimal
centiToDecimal c = Decimal 2 (round c * 100)

-- | Allocate amt across balances pro-rata using the largest remainder
-- method: every allocation is non-negative and allocations sum exactly to
-- min (sum of balances) amt (to the cent). Residual cents go to the elements
-- with the largest fractional remainders, ties broken by index order.
-- Negative balances carry no weight (treated as zero) instead of producing
-- negative allocations (#322 review L3).
prorataFactors :: [Balance] -> Balance -> [Balance]
prorataFactors bals amt
  | totalCents <= 0 = replicate n 0
  | otherwise = map (toEnum . fromInteger) $ zipWith (+) baseAdds extraCents
  where
    n = length bals
    centList = max 0 . toInteger . fromEnum <$> bals
    totalCents = sum centList
    payCents = max 0 $ min totalCents (toInteger $ fromEnum amt)
    baseAdds = [ b * payCents `div` totalCents | b <- centList ]
    residual = payCents - sum baseAdds
    order = sortOn (negate . snd) $ zip [0..] [ b * payCents `mod` totalCents | b <- centList ]
    extraIdx = IntSet.fromList $ fst <$> take (fromInteger residual) order
    extraCents = [ if IntSet.member i extraIdx then 1 else 0 | i <- [0 .. n - 1] ]

-- 

paySeqLiabilities :: Balance -> [Balance] -> [(Balance,Balance)]
paySeqLiabilities startAmt liabilities =
  tail $ reverse $ foldl pay [(startAmt, 0)] liabilities
  where pay accum@((amt, _):xs) target = 
                         if amt >= target then
                            (amt-target, 0):accum
                         else
                            (0, target-amt):accum

-- Input: 1000, [100,200,300] -> [100,200,300]
-- Input: 100, [50,80] ->[50,50]
paySeqLiabilitiesAmt :: Balance -> [Balance] -> [Balance]
paySeqLiabilitiesAmt startAmt funds
  = zipWith (-) funds remainBals
    -- map (\(a,b) -> (a-b)) $ zip funds remainBals
  where 
    remainBals = map snd $ paySeqLiabilities startAmt funds 

paySeqLiabResi :: Amount -> [Balance] -> [Amount]
paySeqLiabResi startAmt funds
  = zipWith (-) funds allocatedAmts
  where 
    allocatedAmts = paySeqLiabilitiesAmt startAmt funds

afterNPeriod :: T.Day -> Integer -> Period -> T.Day
afterNPeriod d i p = advancePeriod i p d

-- | Advance a date by @i@ whole periods. Total over every 'Period'
-- constructor: day-based periods advance by a fixed number of days,
-- month-based periods by a clipped number of calendar months.
advancePeriod :: Integer -> Period -> Date -> Date
advancePeriod i Daily        = T.addGregorianDurationClip (T.CalendarDiffDays 0 i)
advancePeriod i Weekly       = T.addGregorianDurationClip (T.CalendarDiffDays 0 (7 * i))
advancePeriod i BiWeekly     = T.addGregorianDurationClip (T.CalendarDiffDays 0 (14 * i))
advancePeriod i Monthly      = T.addGregorianMonthsClip i
advancePeriod i Quarterly    = T.addGregorianMonthsClip (3 * i)
advancePeriod i SemiAnnually = T.addGregorianMonthsClip (6 * i)
advancePeriod i Annually     = T.addGregorianMonthsClip (12 * i)

-- | Number of whole calendar months between two dates, i.e. how many complete
-- months have elapsed from @t1@ to @t2@. Partial months are clipped, so e.g.
-- 2021-01-31 -> 2021-02-01 is 0 months. Returns a negative value when @t2@ is
-- earlier than @t1@ and 0 when both dates are equal.
monthsBetween :: Date -> Date -> Integer
monthsBetween t1 t2
  = T.cdMonths $ T.diffGregorianDurationClip t2 t1


mkTs :: [(Date,Rational)] -> Ts
mkTs [] = FloatCurve []
mkTs ps = FloatCurve [ TsPoint d v | (d,v) <- ps]


mkRateTs :: [(Date,IRate)] -> Ts
mkRateTs ps = IRateCurve [ TsPoint d v | (d,v) <- ps]


tsTypeName :: Ts -> String
tsTypeName (FloatCurve _) = "FloatCurve"
tsTypeName (BoolCurve _) = "BoolCurve"
tsTypeName (BalanceCurve _) = "BalanceCurve"
tsTypeName (LeftBalanceCurve _) = "LeftBalanceCurve"
tsTypeName (RatioCurve _) = "RatioCurve"
tsTypeName (ThresholdCurve _) = "ThresholdCurve"
tsTypeName (IRateCurve _) = "IRateCurve"
tsTypeName (FactorCurveClosed _ _) = "FactorCurveClosed"
tsTypeName (PricingCurve _) = "PricingCurve"
tsTypeName (PeriodCurve _) = "PeriodCurve"
tsTypeName (IntCurve _) = "IntCurve"

unsupportedCurve :: String -> String -> Ts -> Either ErrorRep a
unsupportedCurve fn expected ts
  = Left $ fn ++ ": expected " ++ expected ++ " but got " ++ tsTypeName ts

tryGetValOnByDate :: Ts -> Date -> Either ErrorRep Balance
tryGetValOnByDate (BalanceCurve dps) d
  = Right $ case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of
              Just (TsPoint _d v) -> v
              Nothing -> 0
tryGetValOnByDate ts _ = unsupportedCurve "getValOnByDate" "BalanceCurve" ts

getValOnByDate :: Ts -> Date -> Balance
getValOnByDate ts d = either error id (tryGetValOnByDate ts d)

tryGetIntValOnByDate :: Ts -> Date -> Either ErrorRep Int
tryGetIntValOnByDate (IntCurve dps) d
  = Right $ case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of
              Just (TsPoint _d v) -> v
              Nothing -> 0
tryGetIntValOnByDate ts _ = unsupportedCurve "getIntValOnByDate" "IntCurve" ts

getIntValOnByDate :: Ts -> Date -> Int
getIntValOnByDate ts d = either error id (tryGetIntValOnByDate ts d)

trySplitTsByDate :: Ts -> T.Day -> Either ErrorRep (Ts, Ts)
trySplitTsByDate (BalanceCurve ds) d
  = Right $ case (findIndex (\(TsPoint _d _) -> _d > d ) ds) of
              Nothing -> (BalanceCurve ds, BalanceCurve [])
              Just idx -> let (l,r) = splitAt idx ds in (BalanceCurve l, BalanceCurve r)
trySplitTsByDate ts _ = unsupportedCurve "splitTsByDate" "BalanceCurve" ts

splitTsByDate :: Ts -> T.Day -> (Ts, Ts)
splitTsByDate ts d = either error id (trySplitTsByDate ts d)

trySubTsBetweenDates :: Ts -> Maybe Date -> Maybe Date -> Either ErrorRep Ts
trySubTsBetweenDates (BalanceCurve vs) msd med
  = Right $ BalanceCurve $ filter (\(TsPoint x _) -> inRange x) vs
    where
      inRange x = maybe True (x >) msd && maybe True (x <) med
trySubTsBetweenDates ts _ _ = unsupportedCurve "subTsBetweenDates" "BalanceCurve" ts

subTsBetweenDates :: Ts -> Maybe Date -> Maybe Date -> Ts
subTsBetweenDates ts msd med = either error id (trySubTsBetweenDates ts msd med)

trySumValTs :: Ts -> Either ErrorRep Amount
trySumValTs (BalanceCurve ds) = Right $ foldr (\(TsPoint _ v) acc -> acc+v ) 0 ds
trySumValTs ts = unsupportedCurve "sumValTs" "BalanceCurve" ts

sumValTs :: Ts -> Amount
sumValTs ts = either error id (trySumValTs ts)


toDate :: String -> Date
toDate = TF.parseTimeOrError True TF.defaultTimeLocale "%Y%m%d"

toDates :: [String] -> [Date]
toDates ds = toDate <$> ds

zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
  = z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []

zipWith9 :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
zipWith9 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js)
  = z a b c d e f g h j : zipWith9 z as bs cs ds es fs gs hs js
zipWith9 _ _ _ _ _ _ _ _ _ _ = []

zipWith10 :: (a->b->c->d->e->f->g->h->i->j->k) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]
zipWith10 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js) (k:ks)
  = z a b c d e f g h j k: zipWith10 z as bs cs ds es fs gs hs js ks
zipWith10 _ _ _ _ _ _ _ _ _ _ _ = []

zipWith11 :: (a->b->c->d->e->f->g->h->i->j->k->l) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]->[l]
zipWith11 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js) (k:ks) (l:ls)
  = z a b c d e f g h j k l: zipWith11 z as bs cs ds es fs gs hs js ks ls
zipWith11 _ _ _ _ _ _ _ _ _ _ _ _ = []

zipWith12 :: (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]->[l]->[m]
zipWith12 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (j:js) (k:ks) (l:ls) (m:ms)
  = z a b c d e f g h j k l m: zipWith12 z as bs cs ds es fs gs hs js ks ls ms
zipWith12 _ _ _ _ _ _ _ _ _ _ _ _ _ = []



-- | given balances and weight, get sum weighted balance
weightedBy :: [Rational] -> [Rational] -> Rational
weightedBy ws vs 
  | sum_weights == 0 = 0
  | otherwise = sum ( zipWith (*) vs ws ) / sum_weights
  where 
    sum_weights = sum ws

-- | Given a start date and a end date, return number of days between(Integer)
daysBetween :: Date -> Date -> Integer 
daysBetween sd ed = fromIntegral (T.diffDays ed sd)

-- | Given a start date and a end date, return number of days between(Int)
daysBetweenI :: Date -> Date -> Int 
daysBetweenI sd ed = fromInteger $ T.diffDays ed sd

genDates :: Date -> Period -> Int -> [Date]
genDates start_day p n = [ advancePeriod (toInteger i) p start_day | i <- [1..n] ]

nextDate :: Date -> Period -> Date
nextDate d p = advancePeriod 1 p d
