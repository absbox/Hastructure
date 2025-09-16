{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    (Amount,Rate,Dates,Period(..),Balance
    ,StartDate,EndDate,daysBetween,daysBetweenI
    ,Spread,Date
    ,paySeqLiabilities,prorataFactors
    ,afterNPeriod,Ts(..),periodsBetween
    ,periodRateFromAnnualRate
    ,Floor,Cap,TsPoint(..)
    ,toDate,toDates,genDates,nextDate
    ,getValOnByDate,getIntValOnByDate,sumValTs,subTsBetweenDates,splitTsByDate
    ,paySeqLiabilitiesAmt,getIntervalDays,getIntervalFactors
    ,zipWith8,zipWith9,zipWith10,zipWith11,zipWith12
    ,weightedBy, mkTs ,mkRateTs,paySeqLiabResi ,centiToDecimal
    ) where

import qualified Data.Time as T
import qualified Data.Time.Format as TF
import Data.List
-- import qualified Data.Scientific as SCI
import qualified Data.Map as M
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Aeson hiding (json)
-- import Text.Regex.TDFA
import Data.Fixed (Fixed(..), HasResolution,Centi, resolution)
import Data.Ratio
import Types
import Control.Lens
import Data.List.Lens
import Control.Lens.TH
import Data.Decimal
import Debug.Trace
debug = flip trace


periodRateFromAnnualRate :: Period -> IRate -> IRate
periodRateFromAnnualRate Annually annual_rate  = annual_rate
periodRateFromAnnualRate Monthly annual_rate  = annual_rate / 12
periodRateFromAnnualRate Quarterly annual_rate  = annual_rate / 4
periodRateFromAnnualRate SemiAnnually annual_rate  = annual_rate / 2
periodRateFromAnnualRate Daily annual_rate  = annual_rate / 365
periodRateFromAnnualRate Weekly annual_rate  = annual_rate / 52.143

addD :: Date -> T.CalendarDiffDays -> Date
addD d calendarMonth = T.addGregorianDurationClip T.calendarMonth d

getIntervalDays :: [Date] -> [Int]
getIntervalDays [] = []
getIntervalDays (d:ds) = zipWith daysBetweenI (init (d:ds)) ds


-- get fractional years from a set of dates
getIntervalFactors :: [Date] -> [Rate]
getIntervalFactors ds = (\x -> toRational x / 365) <$> getIntervalDays ds -- `debug` ("Interval Days"++show(ds))


centiToDecimal :: Centi -> Decimal
centiToDecimal c = Decimal 2 (round c * 100)

-- | 
prorataFactors :: [Balance] -> Balance -> [Balance]
prorataFactors bals amt =
  let 
    s = toRational $ sum bals
    amtToPay = toRational $ min s (toRational amt)
  in 
    case s of 
      0.0 -> replicate (length bals) 0.0
      _ -> let 
             weights = map (\x -> toRational x / s) bals 
             outPut = (\y -> fromRational (y * amtToPay)) <$> weights 
             eps = amt - sum outPut
           in 
             if eps == 0.00 then
                outPut
             else
                over (ix 0) (+ eps) outPut

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
afterNPeriod d i p =
  T.addGregorianMonthsClip ( months * i)  d
  where
    months = case p of
      Monthly -> 1
      Quarterly -> 3
      SemiAnnually -> 6
      Annually -> 12

periodsBetween :: T.Day -> T.Day -> Period -> Integer
periodsBetween t1 t2 p
  = case p of
      Weekly ->  div (T.diffDays t1 t2) 7
      Monthly -> _diff
      Annually -> div _diff 12
      Quarterly -> div _diff 4
  where
    _diff = T.cdMonths $ T.diffGregorianDurationClip t1 t2


mkTs :: [(Date,Rational)] -> Ts
mkTs [] = FloatCurve []
mkTs ps = FloatCurve [ TsPoint d v | (d,v) <- ps]


mkRateTs :: [(Date,IRate)] -> Ts
mkRateTs ps = IRateCurve [ TsPoint d v | (d,v) <- ps]


getValOnByDate :: Ts -> Date -> Balance
getValOnByDate (BalanceCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0

getIntValOnByDate :: Ts -> Date -> Int
getIntValOnByDate (IntCurve dps) d 
  = case find (\(TsPoint _d _) -> ( d >= _d )) (reverse dps)  of 
      Just (TsPoint _d v) -> v
      Nothing -> 0


splitTsByDate :: Ts -> T.Day -> (Ts, Ts)
splitTsByDate (BalanceCurve ds) d
  = case (findIndex (\(TsPoint _d _) -> _d > d ) ds) of
      Nothing -> (BalanceCurve ds, BalanceCurve [])
      Just idx -> (BalanceCurve l, BalanceCurve r)
                  where
                   (l,r) = splitAt idx ds

subTsBetweenDates :: Ts -> Maybe Date -> Maybe Date -> Ts
subTsBetweenDates (BalanceCurve vs) (Just sd) (Just ed)
  =  BalanceCurve $ filter(\(TsPoint x _) -> (x > sd) && (x < ed) ) vs
subTsBetweenDates (BalanceCurve vs) Nothing (Just ed)
  =  BalanceCurve $ filter(\(TsPoint x _) ->  x < ed ) vs
subTsBetweenDates (BalanceCurve vs) (Just sd) Nothing
  =  BalanceCurve $ filter(\(TsPoint x _) ->  x > sd ) vs

sumValTs :: Ts -> Amount
sumValTs (BalanceCurve ds) = foldr (\(TsPoint _ v) acc -> acc+v ) 0 ds


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


floatToFixed :: HasResolution a => Float -> Fixed a
floatToFixed x = y where
  y = MkFixed (round (fromInteger (resolution y) * x))

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
genDates start_day BiWeekly n = 
  [ T.addGregorianDurationClip (T.CalendarDiffDays 0 (toInteger i * 14)) start_day | i <- [1..n]] --`debug` ("Hit weekly")
genDates start_day Weekly n = 
  [ T.addGregorianDurationClip (T.CalendarDiffDays 0 (toInteger i * 7)) start_day | i <- [1..n]] --`debug` ("Hit weekly")
genDates start_day p n =
  [ T.addGregorianDurationClip (T.CalendarDiffDays (toInteger i*mul) 0) start_day | i <- [1..n]]
   where
     mul = case p of
       Monthly -> 1
       Quarterly -> 3
       SemiAnnually -> 6
       Annually -> 12
       _ -> error $ "Invalid period" ++ show p

nextDate :: Date -> Period -> Date
nextDate d p
  = T.addGregorianMonthsClip m d
    where
      m = case p of
        Monthly -> 1
        Quarterly -> 3
        SemiAnnually -> 6
        Annually -> 12
        _ -> 0
