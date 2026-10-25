{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DateUtil(
    yearCountFraction,genSerialDates,genSerialDatesTill,genSerialDatesTill2,subDates,sliceDates,SliceType(..)
    ,splitByDate,projDatesByPattern,monthsAfter,getIntervalFactorsDc
    ,daysInterval
)

    where 

import qualified Data.Time as T
import Data.List
import Data.Maybe ()
import qualified Data.Map as M ()
import qualified Data.Set as S
import Data.Ratio ((%))
import Data.Time ()
import Types
import Data.Ix
import Lib

import Control.Exception ()


-- http://www.deltaquants.com/day-count-conventions
-- https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm
yearCountFraction :: DayCount -> Date -> Date -> Rational 
yearCountFraction dc sd ed 
  = case dc of 
      DC_ACT_ACT -> if sameYear then 
                      _diffDays % daysOfYear syear
                    else
                      (sDaysTillYearEnd % (daysOfYear syear)) + (eDaysAfterYearBeg % (daysOfYear eyear)) + (pred _diffYears) 
                      -- `debug` ("<>"++show sDaysTillYearEnd++"<>"++show(daysOfYear syear) ++"<>"++show (daysOfYear eyear)++"<>"++ show eyear)

      DC_ACT_365F -> _diffDays % 365

      DC_ACT_360  -> _diffDays % 360

      DC_ACT_365A -> if has_leap_day then 
                       _diffDays % 366
                     else 
                       _diffDays % 365

      DC_ACT_365L -> if T.isLeapYear eyear then 
                       _diffDays % 366
                     else  
                       _diffDays % 365
      
      DC_NL_365 -> if has_leap_day then 
                     (pred _diffDays) % 365
                   else  
                     _diffDays % 365

      DC_30E_360 -> let
                      _sday = f31to30 sday
                      _eday = f31to30 eday
                      num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                    in 
                      num / 360  -- `debug` ("NUM->"++show num++"E S month"++show emonth++show smonth)
      
      DC_30Ep_360 -> let
                       _sday = f31to30 sday
                       (_eyear,_emonth,_eday) = T.toGregorian $
                                                    if eday==31 then 
                                                      T.addDays 1 ed
                                                    else
                                                      ed
                       __gapMonth = (toInteger $ _emonth - smonth) % 1
                       __diffYears = (toInteger $ _eyear - syear) % 1
                       num = toRational (_eday - _sday) + 30*__gapMonth + 360*__diffYears
                     in 
                       num / 360
      DC_30_360_ISDA -> let
                          _sday = f31to30 sday
                          _eday = if _sday>=30 && eday==31 then 
                                    30
                                  else 
                                    eday    
                          num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                        in 
                          num / 360
      -- 30/360 Bond basis , this was call 30E/360 ISDA by kalotay
      DC_30_360_German -> let
                            _sday = if sday==31 || (endOfFeb syear smonth sday) then 
                                      30 -- `debug` ("German eof start if>> "++ show (endOfFeb syear smonth sday)++show syear ++show smonth++show sday)
                                    else 
                                      sday  
                                    -- `debug` ("German eof start else "++ show (endOfFeb syear smonth sday)++show syear ++show smonth++show sday)
                            _eday = if eday==31 || (endOfFeb eyear emonth eday) then 
                                      30
                                    else
                                      eday    
                                    -- `debug` ("German eof end "++ show (endOfFeb eyear emonth eday)++show eyear++show emonth++show eday)
                            num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears -- `debug` ("German"++show(_sday)++"<>"++show _eday)
                          in 
                            num / 360
      DC_30_360_US -> let
                        _sday = if (endOfFeb syear smonth sday) || sday==31 then 
                                  30
                                else 
                                  sday  
                        _eday = if (eday==31 && sday >= 30)||(endOfFeb eyear emonth eday) && (endOfFeb syear smonth sday)  then 
                                  30
                                else
                                  eday 
                        num = toRational (_eday - _sday) + 30*_gapMonth + 360*_diffYears
                      in 
                        num / 360
      _ -> error $ "DayCount not supported" ++ show dc
      -- https://www.iso20022.org/15022/uhb/mt565-16-field-22f.htm

    where 
      daysOfYear y = if T.isLeapYear y then 366 else 365
      f31to30 d = if d==31 then 
                    30
                  else
                    d
      endOfFeb y m d = if T.isLeapYear y then 
                         (m==2) && d == 29
                       else 
                         (m==2) && d == 28
      sameYear = syear == eyear
      has_leap_day 
        = case (sameYear,sLeap,eLeap) of                   
            (True,False,False) -> False 
            (True,True,_) -> inRange (sd,ed) (T.fromGregorian syear 2 29)
            _ -> let 
                   _leapDays = [  T.fromGregorian _y 2 29   |  _y <- range (syear,eyear) , T.isLeapYear _y ]
                 in   
                   any (inRange (sd,ed)) _leapDays

      _diffYears = (eyear - syear) % 1 -- Ratio Integer
      _gapDay =   toInteger (eday - sday) % 1
      _gapMonth = toInteger (emonth - smonth) % 1
      sDaysTillYearEnd = succ $ T.diffDays (T.fromGregorian syear 12 31) sd
      eDaysAfterYearBeg = T.diffDays ed (T.fromGregorian eyear 1 1)
      _diffDays = toInteger $ T.diffDays ed sd
      sLeap = T.isLeapYear syear
      eLeap = T.isLeapYear eyear
      (syear,smonth,sday) = T.toGregorian sd 
      (eyear,emonth,eday) = T.toGregorian ed 

genSerialDates :: DatePattern -> CutoffType -> Date -> Int -> Dates
genSerialDates dp ct sd num
  = take num $ 
      filter ftFn $ 
      case dp of 
        MonthEnd -> 
                [T.fromGregorian yearRange (fst __md) (snd __md) | yearRange <- [_y..(_y+yrs)]
                                                                 ,__md <- monthEnds yearRange ]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
        QuarterEnd -> 
                [T.fromGregorian yearRange __m __d | yearRange <- [_y..(_y+yrs)]
                                                   ,(__m,__d) <- quarterEnds]
                where 
                  yrs = fromIntegral $ div num 4 + 1                   
        YearEnd -> 
                [T.fromGregorian yearRange 12 31 | yearRange <- [_y..(_y+(toInteger num))]]
        YearFirst ->
                [T.fromGregorian yearRange 1 1 | yearRange <- [_y..(_y+(toInteger num))]]
        MonthFirst ->
                [T.fromGregorian yearRange monthRange 1 | yearRange <- [_y..(_y+yrs)]
                                                        , monthRange <- [1..12]]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
        QuarterFirst ->
                [T.fromGregorian yearRange __m 1 | yearRange <- [_y..(_y+yrs)]
                                                 ,__m <- [3,6,9,12]]
                where 
                  yrs = fromIntegral $ div num 4 + 1                   
        MonthDayOfYear m d -> 
                [T.fromGregorian yearRange m d | yearRange <- [_y..(_y+(toInteger num))]]
        DayOfMonth d ->
                [T.fromGregorian yearRange monthRange d | yearRange <- [_y..(_y+yrs)]
                                                        , monthRange <- [1..12]]
                where 
                  yrs = fromIntegral $ div num 12 + 1                   
        Weekday wday -> 
                [T.addDays (toInteger _n * 7) startDay | _n <- [0..]]  
                where 
                  dOfWeek = toEnum wday::T.DayOfWeek
                  startDay = T.firstDayOfWeekOnAfter dOfWeek sd
                  
        CustomDate ds -> ds
        EveryNMonth d n -> 
                d:[ T.addGregorianDurationClip (T.CalendarDiffDays ((toInteger _n)*(toInteger n)) 0) d | _n <- [1..num] ]
        SingletonDate d -> [d]

      where 
        quarterEnds = [(3,31),(6,30),(9,30),(12,31)]
        monthEnds y = 
          if T.isLeapYear y then
            [(1,31),(2,29),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
          else
            [(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]
        (_y,_m,_d) = T.toGregorian sd 
        ftFn = if ct == Inc then
                 (>= sd)
               else
                 (> sd)

genSerialDatesTill:: Date -> DatePattern -> Date -> Dates 
genSerialDatesTill sd ptn ed 
  = filter (<= ed) $ genSerialDates ptn Inc sd (fromInteger (succ num))  --`debug` ("Num"++show num)
    where 
      (sy,sm,sday) = T.toGregorian sd 
      (ey,em,eday) = T.toGregorian ed 
      T.CalendarDiffDays cdM cdD = T.diffGregorianDurationRollOver ed sd 
      num = case ptn of 
              MonthEnd -> cdM
              QuarterEnd ->  div cdM 3
              YearEnd ->  div cdM 12
              MonthFirst -> cdM 
              QuarterFirst-> div cdM 3
              YearFirst->  div cdM 12
              MonthDayOfYear _m _d -> div cdM 12 -- T.MonthOfYear T.DayOfMonth
              DayOfMonth _d -> cdM -- T.DayOfMonth 
              CustomDate ds -> 2 + toInteger (length ds)
              EveryNMonth _d _n -> div cdM (toInteger _n)
              Weekday _d -> cdM * 4
              SingletonDate _d -> if _d <= ed then 1 else 0
              _ -> error $ "failed to match" ++ show ptn
              -- DayOfWeek Int -> -- T.DayOfWeek 

genSerialDatesTill2 :: RangeType -> Date -> DatePattern -> Date -> Dates
genSerialDatesTill2 rt sd dp ed 
  = case (rt, not (null _r) && head _r==sd, not (null _r) && last _r==ed) of 
      (II,True,True) -> _r
      (II,True,False) -> _r ++ [ed]
      (II,False,True)-> sd:_r 
      (II,False,False)-> sd:_r ++ [ed] 
      (EI,True,True) -> tail _r 
      (EI,True,False) -> tail _r ++ [ed]
      (EI,False,True) -> _r 
      (EI,False,False) -> _r ++ [ed]
      (IE,True,True) -> init _r 
      (IE,True,False) -> _r 
      (IE,False,True) -> sd:init _r
      (IE,False,False) -> sd:_r 
      (EE,True,True) -> init $ tail _r 
      (EE,True,False) -> tail _r 
      (EE,False,True) -> init _r
      (EE,False,False) -> _r 
      (NO_IE,_,_) -> _r
    where 
      _r = case dp of 
             -- YearFirst -> throw $ userError "YearFirst not supported in genSerialDatesTill2"
             AllDatePattern dps -> concat [ genSerialDatesTill sd _dp ed | _dp <- dps ]
             StartsExclusive d _dp ->  filter (> d)  $ genSerialDatesTill2 rt sd _dp ed

             StartsAt Exc d _dp ->  filter (> d)  $ genSerialDatesTill2 rt sd _dp ed
             StartsAt Inc d _dp ->  filter (>= d)  $ genSerialDatesTill2 rt sd _dp ed
             EndsAt Exc d _dp -> filter (< d)  $ genSerialDatesTill2 rt sd _dp ed
             EndsAt Inc d _dp -> filter (<= d)  $ genSerialDatesTill2 rt sd _dp ed
             
             Exclude _d _dps ->
                 let 
                   a = S.fromList $ genSerialDatesTill2 rt sd _d ed
                   b = S.fromList $ genSerialDatesTill2 rt sd (AllDatePattern _dps) ed
                 in 
                   sort $ S.toList $ S.difference a b
             OffsetBy _dp _n -> [ T.addDays (toInteger _n) _d   | _d <- genSerialDatesTill2 rt sd _dp ed ]
             _ -> genSerialDatesTill sd dp ed -- maybe sd/ed in _r


subDates :: RangeType -> Date -> Date -> [Date] -> [Date]
subDates rt sd ed ds 
  = case rt of 
      II -> filter (\x -> x >= sd && x <= ed ) ds 
      EI -> filter (\x -> x > sd && x <= ed ) ds
      IE -> filter (\x -> x >= sd && x < ed ) ds
      EE -> filter (\x -> x > sd && x < ed ) ds
      NO_IE -> error "Need to specify II/EI/EE/IE when subset dates vector "

data SliceType = SliceAfter Date 
               | SliceOnAfter Date 
               | SliceAfterKeepPrevious Date
               | SliceOnAfterKeepPrevious Date

sliceDates :: SliceType -> [Date] -> [Date] 
sliceDates st ds =
    case st of 
      SliceAfter d -> filter (> d) ds
      SliceOnAfter d -> filter (>= d) ds
      SliceAfterKeepPrevious d -> 
          case findIndex (> d) ds of
            Just idx -> snd $ splitAt (pred idx) ds
            Nothing -> [] 
      SliceOnAfterKeepPrevious d -> 
          case findIndex (>= d) ds of
            Just idx -> snd $ splitAt (pred idx) ds
            Nothing -> [] 


projDatesByPattern :: DatePattern -> Date -> Date -> Dates   --TODO to be replace by generateDateSeries
projDatesByPattern dp sd ed
  = let 
      (T.CalendarDiffDays cdm cdd) = T.diffGregorianDurationClip ed sd
      num = case dp of
              MonthEnd -> cdm + 1
              QuarterEnd -> div cdm 3 + 1 -- `debug` ("cdm"++show cdm)
              YearEnd  -> div cdm 12 + 1
              MonthFirst -> cdm + 1
              QuarterFirst -> div cdm 3 + 1
              YearFirst -> div cdm 12 + 1
              MonthDayOfYear _ _ -> div cdm 12 + 1
              DayOfMonth _ -> cdm + 1
    in 
      genSerialDates dp Inc sd (fromInteger num)

splitByDate :: TimeSeries a => [a] -> Date -> SplitType -> ([a],[a])
splitByDate xs d st 
  = case st of 
      EqToLeft ->  span (\x -> getDate x <= d) xs
      EqToRight -> span (\x -> getDate x < d) xs
      EqToLeftKeepOne -> 
          case findIndex (\x -> getDate x >= d ) xs of 
            Just idx -> splitAt (pred idx) xs -- `debug` ("split with "++show (pred idx)++">>"++show (length xs))
            Nothing -> (xs,[])
     -- EqToRightKeepOne -> 
     --     case findIndex (\x -> (getDate x) >= d ) xs of 
     --       Just idx -> splitAt (pred idx) xs -- `debug` ("split with "++show (pred idx)++">>"++show (length xs))
     --       Nothing -> (xs,[])

     -- EqToLeftKeepOnes -> 
     --     case findIndices (\x -> (getDate x) <= d) xs of
     --       [] -> (xs,[])
     --       inds -> 

monthsAfter :: Date -> Integer -> Date
monthsAfter d n = T.addGregorianDurationClip (T.CalendarDiffDays n 0) d

getIntervalFactorsDc :: DayCount -> [Date] -> [Rate]
getIntervalFactorsDc dc ds 
  = zipWith (yearCountFraction dc) (init ds) (tail ds)

daysInterval :: [Date] -> [Integer]
daysInterval ds = zipWith daysBetween (init ds) (tail ds)
