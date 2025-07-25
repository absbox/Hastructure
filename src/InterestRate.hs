{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module InterestRate
  (ARM(..),RateType(..),runInterestRate2,runInterestRate,UseRate(..)
  ,getRateResetDates,getDayCount,calcInt, calcIntRate,calcIntRateCurve
  ,getSpread,_getSpread,calcIntRates)
  
  where

import Language.Haskell.TH
import Data.Aeson       hiding (json)
import Data.Aeson.TH
import Data.Maybe
import Data.Fixed
import GHC.Generics
import DateUtil
import Data.Decimal

import Types
import Util
import Lib

import Debug.Trace
debug = flip trace

type InitPeriod = Int 
type PeriodicCap = Maybe Spread
type LifetimeCap = Maybe IRate
type PaymentCap = Maybe Balance
type RateFloor = Maybe IRate
type RateCap = Maybe IRate
type InitCap = Maybe IRate
type ResetDates = [Date]
type StartRate = IRate

data RateType = Fix DayCount IRate
              | Floater DayCount Index Spread IRate DatePattern RateFloor RateCap (Maybe (RoundingBy IRate))
              deriving (Show,Generic,Eq,Ord)

getDayCount :: RateType -> DayCount
getDayCount (Fix dc _) = dc
getDayCount (Floater dc _ _ _ _ _ _ _ ) = dc

_getSpread :: RateType -> Maybe Spread
_getSpread (Fix _ _) = Nothing
_getSpread (Floater _ _ spd _ _ _ _ _) = Just spd

data ARM = ARM InitPeriod InitCap PeriodicCap LifetimeCap RateFloor
         | OtherARM
         deriving (Show,Generic,Eq,Ord)

getRateResetDates :: Date -> Date -> Maybe RateType -> Dates
getRateResetDates _ _ Nothing = []
getRateResetDates _ _ (Just (Fix _ _)) = []
getRateResetDates sd ed (Just (Floater _ _ _ _ dp _ _ _)) = genSerialDatesTill2 NO_IE sd dp ed 

runInterestRate :: ARM -> StartRate -> RateType -> ResetDates -> Ts -> [IRate]
runInterestRate (ARM ip icap pc lifeCap floor) sr (Floater _ _ spd _ _ _ _ mRoundBy) resetDates rc
  = sr:cappedRates
    where 
      fr:rrs = (spd +) . fromRational <$> getValByDates rc Inc resetDates
      firstRate 
        | isNothing icap = fr
        | (sr + fromMaybe 0 icap) <= fr = sr + fromMaybe 0 icap
        | otherwise = fr
      rounder = roundingByM mRoundBy
      restRates = tail $
                    scanl 
                      (\lastRate idxRate -> 
                          if isNothing pc then -- periodic cap
                            rounder idxRate
                          else
                            if lastRate + (fromMaybe 0 pc) <= idxRate then 
                              rounder $ lastRate + (fromMaybe 0 pc)
                            else 
                              rounder idxRate)
                      firstRate
                      rrs
      flooredRates = max (fromMaybe 0 floor) <$> (firstRate:restRates) -- `debug` ("reset rates" ++ show (firstRate:restRates))
      cappedRates = min (fromMaybe 1 lifeCap) <$> flooredRates 

runInterestRate2 :: ARM -> (Date,StartRate) -> RateType -> ResetDates -> Ts -> Ts
runInterestRate2 arm (d,sr) floater resetDates rc
  = mkRateTs $ zip (d:resetDates) resultRates -- `debug` ("Result Rate"++show resultRates)
    where 
     resultRates = runInterestRate arm sr floater resetDates rc 

calcIntRate :: Date -> Date -> IRate -> DayCount -> IRate
calcIntRate startDate endDate intRate dayCount =
  let 
    yf = yearCountFraction dayCount startDate endDate
  in 
    intRate * fromRational yf

calcIntRates :: DayCount -> IRate -> [Date] -> [IRate]
calcIntRates dc r ds 
  = [ calcIntRate sd ed r dc |  (sd,ed) <- zip (init ds) (tail ds) ]

calcIntRateCurve :: DayCount -> IRate -> [Date] -> [IRate]
calcIntRateCurve dc r ds 
  = [ calcIntRate sd ed r dc |  (sd,ed) <- zip (init ds) (tail ds) ]

calcInt :: Balance -> Date -> Date -> IRate -> DayCount -> Amount
calcInt bal startDate endDate intRate dayCount =
  let 
    yfactor = yearCountFraction dayCount startDate endDate
  in 
    mulBR bal (yfactor * toRational intRate)

class UseRate x where 
  isAdjustableRate :: x -> Bool
  -- get first index available,if not found return Nothing
  getIndex :: x -> Maybe Index
  getIndexes :: x -> Maybe [Index]
  getResetDates :: x -> Dates
  getSpread :: x -> Maybe Spread


$(deriveJSON defaultOptions ''ARM)
$(deriveJSON defaultOptions ''RateType)
