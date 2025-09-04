{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analytics (calcConvexity,calcDuration,pv,calcWAL,pv2,pv3
      ,fv2,pv21,calcRequiredAmtForIrrAtDate,calcIRR
      ,calcSurvivorFactors,interpolateWithHermite,pv4)

  where 
import Types
import Lib
import Util
import DateUtil
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Ord (comparing)
import Data.List (sortBy)
import GHC.Generics
import Data.Ratio
import Numeric.RootFinding
import qualified Numeric.Interpolation.Piecewise as Piecewise
import qualified Numeric.Interpolation.Type as Type
import qualified Numeric.Interpolation.NodeList as Nodes

import Debug.Trace
debug = flip trace

calcSurvivorFactors :: Date -> [Date] -> Double -> [Double]
calcSurvivorFactors sd ds 0 = replicate (length ds) 1.0 
calcSurvivorFactors sd ds survivalRate = 
  let 
    yearFractions::[Double] = [ realToFrac (daysBetween sd d) / 365.0 | d <- ds ]
    factors = [ (1 - survivalRate) ** x | x <- yearFractions ]
  in 
    factors


-- ^ interpolate the yield curve using Fritsch-Carlson method
type TimeYield = (Double, Double)
type YieldPoint = (Date, Double)

toTimeYield :: Date -> [YieldPoint] -> [TimeYield]
toTimeYield refDate points 
  = let 
        sorted = sortBy (comparing fst) points
    in 
        [ (fromRational (yearCountFraction DC_ACT_365F refDate d), y) | (d, y) <- sorted]

secantSlopes :: [TimeYield] -> [Double]
secantSlopes points = zipWith 
                        (\(t1, y1) (t2, y2) -> (y2 - y1) / (t2 - t1)) 
                        points 
                        (tail points)

-- Compute initial slopes (average of adjacent secant slopes for interior points)
initialSlopes :: [TimeYield] -> [Double]
initialSlopes points@((t0, y0):rest) = 
    let deltas = secantSlopes points
        n = length points
        slopes = [head deltas] ++ -- First slope: use first secant slope
                 [ (d1 + d2) / 2 | (d1, d2) <- zip deltas (tail deltas) ] ++ -- Interior slopes
                 [last deltas] -- Last slope: use last secant slope
    in slopes

fritschCarlsonSlopes :: [TimeYield] -> [Double]
fritschCarlsonSlopes points = adjustSlopes points initialSlopes
  where
    -- Compute initial slopes
    initialSlopes :: [Double]
    initialSlopes = let deltas = secantSlopes points
                        n = length points
                    in [head deltas] ++ -- First slope: use first secant slope
                       [ if d1 * d2 > 0 then 2 / (1 / d1 + 1 / d2) else 0
                         | (d1, d2) <- zip deltas (tail deltas) ] ++ -- Interior slopes: harmonic mean
                       [last deltas] -- Last slope: use last secant slope

    -- Adjust slopes for monotonicity
    adjustSlopes :: [TimeYield] -> [Double] -> [Double]
    adjustSlopes pts slopes = go pts slopes []
      where
        go :: [TimeYield] -> [Double] -> [Double] -> [Double]
        go (_:[]) [m] acc = reverse (m:acc) -- Last point
        go ((t1,y1):p2@((t2,y2):rest)) (m1:m2:ms) acc =
            let delta = (y2 - y1) / (t2 - t1)
                alpha = if delta == 0 then 0 else m1 / delta
                beta = if delta == 0 then 0 else m2 / delta
                r = sqrt (alpha * alpha + beta * beta)
                (m1', m2') = if delta == 0 then (0, 0) -- Flat segment
                             else if r > 3 then (m1 * 3 / r, m2 * 3 / r) -- Scale slopes
                             else (m1, m2)
            in go p2 (m2':ms) (m1':acc)
        go _ _ _ = error "Invalid input"

interpolateWithHermite :: (Date,[(Date, Double)]) -> Dates -> [Double]
interpolateWithHermite (sd, ts) ds 
  = let 
      xs = (fromRational . (yearCountFraction DC_ACT_365F sd)) <$> (fst <$> ts)
      ys = snd <$> ts
      yds = fritschCarlsonSlopes (zip xs ys)
      nodes = Nodes.fromList $
                zipWith3 (\x' y' yd' -> (x',(y',yd'))) xs ys yds -- `debug` ("x y yd"++ show xs ++">>"++ show ys ++">>"++show yds)

      lookupFn = Piecewise.interpolate Type.hermite1 nodes
      xs' = (fromRational . yearCountFraction DC_ACT_365F sd) <$> ds 
    in 
      lookupFn <$> xs'

-- ^ calculate the Weighted Average Life of cashflow, with unit option to Monthly or Yearly
calcWAL :: TimeHorizion -> Balance -> Date -> [(Balance,Date)] -> Balance 
calcWAL th bal d ps = 
  let 
    interval = case th of
                  ByYear -> 365
                  ByMonth -> 30
    weightedAmts = [ mulBR futureAmt ((daysBetween d futureDate) % interval)  | (futureAmt,futureDate) <- ps ]
  in 
    sum weightedAmts / bal

calcDuration :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcDuration dc d ps pricingCurve 
  = (foldr (\(_d,_b) acc ->
                    (*) 
                      (divideBB (pv pricingCurve d _d _b) presentValue) 
                      (yearCountFraction dc d _d)
                    + acc)
                    0.0000
                    ps)
    where 
      presentValue = sum [ pv pricingCurve d _d _b | (_d,_b) <- ps ] 

calcConvexity :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcConvexity dc d ps pricingCurve 
  = toRational $
      (*)
        presentValue' $
        (foldr (\(_t,_c,_f) acc ->
                      (_t * (_t + 1) * fromRational _c) / ((1.000 + _f) ** (_t+2))
                      )
                      0.0000
                      (zip3 ts payments pvFactors)) -- `debug` ("'v"++show presentValue'++"others"++ show (zip3 ts payments pvFactors))
    where 
      pvFactors::[Double] = fromRational <$> getValByDate pricingCurve Inc <$> fst <$> ps
      presentValue'::Double = 1 / (fromRational . toRational) (sum [ pv pricingCurve d _d _b | (_d,_b) <- ps ])
      payments = toRational . snd <$> ps
      ts::[Double] = fromRational <$> yearCountFraction dc d <$> fst <$> ps

-- ^ calculate present value of input amount in future with given a curve and PV date
pv :: Ts -> Date -> Date -> Amount -> Amount
pv pc today d amt = 
  realToFrac $ (realToFrac amt) * (1 / factor) --  `debug` ("DF:"++show factor++" PV AMT"++show amt)
  where
    distance::Double = fromIntegral $ daysBetween today d
    discount_rate = fromRational $ getValByDate pc Exc d -- `debug` ("Get val by ts"++show pc ++">>d"++ show d)
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365) --  `debug` ("discount_rate"++show(discount_rate) ++" dist days=>"++show(distance))

-- ^ calculate present value in the future using constant rate
pv2 :: IRate -> Date -> Date -> Amount -> Amount
pv2 discount_rate today d amt 
  | today == d = amt
  | otherwise 
    = realToFrac $ (realToFrac amt) * (1/denominator)  -- `debug` ("pv: cash"++ show amt++" deno"++ show denominator++">> rate"++show discount_rate)
      where
        denominator::Double = (1 + realToFrac discount_rate) ** (distance / 365)
        distance::Double = fromIntegral $ daysBetween today d -- `debug` ("days betwwen"++ show (daysBetween today d)++">>"++ show d ++ ">>today>>"++ show today)

-- ^ calculate present value to specific date given a series of amount with dates
pv21 :: IRate -> Date -> [Date] -> [Amount] -> Balance
pv21 r d ds vs = sum [ pv2 r d _d amt | (_d,amt) <- zip ds vs ]

-- ^ using double for ridder's method

pv2' :: Double -> Date -> Date -> Double -> Double
pv2' r today d amt 
  | amt == 0 = 0
  | today == d = amt
  | otherwise 
    = amt * (1/denominator)  -- `debug` ("pv: cash"++ show amt++" deno"++ show denominator++">> rate"++show discount_rate)
      where
        denominator::Double = (1 + r) ** (distance / 365)
        distance::Double = fromIntegral $ daysBetween today d -- `debug` ("days betwwen"++ show (daysBetween today d)++">>"++ show d ++ ">>today>>"++ show today)

pv22 :: Double -> Date -> [Date] -> [Double] -> Double
pv22 r d ds vs = sum [ pv2' r d _d amt | (_d,amt) <- zip ds vs ] 

-- ^ calcualte present value given a series of amount with dates
pv3 :: Ts -> Date -> [Date] -> [Amount] -> Balance 
pv3 pvCurve pricingDate ds vs 
  = let 
      rs = fromRational <$> getValByDates pvCurve Inc ds
      pvs = [ pv2 r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs ]
    in 
      sum pvs

pv3' :: Ts -> Date -> [Date] -> [Amount] -> Balance
pv3' pvCurve pricingDate ds vs 
  = let 
      rs = fromRational <$> getValByDates pvCurve Inc ds
      vs' = (fromRational . toRational) <$> vs
      pvs = [ pv2' r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs' ]
    in 
      fromRational . toRational $ foldr (+) 0 pvs

-- ^ using hermite interpolation to get present value
pv4 :: (Date,Ts) -> [(Date,Amount)] -> Either ErrorRep Balance
pv4 _ [] = Left "pv4: No cashflow to get present value"
pv4 (pricingDate, pvCurve) (cf:cfs)
  | null ds' = Left "pv4: empty pricing curve"
  | head ds' < pricingDate = Left $ "pv4: cashflow date "++ show (head ds') ++" is before pricing date "++ show pricingDate
  | fst cf < pricingDate = Left $ "pv4: cashflow date "++ show (fst cf) ++" is before pricing date "++ show pricingDate
  | otherwise 
    = let 
        ds = fst <$> (cf:cfs)
        rs = let 
                ds' = getTsDates pvCurve
                vs'::[Double] = fromRational <$> (getTsVals pvCurve)
              in 
                interpolateWithHermite 
                  (pricingDate, zip ds' vs') 
                  ds
        vs' = (fromRational . toRational . snd) <$> (cf:cfs)
        pvs = [ pv2' r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs' ]
      in 
        return $ fromRational . toRational $ foldr (+) 0 pvs
  where
    ds' = getTsDates pvCurve



fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt 
  = realToFrac $ realToFrac amt * factor 
  where
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today futureDay


calcPvFromIRR :: Double -> [Date] -> [Amount] -> Date -> Double -> Double
calcPvFromIRR irr [] _ d amt = 0
calcPvFromIRR irr ds vs d amt = 
  let 
    begDate = head ds
    vs' = fromRational . toRational <$> vs
    pv = pv22 irr begDate (ds++[d]) (vs'++[amt])
  in 
    (fromRational . toRational) pv

-- ^ calculate IRR of a series of cashflow
calcRequiredAmtForIrrAtDate :: Double -> [Date] -> [Amount] -> Date -> Maybe Amount
calcRequiredAmtForIrrAtDate irr [] _ d = Nothing 
calcRequiredAmtForIrrAtDate irr ds vs d = 
  let 
    itertimes = 500
    def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.00000001}
  in 
    case ridders def (0.0001,100000000000000) (calcPvFromIRR irr ds vs d) of
      Root finalAmt -> Just (fromRational (toRational finalAmt))
      _ -> Nothing

-- ^ calc IRR from a cashflow 
calcIRR :: [Date] -> [Amount] -> Either String Rate
calcIRR  _ [] = Left "No cashflow amount"
calcIRR [] _ = Left "No cashflow date"
calcIRR ds vs
  | all (>= 0) vs = Left $ "All cashflow can't be all positive:"++ show vs
  | all (<= 0) vs = Left $ "All cashflow can't be all negative:"++ show vs
  | all (== 0) vs = Left "All cashflow can't be all zeros"
  | otherwise = 
    let 
      itertimes = 1000
      def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.000001}
      beginDate = head ds
      vs' = fromRational . toRational <$> vs
      sumOfPv irr = pv22 irr beginDate ds vs'
    in 
      case ridders def (-1,1000) sumOfPv of
        Root irrRate -> Right $ toRational irrRate
        NotBracketed -> Left $ "IRR: not bracketed" ++ show vs' ++ " and dates"++ show ds
        SearchFailed -> Left $ "IRR: search failed:  can't be calculated with input "++ show vs++" and dates"++ show ds
