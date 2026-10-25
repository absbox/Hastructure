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
import Data.List (sortBy, uncons)
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
secantSlopes ((t1, y1) : rest@((t2, y2) : _)) =
    (y2 - y1) / (t2 - t1) : secantSlopes rest
secantSlopes _ = []

fritschCarlsonSlopes :: [TimeYield] -> [Double]
fritschCarlsonSlopes [] = []
fritschCarlsonSlopes [_] = [0]
fritschCarlsonSlopes points = adjustSlopes points initialSlopes
  where
    -- Compute initial slopes
    initialSlopes :: [Double]
    initialSlopes = case secantSlopes points of
        [] -> []
        (d0:ds) -> d0 : go d0 ds
      where
        go prev [] = [prev]
        go prev (d:rest) = harmonicMean prev d : go d rest

        harmonicMean d1 d2
          | d1 * d2 > 0 = 2 / (1 / d1 + 1 / d2)
          | otherwise = 0

    -- Adjust slopes for monotonicity.
    -- INVARIANT: `pts` and `slopes` have the same length (`initialSlopes`
    -- produces exactly one slope per point). The final clause is a total
    -- fallback that is unreachable while that invariant holds.
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
        go _ _ acc = reverse acc -- totality fallback for an impossible length mismatch

interpolateWithHermite :: (Date,[(Date, Double)]) -> Dates -> [Double]
interpolateWithHermite (_sd, []) ds = replicate (length ds) 0
interpolateWithHermite (_sd, [(_,y)]) ds = replicate (length ds) y
interpolateWithHermite (sd, ts) ds 
  = let 
      xs = (fromRational . (yearCountFraction DC_ACT_365F sd)) <$> (fst <$> ts)
      ys = snd <$> ts
      yds = fritschCarlsonSlopes (zip xs ys)
      nodes = Nodes.fromList $
                zipWith3 (\x' y' yd' -> (x',(y',yd'))) xs ys yds -- `debug` ("x y yd"++ show xs ++">>"++ show ys ++">>"++show yds)

      lookupFn = Piecewise.interpolateConstantExt Type.hermite1 nodes
      xs' = (fromRational . yearCountFraction DC_ACT_365F sd) <$> ds 
    in 
      lookupFn <$> xs'

-- ^ calculate the Weighted Average Life of cashflow, with unit option to Monthly, Quarterly or Yearly
calcWAL :: TimeHorizion -> Balance -> Date -> [(Balance,Date)] -> Balance 
calcWAL th bal d ps = 
  let 
    interval = case th of
                  ByYear -> 365
                  ByMonth -> 30
                  ByQuarter -> 91
    weightedAmts = [ mulBR futureAmt ((daysBetween d futureDate) % interval)  | (futureAmt,futureDate) <- ps ]
  in 
    sum weightedAmts / bal

-- | Curve types that take part in Hermite rate interpolation (the same set as
-- getTsDates/getTsVals). Everything else keeps the getValByDate lookup.
isHermiteCurve :: Ts -> Bool
isHermiteCurve (IRateCurve _)   = True
isHermiteCurve (RatioCurve _)   = True
isHermiteCurve (FloatCurve _)   = True
isHermiteCurve (PricingCurve _) = True
isHermiteCurve (BalanceCurve _) = True
isHermiteCurve _                = False

-- | Curve tag for pv4's typed error on unsupported pricing-curve types.
tsTypeName :: Ts -> String
tsTypeName (FloatCurve _)          = "FloatCurve"
tsTypeName (BoolCurve _)           = "BoolCurve"
tsTypeName (BalanceCurve _)        = "BalanceCurve"
tsTypeName (LeftBalanceCurve _)    = "LeftBalanceCurve"
tsTypeName (RatioCurve _)          = "RatioCurve"
tsTypeName (ThresholdCurve _)      = "ThresholdCurve"
tsTypeName (IRateCurve _)          = "IRateCurve"
tsTypeName (FactorCurveClosed _ _) = "FactorCurveClosed"
tsTypeName (PricingCurve _)        = "PricingCurve"
tsTypeName (PeriodCurve _)         = "PeriodCurve"
tsTypeName (IntCurve _)            = "IntCurve"

-- | The single discount-rate evaluator shared by pv, pv3, pv3', duration and
-- convexity: Hermite (Fritsch-Carlson) through the curve nodes with constant
-- extension at both borders, exactly like pv4. Flat curves short-circuit to the
-- constant rate so legacy constant-rate arithmetic stays bit-identical.
curveRatesAt :: Date -> Ts -> [Date] -> [Double]
curveRatesAt d curve ds
  | isHermiteCurve curve = rates
  | otherwise            = map (fromRational . getValByDate curve Exc) ds
  where
    rates = case rs of
      []                    -> replicate (length ds) 0
      (r:_) | all (== r) rs -> replicate (length ds) r
            | otherwise     -> interpolateWithHermite (d, zip (getTsDates curve) rs) ds
    rs = map fromRational (getTsVals curve)

-- | Discount one amount at an explicit rate (same arithmetic as pv).
pvWithRate :: Double -> Date -> Date -> Amount -> Amount
pvWithRate discount_rate today d amt
  = realToFrac $ (realToFrac amt) * (1 / factor)
  where
    distance::Double = fromIntegral $ daysBetween today d
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)

calcDuration :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcDuration dc d ps pricingCurve 
  = (foldr (\(r,(_d,_b)) acc ->
                    (*) 
                      (divideBB (pvWithRate r d _d _b) presentValue) 
                      (yearCountFraction dc d _d)
                    + acc)
                    0.0000
                    (zip rates ps))
    where 
      rates = curveRatesAt d pricingCurve (fst <$> ps)
      presentValue = sum [ pvWithRate r d _d _b | (r,(_d,_b)) <- zip rates ps ] 

calcConvexity :: DayCount -> Date -> [(Date,Balance)] -> Ts -> Rate
calcConvexity dc d ps pricingCurve 
  = toRational $
      (*)
        presentValue' $
        (foldr (\(_t,_c,_f) acc ->
                      (_t * (_t + 1) * fromRational _c) / ((1.000 + _f) ** (_t+2)) + acc
                      )
                      0.0000
                      (zip3 ts payments pvFactors))
    where 
      pvFactors::[Double] = curveRatesAt d pricingCurve (fst <$> ps)
      presentValue'::Double = 1 / (fromRational . toRational) (sum [ pvWithRate r d _d _b | (r,(_d,_b)) <- zip pvFactors ps ])
      payments = toRational . snd <$> ps
      ts::[Double] = fromRational <$> yearCountFraction dc d <$> fst <$> ps

-- ^ calculate present value of input amount in future with given a curve and PV date
pv :: Ts -> Date -> Date -> Amount -> Amount
pv pc today d amt = pvWithRate rate today d amt
  where
    rate = case curveRatesAt today pc [d] of
             (r:_) -> r
             []    -> 0

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
      rs = curveRatesAt pricingDate pvCurve ds
      pvs = [ pvWithRate r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs ]
    in 
      sum pvs

pv3' :: Ts -> Date -> [Date] -> [Amount] -> Balance
pv3' pvCurve pricingDate ds vs 
  = let 
      rs = curveRatesAt pricingDate pvCurve ds
      vs' = (fromRational . toRational) <$> vs
      pvs = [ pv2' r pricingDate d amt | (r,d,amt) <- zip3 rs ds vs' ]
    in 
      fromRational . toRational $ foldr (+) 0 pvs

-- ^ using hermite interpolation to get present value
pv4 :: (Date,Ts) -> [(Date,Amount)] -> Either ErrorRep Balance
pv4 _ [] = Left "pv4: No cashflow to get present value"
pv4 (pricingDate, pvCurve) (cf:cfs)
  | not (isHermiteCurve pvCurve) = Left $ "pv4: unsupported pricing curve type: " ++ tsTypeName pvCurve
  | otherwise = case ds' of 
      [] -> Left "pv4: empty pricing curve"
      (d0:_) 
        | d0 < pricingDate -> Left $ "pv4: cashflow date "++ show d0 ++" is before pricing date "++ show pricingDate
        | fst cf < pricingDate -> Left $ "pv4: cashflow date "++ show (fst cf) ++" is before pricing date "++ show pricingDate
        | length ds' > 1 && firstCfDate < d0 -> Left $ "pv4: first cashflow date " ++ show firstCfDate ++ " is before pricing curve start " ++ show d0
        | otherwise 
          -> let 
              ds = fst <$> (cf:cfs)
              rs = let 
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
    firstCfDate = minimum (fst <$> (cf:cfs))



fv2 :: IRate -> Date -> Date -> Amount -> Amount
fv2 discount_rate today futureDay amt 
  = realToFrac $ realToFrac amt * factor 
  where
    factor::Double = (1 + realToFrac discount_rate) ** (distance / 365)
    distance::Double = fromIntegral $ daysBetween today futureDay


calcPvFromIRR :: Double -> [Date] -> [Amount] -> Date -> Double -> Double
calcPvFromIRR irr [] _ d amt = 0
calcPvFromIRR irr ds@(sd:_) vs d amt = 
  let 
    begDate = sd
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
    case ridders def (-100000000000.0,100000000000000) (calcPvFromIRR irr ds vs d) of
      Root finalAmt -> Just (fromRational (toRational finalAmt))
      error -> Nothing -- `debug` ("calcRequiredAmtForIrrAtDate: error"++ show error)

-- ^ calc IRR from a cashflow 
calcIRR :: [Date] -> [Amount] -> Either ErrorRep Rate
calcIRR  _ [] = Left "No cashflow amount"
calcIRR ds vs
  | all (>= 0) vs = Left $ "All cashflow can't be all positive:"++ show vs
  | all (<= 0) vs = return $ -1.0
  | all (== 0) vs = Left "All cashflow can't be all zeros"
  | otherwise = 
    let 
      itertimes = 1000
      def = RiddersParam { riddersMaxIter = itertimes, riddersTol = RelTol 0.000001}
      vs' = fromRational . toRational <$> vs
    in
      case uncons ds of
        Nothing -> Left "calcIRR: empty dates"
        Just (beginDate, _) -> 
          let 
            sumOfPv irr = pv22 irr beginDate ds vs'
          in 
            case ridders def (-1,1000) sumOfPv of
              Root irrRate -> return $ toRational irrRate
              NotBracketed -> Left $ "IRR: not bracketed" ++ show vs' ++ " and dates"++ show ds
              SearchFailed -> Left $ "IRR: search failed:  can't be calculated with input "++ show vs++" and dates"++ show ds
