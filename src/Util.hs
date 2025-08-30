{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
    (mulBR,mulBIR,mulBI,mulBInt,mulBInteger,lastN
    ,getValByDate,getValByDates,scaleUpToOne
    ,divideBB,getIntervalFactorsDc
    ,multiplyTs,zipTs,getTsVals,getTsSize,divideBI,mulIR, daysInterval
    ,replace,paddingDefault, capWith, getTsDates
    ,shiftTsByAmt,calcWeightBalanceByDates
    ,maximum',minimum',roundingBy,roundingByM
    ,floorWith,slice,toPeriodRateByInterval, dropLastN, zipBalTs
    ,lastOf,findBox,safeDivide', safeDiv
    ,safeDivide,lstToMapByFn,paySequentially,payProRata,mapWithinMap
    ,payInMap,payInMapM,adjustM,lookupAndApply,lookupAndUpdate,lookupAndApplies
    ,lookupInMap,selectInMap,scaleByFstElement
    ,lookupTuple6 ,lookupTuple7,diffNum,splitBal,lookupM,lookupVs
    ,lookupMM
    -- for debug
    ,debugOnDate,paySeqM,payProM,splitByLengths,showLength
    )
    where
import qualified Data.Time as T
import qualified Data.Map as Map
import Data.List
import Data.Fixed
import Data.Ratio ((%))
import Data.Ix
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Lib
import Types
import DateUtil

import Numeric.Limits (infinity)
import Text.Printf
import Control.Exception

import Data.Time (addDays)
import Debug.Trace
debug = flip trace

mulBR :: Balance -> Rate -> Balance
mulBR b r = fromRational $ toRational b * r 

mulBIR :: Balance -> IRate -> Balance
mulBIR b r = fromRational $ toRational b * toRational r

mulIR :: Int -> Rational -> Rational
mulIR i r = toRational i * r 

splitBal :: Rate -> Balance -> (Balance,Balance)
splitBal r b = (mulBR b r, mulBR b (1-r))

mulIntegerR :: Integer -> Rational -> Rational
mulIntegerR i r = toRational i * r

mulBInt :: Balance -> Int -> Rational 
mulBInt b i = toRational b * toRational i

mulBInteger :: Balance -> Integer -> Rational 
mulBInteger b i = mulBInt b (fromInteger i)

mulBI :: Balance -> IRate -> Amount
mulBI bal r = fromRational  $ toRational bal * toRational r

divideBI :: Balance -> Int -> Balance
divideBI b i = fromRational $ toRational b / toRational i

divideBB :: Balance -> Balance -> Rational
divideBB b1 b2 = toRational b1 / toRational b2

safeDivide :: RealFloat a => a -> a -> a
safeDivide _ 0 = Numeric.Limits.infinity
safeDivide x y = x / y


safeDiv :: Rational -> Rational -> Maybe Rational 
safeDiv _ 0 = Nothing 
safeDiv x y = Just $ x / y

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

tsPointVal :: TsPoint a -> a 
tsPointVal (TsPoint d v) = v

getValByDate :: Ts -> CutoffType -> Date -> Rational

getValByDate (LeftBalanceCurve dps) ct d
  = case find (\(TsPoint _d _) -> (cmpFun ct) _d d) (reverse dps) of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0
    where 
      cmpFun Inc = (<=)
      cmpFun Exc = (<)

getValByDate (BalanceCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0

getValByDate (BalanceCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v
      Nothing -> 0

getValByDate (FloatCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (FloatCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of 
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (IRateCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (IRateCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (RatioCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (RatioCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d >= _d) (reverse dps)  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> 0              -- `debug` ("Getting 0 ")

getValByDate (ThresholdCurve dps) Inc d
  = case find (\(TsPoint _d _) -> d <= _d) dps  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> tsPointVal $ last dps --`debug` ("Not found in gvbd")

getValByDate (ThresholdCurve dps) Exc d
  = case find (\(TsPoint _d _) -> d < _d) dps  of
      Just (TsPoint _d v) -> toRational v  -- `debug` ("Getting rate "++show(_d)++show(v))
      Nothing -> tsPointVal $ last dps --`debug` ("Not found in gvbd")

getValByDate (FactorCurveClosed dps ed) Exc d
  = case find (\(TsPoint _d _) -> d > _d) (reverse dps)  of 
      Just found@(TsPoint _found_d _found_v) -> 
        if d >= ed then 
          1.0
        else 
          _found_v
      Nothing -> 1.0

getValByDate (PricingCurve dps) _ d
  = case (d>=lday,d<=fday) of 
      (True,_) -> tsPointVal $ last dps
      (_,True) -> tsPointVal $ head dps
      _  -> let 
              rindex = fromMaybe 0 $findIndex (\(TsPoint _dl _) -> ( _dl > d )) dps
              rdp@(TsPoint _dr _rv) = dps!!rindex 
              ldp@(TsPoint _dl _lv) = dps!!(pred rindex)
              leftDistance = toRational $ daysBetween _dl d  -- `debug` ("LEFT"++show d)
              distance = toRational $ daysBetween _dl _dr  -- `debug` ("TOTAL Horizion"++show _dl++show _dr)
              vdistance =  _rv - _lv -- ("DIST")
            in 
              toRational $ _lv + (vdistance * leftDistance) / distance 
 --              `debug` ("PricingCurve get Val: D "++ show _lv++">>"++ show vdistance++">>"++ show leftDistance++">>"++ show distance)
    where 
      fday = getDate $ head dps
      lday = getDate $ last dps

getValByDate a b c = error $ "Not match for curve type"++show a++" > "++show b++" > " ++show c


getIndexRateByDates :: RateAssumption  -> [Date] -> [IRate]
getIndexRateByDates (RateCurve idx rc) ds = fromRational <$> getValByDates rc Inc ds
getIndexRateByDates (RateFlat idx r) ds = replicate (length ds) r 

getValByDates :: Ts -> CutoffType -> [Date] -> [Rational]
getValByDates rc ct = map (getValByDate rc ct)

getTsVals :: Ts -> [Rational]
getTsVals (FloatCurve ts) = [ v | (TsPoint d v) <- ts ]
getTsVals (RatioCurve ts) = [ v | (TsPoint d v) <- ts ]
getTsVals (BalanceCurve ts) = [ toRational v | (TsPoint d v) <- ts ]
getTsVals (IRateCurve ts) = [ toRational v | (TsPoint d v) <- ts ]

getTsDates :: Ts -> [Date]
getTsDates (IRateCurve tps) =  map getDate tps
getTsDates (RatioCurve tps) =  map getDate tps
getTsDates (FloatCurve tps) =  map getDate tps
getTsDates (PricingCurve tps) =  map getDate tps
getTsDates (BalanceCurve tps) =  map getDate tps

getTsSize :: Ts -> Int 
getTsSize ts = length (getTsVals ts)

zipTs :: [Date] -> [Rational] -> Ts 
zipTs ds rs = FloatCurve [ TsPoint d r | (d,r) <- zip ds rs ]

zipBalTs :: [Date] -> [Balance] -> Ts
zipBalTs ds rs = BalanceCurve [ TsPoint d r | (d,r) <- zip ds rs ]

-- ^ multiply 1st Ts with values from 2nd Ts
multiplyTs :: CutoffType -> Ts -> Ts -> Ts
multiplyTs ct (FloatCurve ts1) ts2
  = FloatCurve [(TsPoint d (v * (getValByDate ts2 ct d))) | (TsPoint d v) <- ts1 ] 

multiplyTs ct (IRateCurve ts1) ts2
  = IRateCurve [(TsPoint d (v * (fromRational (getValByDate ts2 ct d)))) | (TsPoint d v) <- ts1 ] 

multiplyTs c a b = error  $ "Failed to match : multiplyTs"++ show c ++ show a ++ show b


-- | swap a value in list with index supplied
replace :: [a] -> Int -> a -> [a]
replace xs i e 
  | i > pred (length xs) = error $ "index:"++show i++" is greater than size"++ show (length xs)
  | otherwise = case splitAt i xs of
                   (before, _:after) -> before ++ e: after
                   _ -> xs

-- ^ padding default value to end of list ,make it length with N
paddingDefault :: a -> [a] -> Int -> [a]
paddingDefault x xs s 
  | length xs > s = take s xs
  | otherwise = xs ++ replicate (s - length xs) x

capWith :: Ord a => a -> [a] -> [a]
capWith cap xs = [ min cap x | x <- xs ]

floorWith :: Ord a => a -> [a] -> [a]
floorWith floor xs = [ max x floor | x <- xs]

diffNum :: Num a => [a] -> [a]
diffNum xs = zipWith (-) (init xs) (tail xs)

scaleByFstElement :: forall a. Fractional a => a -> [a] -> [a]
scaleByFstElement x [] = []
scaleByFstElement y (b:xs) = 
  let 
    s = y/b 
  in 
    y:[ x * s | x <- xs ]


debugLine :: Show a => [a] -> String 
debugLine xs = ""

lastOf:: [a] -> (a->Bool) -> Maybe a
lastOf [] fn = Nothing
lastOf xs fn = 
  let 
    l = last xs
  in 
    if fn l then 
      Just l 
    else
      lastOf (init xs) fn

shiftTsByAmt :: Ts -> Rational -> Ts 
shiftTsByAmt (IRateCurve tps) delta 
  = IRateCurve $ [ TsPoint d (fromRational delta+v) | TsPoint d v <- tps ]

shiftTsByAmt _ts delta = _ts

assert1 :: Bool -> a -> String -> a
assert1 False x msg = error msg
assert1 _     x _ = x


-- ^ get a weighted average balance on year basis with a dayCount required
calcWeightBalanceByDates :: DayCount -> [Balance] -> [Date] -> Balance 
calcWeightBalanceByDates dc bals ds 
  = assert1
      (succ bs_length == ds_length) 
      (sum $ zipWith mulBR bals weights)
      "calcWeightBalanceByDates: bs and ds should be same length"
      where 
        bs_length = length bals 
        ds_length = length ds
        weights = getIntervalFactorsDc dc ds

testSumToOne :: [Rate] -> Bool
testSumToOne rs = sum rs == 1.0

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y ->if x >= y then y else x)

roundingBy :: (Num a,Fractional a, RealFrac a) => RoundingBy a -> a -> a
roundingBy (RoundFloor x) n = x * fromIntegral (floor (n/x) :: Integer)
roundingBy (RoundCeil x) n = x * fromIntegral (ceiling (n/x) :: Integer)

roundingByM :: (Fractional a,RealFrac a) => Maybe (RoundingBy a) -> a -> a 
roundingByM Nothing x = x 
roundingByM (Just rb) x = roundingBy rb x

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from ) (drop from xs)

dropLastN :: Int -> [a] -> [a]
dropLastN n xs = slice 0 (length xs - n) xs

-- ^ convert annual rate (in 365 days) to period rate by interval days
toPeriodRateByInterval :: Rate -> Int -> Rate
toPeriodRateByInterval annualRate days
  = toRational $ 1 - fromRational (1-annualRate) ** (fromIntegral days / 365) -- `debug` ("days>>"++show days++"DIV"++ show ((fromIntegral days) / 365))

scaleUpToOne :: [Rational] -> [Rational]
scaleUpToOne rs =
  let 
    s = 1 / sum rs
  in 
    (s *) <$> rs 


findBox :: (Ord a,Num a) => (CutoffType,CutoffType) -> a -> [(a,a)] -> Maybe (a,a)
findBox _ x [] = Nothing
findBox (Inc,Inc) x ((l,h):xs) 
  | x >= l && x <= h = Just (l,h)
  | otherwise = findBox (Inc,Inc) x xs

findBox (Exc,Inc) x ((l,h):xs) 
  | x > l && x <= h = Just (l,h)
  | otherwise = findBox (Exc,Inc) x xs

findBox (Inc,Exc) x ((l,h):xs) 
  | x >= l && x < h = Just (l,h)
  | otherwise = findBox (Inc,Exc) x xs

findBox (Exc,Exc) x ((l,h):xs) 
  | x >= l && x < h = Just (l,h)
  | otherwise = findBox (Exc,Exc) x xs


safeDivide' :: (Eq a, Fractional a, Real a, Show a, Show b) => b -> a -> a -> Either ErrorRep Rational
safeDivide' m x 0 = Left $ "Division by zero with: "++ show m 
safeDivide' m x y = Right $ toRational x / toRational y


lstToMapByFn :: (a -> String) -> [a] -> M.Map String a 
lstToMapByFn fn lst =
  let 
    ks = fn <$> lst 
  in 
    M.fromList $ zip ks lst

paySeqM :: Date -> Amount -> (a->Balance) -> (Amount->a->Either ErrorRep a) -> Either ErrorRep [a] -> [a] -> Either ErrorRep ([a],Amount)
paySeqM d amt getDueAmt payFn paidList []
  = do 
      pList <- paidList 
      return (reverse pList, amt)
paySeqM d 0 getDueAmt payFn paidList tobePaidList
  = do 
      pList <- paidList 
      return (reverse pList++tobePaidList, 0)
paySeqM d amt getDueAmt payFn paidList (l:tobePaidList)
  = do 
      let dueAmt = getDueAmt l
      let actualPaidOut = min amt dueAmt 
      let remainAmt = amt - actualPaidOut
      paidL <- payFn actualPaidOut l
      paidList_ <- paidList
      paySeqM d remainAmt getDueAmt payFn (Right $ paidL:paidList_) tobePaidList

payProM :: Date -> Amount -> (a->Balance) -> (Amount -> a -> Either ErrorRep a) -> [a] -> Either ErrorRep ([a],Amount)
payProM d amt getDueAmt payFn tobePaidList
  = let
      dueAmts = getDueAmt <$> tobePaidList
      totalDueAmt = sum dueAmts
      actualPaidOut = min amt totalDueAmt
      remainAmt = amt - actualPaidOut
      allocAmt = prorataFactors dueAmts actualPaidOut
    in
      do 
        paidList <- sequenceA [ payFn amt l | (amt,l) <- zip allocAmt tobePaidList ]
        return (paidList, remainAmt)



paySequentially :: Date -> Amount -> (a->Balance) -> (Amount->a->a) -> [a] -> [a] -> ([a],Amount)
paySequentially d amt getDueAmt payFn paidList []
  = (reverse paidList, amt)
paySequentially d 0 getDueAmt payFn paidList tobePaidList
  = (reverse paidList++tobePaidList, 0)
paySequentially d amt getDueAmt payFn paidList (l:tobePaidList)
  = let 
      dueAmt = getDueAmt l
      actualPaidOut = min amt dueAmt 
      remainAmt = amt - actualPaidOut
      paidL = payFn actualPaidOut l
    in 
      paySequentially d remainAmt getDueAmt payFn (paidL:paidList) tobePaidList

payProRata :: Date -> Amount -> (a->Balance) -> (Amount->a->a) -> [a] -> ([a],Amount)
payProRata d amt getDueAmt payFn tobePaidList
  = let 
      dueAmts = getDueAmt <$> tobePaidList
      totalDueAmt = sum  dueAmts
      actualPaidOut = min amt totalDueAmt
      remainAmt = amt - actualPaidOut

      allocAmt = prorataFactors dueAmts actualPaidOut

      paidList = [ payFn amt l | (amt,l) <- zip allocAmt tobePaidList ]
    in 
      (paidList, remainAmt)

payInMap :: Date -> Balance -> (a->Balance) -> (Balance->a->a)-> [String] 
          -> HowToPay -> Map.Map String a -> Map.Map String a
payInMap d amt getDueFn payFn objNames how inputMap 
  = let 
      objsToPay = (inputMap Map.!) <$> objNames  
      dueAmts = getDueFn <$> objsToPay
      totalDueAmt = sum dueAmts
      actualPaidOut = min totalDueAmt amt
      allocatedPayAmt = case how of 
                          ByProRata -> prorataFactors dueAmts actualPaidOut
                          BySequential -> paySeqLiabilitiesAmt amt dueAmts
      paidObjs = [ payFn amt l | (amt,l) <- zip allocatedPayAmt objsToPay ]
    in 
      (Map.fromList $ zip objNames paidObjs) <> inputMap

payInMapM :: Date -> Balance -> (a->Balance) -> (Balance->a->Either ErrorRep a)-> [String] 
          -> HowToPay -> Map.Map String a -> Either ErrorRep (Map.Map String a)
payInMapM d amt getDueFn payFn objNames how inputMap
  = let 
      objsToPay = (inputMap Map.!) <$> objNames  
      dueAmts = getDueFn <$> objsToPay
      totalDueAmt = sum dueAmts
      actualPaidOut = min totalDueAmt amt
      allocatedPayAmt = case how of 
                          ByProRata -> prorataFactors dueAmts actualPaidOut
                          BySequential -> paySeqLiabilitiesAmt amt dueAmts
    in 
      do 
        paidObjs <- sequenceA [ payFn amt l | (amt,l) <- zip allocatedPayAmt objsToPay ]
        return $ (Map.fromList $ zip objNames paidObjs) <> inputMap


mapWithinMap :: Ord k => (a -> a) -> [k] -> Map.Map k a -> Map.Map k a  
mapWithinMap fn ks m = foldr (Map.adjust fn) m ks

adjustM :: (Ord k, Applicative m) => (a -> m a) -> k -> Map.Map k a -> m (Map.Map k a)
adjustM f = Map.alterF (traverse f)

lookupM :: (Show k,Ord k) => k -> Map.Map k a -> Either ErrorRep a
lookupM key m =
  case Map.lookup key m of
    Nothing -> Left $ "Key not found: " ++ show key ++ " in map with keys: " ++ show (Map.keys m)
    Just a  -> return a

lookupMM :: (Show k,Ord k) => k -> Maybe (Map.Map k a) -> Either ErrorRep a
lookupMM k Nothing = Left $ "Empty map to look up for key: "++ show k
lookupMM key (Just m) = lookupM key m



lookupVs :: (Show k, Ord k) => [k] -> Map.Map k a -> Either ErrorRep [a]
lookupVs keys m = 
  let 
    missingKeys = filter (not . (`Map.member` m)) keys
  in 
    if null missingKeys then 
      return $ map (m Map.!) keys
    else 
      Left $ "Missing keys: " ++ show missingKeys ++ " in map with keys: " ++ show (Map.keys m)


-- ^ lookup and apply a function to a single value in a map ,return a value
lookupAndApply :: Ord k => (a -> b) -> String -> k -> Map.Map k a -> Either ErrorRep b
lookupAndApply f errMsg key m =
  case Map.lookup key m of
    Nothing -> Left errMsg
    Just a  -> return $ f a

-- ^ lookup and apply a function to values in a map ,return a list
lookupAndApplies :: Ord k => (a -> b) -> String -> [k] -> Map.Map k a -> Either ErrorRep [b]
lookupAndApplies f errMsg keys m 
  = sequenceA $ (\x -> lookupAndApply f errMsg x m) <$> keys

lookupAndUpdate :: (Show k, Ord k) => (a -> a) -> String -> [k] -> Map.Map k a -> Either ErrorRep (Map.Map k a) 
lookupAndUpdate f errMsg keys m 
  | S.isSubsetOf inputKs mapKs = return $ mapWithinMap f keys m
  | otherwise = Left $ errMsg++":Missing keys, valid range "++ show mapKs ++ "But got:" ++ show inputKs
  where 
      inputKs = S.fromList keys
      mapKs = Map.keysSet m

lookupInMap :: (Show k, Ord k) => String -> [k] -> Map.Map k a -> Either String (Map.Map k a)
lookupInMap = lookupAndUpdate id  

selectInMap :: (Show k, Ord k) => String -> [k] -> Map.Map k a -> Either String (Map.Map k a)
selectInMap errMsg keys m 
  | S.isSubsetOf inputKs mapKs = Right $ Map.filterWithKey (\k _ -> S.member k inputKs) m
  | otherwise = Left $ errMsg++":Missing keys, valid range "++ show mapKs ++ "But got:" ++ show inputKs
  where 
      inputKs = S.fromList keys
      mapKs = Map.keysSet m

lookupTuple6 :: (Show k,Ord k) => (k, k, k, k, k, k) -> Map.Map k v -> Either ErrorRep (v, v, v, v, v, v)
lookupTuple6 (k1, k2, k3, k4, k5, k6) m 
  | any (\k -> Map.notMember k m) [k1, k2, k3, k4, k5, k6] = Left $ "lookupTuple6: Empty key in tuple"++ show (k1, k2, k3, k4, k5, k6) ++ "but map has keys: " ++ (show (Map.keys m))
  | otherwise = return (m Map.! k1, m Map.! k2, m Map.! k3, m Map.! k4, m Map.! k5, m Map.! k6)

lookupTuple7 :: (Ord k, Show k) => (k, k, k, k, k, k, k) -> Map.Map k v -> Either ErrorRep (v, v, v, v, v, v, v)
lookupTuple7 (k1, k2, k3, k4, k5, k6, k7) m
  | any (\k -> Map.notMember k m) [k1, k2, k3, k4, k5, k6, k7] 
    = Left $ "lookupTuple7: Empty key in tuple"++ show (k1, k2, k3, k4, k5, k6, k7) ++ "but map has keys: " ++ (show (Map.keys m))
  | otherwise = return (m Map.! k1, m Map.! k2, m Map.! k3, m Map.! k4, m Map.! k5, m Map.! k6, m Map.! k7)
  


splitByLengths :: Num a => [a] -> [Int] -> [[a]]
splitByLengths xs ns = go xs ns
  where
    go _ [] = []
    go [] _ = []
    go xs (n:ns) = take n xs : go (drop n xs) ns

----- DEBUG/PRINT
debugOnDate :: Date -> Date -> Date -> String
debugOnDate d1 d2 d 
  | (d <= d2) && (d >= d1)  = "Date:"++show d
  | otherwise = ""

showLength :: Show a => [a] -> String
showLength xs = "Length:" ++ (show . length) xs
