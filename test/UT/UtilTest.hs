{-# LANGUAGE ScopedTypeVariables #-}

module UT.UtilTest(daycountTests1,daycountTests2,daycountTests3,daycountTests4
                  ,tsTest,ts2Test,ts3Test,curveLookupLegacyTest,dateVectorPatternTest,paddingTest,dateSliceTest
                  ,capTest,roundingTest,sliceTest,splitTsTest,tableTest,lastOftest,paySeqTest
                  ,scaleListTest,mapUpdateTest,boundedParMapTest,resolveWorkerCapTest,forceMapValuesTest
                  ,workerNumTest,serverConfigLimitTest,badRequestJSONTest
                  ,moneyMathLegacyTest)--,daycountTests3,daycountTests4)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Cashflow as CF
import qualified Liability as L
import Util
import DateUtil
import Lib
import Types
import Interface
import Stmt
import Data.Fixed
import qualified Data.DList as DL
import Data.Ratio ((%))
import Control.Exception (ArithException(..), ErrorCall(..), evaluate, try)
import Network.HTTP.Types (hContentType)
import Servant.Server (ServerError(..))
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Data.Either
import Data.List (isInfixOf)

import ServerConfig
import Debug.Trace
debug = flip trace


daycountTests1 =  
  let
    d1 = T.fromGregorian 2007 12 28
    d2 = T.fromGregorian 2008 2 28
  in
    testGroup "Day Count Tests 1"
  [
    testCase "Act/Act" $
      assertEqual "should be close to 0.1694288494678 " 
        ((4 % 365) + (58 % 366)) $  
        yearCountFraction DC_ACT_ACT d1 d2
    ,testCase "Act/365F" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_ACT_365F d1 d2
    ,testCase "Act/360" $
      assertEqual "" 
        (62 % 360) $ 
        yearCountFraction DC_ACT_360 d1 d2
    ,testCase "Act/365A" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_ACT_365A d1 d2
    ,testCase "Act/365L" $
      assertEqual "" 
        (62 % 366) $ 
        yearCountFraction DC_ACT_365L d1 d2
    ,testCase "NL/365" $
      assertEqual "" 
        (62 % 365) $ 
        yearCountFraction DC_NL_365 d1 d2
    ,testCase "30 360 ISDA" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_ISDA d1 d2
    ,testCase "30E 360" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30E_360 d1 d2
    ,testCase "30Ep 360" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30Ep_360 d1 d2
    ,testCase "30 360 German" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_German d1 d2
    ,testCase "30 360 US" $
      assertEqual "" 
        (60 % 360) $ 
        yearCountFraction DC_30_360_US d1 d2
  ]

daycountTests2 =  
 let
   d1 = T.fromGregorian 2007 12 28
   d2 = T.fromGregorian 2008 2 29
 in
   testGroup "Day Count Tests 2"
 [
   testCase "Act/Act" $
     assertEqual "" 
       ((4 % 365) + (59 % 366)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (63 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (63 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (63 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (63 % 366) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (62 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (62 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (61 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
 
daycountTests3 =  
 let
   d1 = T.fromGregorian 2007 10 31
   d2 = T.fromGregorian 2008 11 30
 in
   testGroup "Day Count Tests 3"
 [
   testCase "Act/Act" $
     assertEqual "should be close to 0.1694288494678 " 
       ((62 % 365) + (334 % 366)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (396 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (396 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (396 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (396 % 366) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (395 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (390 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]
 
daycountTests4 =  
 let
   d1 = T.fromGregorian 2008 2 1
   d2 = T.fromGregorian 2009 5 31
 in
   testGroup "Day Count Tests 4"
 [
   testCase "Act/Act" $
     assertEqual "" 
       ((335 % 366) + (150 % 365)) $ 
       yearCountFraction DC_ACT_ACT d1 d2
   ,testCase "Act/365F" $
     assertEqual "" 
       (485 % 365) $ 
       yearCountFraction DC_ACT_365F d1 d2
   ,testCase "Act/360" $
     assertEqual "" 
       (485 % 360) $ 
       yearCountFraction DC_ACT_360 d1 d2
   ,testCase "Act/365A" $
     assertEqual "" 
       (485 % 366) $ 
       yearCountFraction DC_ACT_365A d1 d2
   ,testCase "Act/365L" $
     assertEqual "" 
       (485 % 365) $ 
       yearCountFraction DC_ACT_365L d1 d2
   ,testCase "NL/365" $
     assertEqual "" 
       (484 % 365) $ 
       yearCountFraction DC_NL_365 d1 d2
   ,testCase "30 360 ISDA" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30_360_ISDA d1 d2
   ,testCase "30E 360" $
     assertEqual "" 
       (479 % 360) $ 
       yearCountFraction DC_30E_360 d1 d2
   ,testCase "30Ep 360" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30Ep_360 d1 d2
   ,testCase "30 360 German" $
     assertEqual "" 
       (479 % 360) $ 
       yearCountFraction DC_30_360_German d1 d2
   ,testCase "30 360 US" $
     assertEqual "" 
       (480 % 360) $ 
       yearCountFraction DC_30_360_US d1 d2
 ]

tsTest = 
  let
    d1 = T.fromGregorian 2007 12 28
    d2 = T.fromGregorian 2008 2 28
    dpairs = [(toDate "20061201",100)
              ,(toDate "20070201",80)  
              ,(toDate "20080201",60)]
    ed =  (toDate "20090101")
    testTs = mkTs [(toDate "20061201",100)
                  ,(toDate "20070201",80)  
                  ,(toDate "20080201",60)]
    tps = [TsPoint _d _v | (_d,_v) <- dpairs ]

    rateCurve = FloatCurve [TsPoint (toDate "20061201") 0.03
                           ,TsPoint (toDate "20070201") 0.05
                           ,TsPoint (toDate "20080201") 0.07]

    factorCurve = FloatCurve [TsPoint (toDate "20061201") 1.0
                             ,TsPoint (toDate "20070201") 0.8
                             ,TsPoint (toDate "20080201") 0.6]
  in
    testGroup "Test Trigger Factor Curve"
  [
    testCase "" $
      assertEqual "left most" 
        [0,100,80,60] $
        getValByDates testTs Exc [(toDate "20061201")
                                 ,(toDate "20061211")
                                 ,(toDate "20070301")
                                 ,(toDate "20081201")]

    ,testCase "FactorCurveClosed" $                         
      assertEqual "leftNotFound as 1"
        1.0 $
        getValByDate 
          (FactorCurveClosed tps ed)
          Exc
          (toDate "20060601")
    ,testCase "FactorCurveClosed" $                         
      assertEqual "in middle"
        80 $
        getValByDate 
          (FactorCurveClosed tps ed)
          Exc
          (toDate "20070202")          
    ,testCase "FactorCurveClosed" $                         
      assertEqual "right after last dps"
        60 $
        getValByDate 
          (FactorCurveClosed tps ed)
          Exc
          (toDate "20081221") 
    ,testCase "FactorCurveClosed" $                         
      assertEqual "After end date" 
        1.0 $
        getValByDate 
          (FactorCurveClosed tps ed)
          Exc
          (toDate "20090601")
    ,testCase "Multiply Ts" $
      assertEqual " multiplyTs 1 "
        (mkTs [(toDate "20061201", 0.03) ,(toDate "20070201", 0.04),(toDate "20080201", 0.042)]) 
        (multiplyTs Inc rateCurve factorCurve)

  ]

ts2Test = 
    let 
       testThresholdCurve = ThresholdCurve [(TsPoint (toDate "20220101") (1 % 100))
                                           ,(TsPoint (toDate "20220201") (2 % 100))
                                           ,(TsPoint (toDate "20220301") (3 % 100))]
    in 
    testGroup "Test Trigger Threshold Curve"
  [
    testCase "" $
      assertEqual "left most" 
        (1 % 100) $
        getValByDate testThresholdCurve Inc (toDate "20211201")
    ,testCase "" $
      assertEqual "on first-ts" 
        (1 % 100) $
        getValByDate testThresholdCurve Inc (toDate "20220101")
    ,testCase "" $
      assertEqual "after first-ts" 
        (2 % 100) $
        getValByDate testThresholdCurve Inc (toDate "20220110")
    ,testCase "" $
      assertEqual "Right most" 
        (3 % 100) $
        getValByDate testThresholdCurve Inc (toDate "20220310")
  ]

-- | Legacy reference for the plural lookup. The fast path must match this
-- exactly, including for descending/unsorted dates and unsorted curve points.
legacyGetValByDates :: Ts -> CutoffType -> [Date] -> [Rational]
legacyGetValByDates rc ct = map (getValByDate rc ct)

assertCurveLookup :: String -> Ts -> CutoffType -> [[Date]] -> IO ()
assertCurveLookup label curve ct dss =
  mapM_ (\ds -> assertEqual (label ++ " / " ++ show ct ++ " / n=" ++ show (length ds))
                   (legacyGetValByDates curve ct ds)
                   (getValByDates curve ct ds)) dss

curveLookupLegacyTest :: TestTree
curveLookupLegacyTest =
  let
    d0 = toDate "20220101"
    d1 = T.addDays 30 d0
    d2 = T.addDays 60 d0
    d3 = T.addDays 90 d0
    dBefore = T.addDays (-10) d0
    dAfter = T.addDays 200 d0

    ratePts :: [TsPoint Rational]
    ratePts = [ TsPoint d0 (1 % 100)
              , TsPoint d1 (2 % 100)
              , TsPoint d1 (25 % 1000)
              , TsPoint d2 (3 % 100)
              , TsPoint d3 (4 % 100) ]

    iratePts :: [TsPoint IRate]
    iratePts = [ TsPoint d0 0.01
               , TsPoint d1 0.02
               , TsPoint d1 0.025
               , TsPoint d2 0.03
               , TsPoint d3 0.04 ]

    balPts :: [TsPoint Balance]
    balPts = [ TsPoint d0 100
             , TsPoint d1 200
             , TsPoint d2 300
             , TsPoint d3 400 ]

    unsortedRatePts :: [TsPoint Rational]
    unsortedRatePts = [TsPoint d2 (3 % 100), TsPoint d0 (1 % 100), TsPoint d1 (2 % 100)]

    unsortedIratePts :: [TsPoint IRate]
    unsortedIratePts = [TsPoint d2 0.03, TsPoint d0 0.01, TsPoint d1 0.02]

    dateSets :: [[Date]]
    dateSets = [ []
               , [d0]
               , [dBefore]
               , [dAfter]
               , [d0, d1, d2, d3]
               , [d0, d0, d1, d1, d2, d3, d3]
               , [d1]
               , [d1, d2]
               , [d3, d2, d1, d0]
               , [d1, d0, d2, d3]
               , [dBefore, d0, d1, dAfter]
               , [T.addDays (toInteger i) d0 | i <- [0..999]]
               ]

    simpleCurves :: [(String, Ts)]
    simpleCurves =
      [ ("IRateCurve", IRateCurve iratePts)
      , ("FloatCurve", FloatCurve ratePts)
      , ("RatioCurve", RatioCurve ratePts)
      , ("BalanceCurve", BalanceCurve balPts)
      , ("LeftBalanceCurve", LeftBalanceCurve balPts)
      ]

    emptyCurves :: [Ts]
    emptyCurves =
      [ IRateCurve []
      , FloatCurve []
      , RatioCurve []
      , BalanceCurve []
      , LeftBalanceCurve []
      , ThresholdCurve []
      ]
  in
    testGroup "getValByDates legacy compatibility"
    [ testCase "ascending, duplicate, descending and unsorted dates" $
        mapM_ (\(name, c) -> do
          assertCurveLookup name c Inc dateSets
          assertCurveLookup name c Exc dateSets) simpleCurves
    , testCase "unsorted curve points fall back exactly" $
        mapM_ (\(name, c) -> do
          assertCurveLookup name c Inc dateSets
          assertCurveLookup name c Exc dateSets)
          [ ("IRateCurve-unsorted", IRateCurve unsortedIratePts)
          , ("FloatCurve-unsorted", FloatCurve unsortedRatePts)
          ]
    , testCase "FactorCurveClosed matches the legacy lookup" $ do
        assertCurveLookup "factor" (FactorCurveClosed ratePts d2) Exc dateSets
        assertCurveLookup "factor-unsorted" (FactorCurveClosed unsortedRatePts d2) Exc dateSets
    , testCase "ThresholdCurve falls back exactly" $ do
        assertCurveLookup "threshold" (ThresholdCurve ratePts) Inc dateSets
        assertCurveLookup "threshold" (ThresholdCurve ratePts) Exc dateSets
    , testCase "PricingCurve falls back exactly" $ do
        assertCurveLookup "pricing" (PricingCurve ratePts) Inc dateSets
        assertCurveLookup "pricing" (PricingCurve ratePts) Exc dateSets
    , testCase "empty curves" $ do
        mapM_ (\c -> do
          assertCurveLookup "empty" c Inc dateSets
          assertCurveLookup "empty" c Exc dateSets) emptyCurves
        assertCurveLookup "empty-factor" (FactorCurveClosed [] d2) Exc dateSets
        assertCurveLookup "empty-pricing" (PricingCurve []) Inc dateSets
    , testCase "empty dates always return []" $ do
        mapM_ (\(name, c) -> do
          assertEqual (name ++ " Inc") [] (getValByDates c Inc [])
          assertEqual (name ++ " Exc") [] (getValByDates c Exc [])) simpleCurves
        assertEqual "factor" [] (getValByDates (FactorCurveClosed ratePts d2) Exc [])
    ]

-- ^ date pattern test
dateVectorPatternTest = 
  let 
    a = 1
  in 
    testGroup "Test on Date Vector generation"
    [ testCase "" $
        assertEqual "LeapYear&Month End"
          [(toDate "20240229"), (toDate "20240331")]
          (genSerialDates MonthEnd Inc (toDate "20240215") 2)
    , testCase "Week Day Test" $
        assertEqual "week day 2"
          [(toDate "20240514"), (toDate "20240521")]
          (genSerialDates (Weekday 2) Inc (toDate "20240509") 2)
    , testCase "Week Day Test" $
        assertEqual "week day 0"
          [(toDate "20240512"), (toDate "20240519")]
          (genSerialDates (Weekday 0) Inc (toDate "20240509") 2)
    , testCase "" $
        assertEqual "till test"
        [(toDate "20220131"),(toDate "20220228")]
        (genSerialDatesTill (toDate "20220101") MonthEnd (toDate "20220315"))
    , testCase "First Date in Pattern" $
        assertEqual ""
         [(toDate "20220630"),(toDate "20220731"),(toDate "20220831")]
         (genSerialDatesTill2 IE (toDate "20220630") MonthEnd (toDate "20220901"))
    , testCase "Custom Dates" $
        assertEqual "exatct same length"
         [(toDate "20230202"),(toDate "20230909")]
         (genSerialDatesTill2 EE (toDate "20200630") (CustomDate [toDate "20230202", toDate "20230909"] ) (toDate "20230910"))
    , testCase "Custom Dates" $
        assertEqual "Include both ends"
         [(toDate "20200630"), (toDate "20230202"),(toDate "20230909"), (toDate "20230910")]
         (genSerialDatesTill2 II (toDate "20200630") (CustomDate [toDate "20230202", toDate "20230909"] ) (toDate "20230910"))
    , testCase "Mutiple Date Pattern" $
        assertEqual ""
         [(toDate "20230909"), (toDate "20230919")]
         (genSerialDatesTill2 EE 
            (toDate "20200630") 
            (AllDatePattern
              [(CustomDate [toDate "20230909"]),(CustomDate [toDate "20230919"])])
            (toDate "20230930"))
    , testCase "" $
        assertEqual "Generate Dates by N Month"
        (toDates ["20220201","20220401","20220601"])
        (genSerialDatesTill (toDate "20220101") (EveryNMonth (toDate "20220201") 2) (toDate "20220715"))
    , testCase "exclude dates" $
        assertEqual "Exclude Dates "
         [(toDate "20230909")]
         (genSerialDatesTill2 EE (toDate "20200630") 
             (Exclude 
              (CustomDate [toDate "20230202", toDate "20230909"] )
              [(CustomDate [toDate "20230202"] )])
              (toDate "20230910"))
    , testCase "offset by dates" $
        assertEqual "offset by dates"
         [(toDate "20230204"),(toDate "20230911")]
         (genSerialDatesTill2 EE 
           (toDate "20200630") 
           (OffsetBy 
             (CustomDate [toDate "20230202", toDate "20230909"]) 2)
           (toDate "20230910"))
    , testCase "No Inclusive or Exclusive" $
        assertEqual "1"
        (toDates ["20230831","20230930","20231031"])
        (genSerialDatesTill2 NO_IE (toDate "20230810") MonthEnd (toDate "20231031") )
    , testCase "No Inclusive or Exclusive" $
        assertEqual "2"
        (toDates ["20230831","20230930"])
        (genSerialDatesTill2 NO_IE (toDate "20230810") MonthEnd (toDate "20231030") )
    , testCase "No Inclusive or Exclusive" $
        assertEqual "3"
        (toDates ["20230831","20230930","20231031"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") MonthEnd (toDate "20231031") )
    , testCase "No Inclusive or Exclusive" $
        assertEqual "4"
        (toDates ["20230831","20230930"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") MonthEnd (toDate "20231030") )
    , testCase "starts " $
        assertEqual " inc "
        (toDates ["20230930"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") (StartsAt Inc (toDate "20230901") MonthEnd) (toDate "20231030") )
    , testCase "starts " $
        assertEqual " inc "
        (toDates ["20230831", "20230930"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") (StartsAt Inc (toDate "20230831") MonthEnd) (toDate "20231030") )
    , testCase "ends " $
        assertEqual " inc "
        (toDates ["20230831"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") (EndsAt Inc (toDate "20230901") MonthEnd) (toDate "20231030") )
    , testCase "ends " $
        assertEqual " inc "
        (toDates ["20230831"])
        (genSerialDatesTill2 NO_IE (toDate "20230831") (EndsAt Inc (toDate "20230831") MonthEnd) (toDate "20231030") )
    , testCase "ends " $
        assertEqual " exc "
        ([])
        (genSerialDatesTill2 NO_IE (toDate "20230831") (EndsAt Exc (toDate "20230831") MonthEnd) (toDate "20231030") )
    ]                          

paddingTest = 
    let 
      a = [1,2,3]
    in 
      testGroup "padding"
      [ testCase "" $
          assertEqual "padding+"
          [1,2,3,0]
          (paddingDefault 0 a 4)
       ,testCase "" $
          assertEqual "padding-"
          [1,2]
          (paddingDefault 0 a 2)
      ]

dateSliceTest = 
    let 
        ds = [(toDate "20230101"),(toDate "20230201"),(toDate "20230301")]
    in 
        testGroup "slice dates"
        [ testCase "sliceAfterkeepPrevious" $ 
            assertEqual ""
            ds
            (sliceDates (SliceAfterKeepPrevious (toDate "20230115")) ds)
        , testCase "sliceAfterkeepPrevious" $ 
            assertEqual ""
            (tail ds)
            (sliceDates (SliceAfterKeepPrevious (toDate "20230215")) ds)
        ]

capTest = 
    let 
      a = [1,2,3,4]
    in 
      testGroup "Cap Test"
      [ testCase "Cap with 2" $ 
        assertEqual "Cap with 2" 
        [1,2,2,2]
        (capWith 2 a)
      ,testCase "No Cap" $ 
        assertEqual "No Cap" 
        [1,2,3,4]
        (capWith 5 a)
      ]

ts3Test = 
  let 
    ts1 = IRateCurve [TsPoint (toDate "20230301") 0.03,TsPoint (toDate "20230301") 0.05]
  in 
    testGroup "curve update test"
    [ testCase "shift curve" $ 
      assertEqual "Shift RateCurve by 0.01"
      (IRateCurve [TsPoint (toDate "20230301") 0.04,TsPoint (toDate "20230301") 0.06]) $
      (shiftTsByAmt ts1 0.01)
    ]

roundingTest = 
    testGroup "rounding by test"
    [ testCase "Rounding on Rate" $
      assertEqual "Rounding Floor"
      0.02
      (roundingBy (RoundFloor 0.00125) 0.02124)
    , testCase "Rounding on Rate" $
      assertEqual "Rounding Ceiling"
      0.02
      (roundingBy (RoundCeil 0.00125) 0.01876)
    , testCase "Rounding on Rate" $
      assertEqual "Rounding Equal"
      0.02
      (roundingBy (RoundCeil 0.00125) 0.02)
    , testCase "Rounding on Balance" $
      assertEqual "Rounding Ceiling"
      100
      (roundingBy (RoundCeil 5) 96)
    , testCase "Rounding on Balance" $
      assertEqual "Rounding Floor"
      100
      (roundingBy (RoundFloor 5) 104)
    , testCase "Rounding on Balance" $
      assertEqual "Rounding Equal"
      100
      (roundingBy (RoundFloor 5) 100)
    ]

-- | Pre-optimization reference implementations. The optimized helpers must
-- stay bit-for-bit identical to these, including the rounding direction of
-- 'Data.Fixed.fromRational' (floor, including for negatives).
legacyMulBR :: Balance -> Rate -> Balance
legacyMulBR b r = fromRational (toRational b * r)

legacyMulBIR :: Balance -> IRate -> Balance
legacyMulBIR b r = fromRational (toRational b * toRational r)

legacyDivideBI :: Balance -> Int -> Balance
legacyDivideBI b i = fromRational (toRational b / toRational i)

legacyDivideBB :: Balance -> Balance -> Rational
legacyDivideBB b1 b2 = toRational b1 / toRational b2

legacyMulBInt :: Balance -> Int -> Rational
legacyMulBInt b i = toRational b * toRational i

assertMoneyGrid :: (Show a, Show b, Eq c, Show c)
                => String -> (a -> b -> c) -> (a -> b -> c) -> [a] -> [b] -> IO ()
assertMoneyGrid label optimized legacy as bs =
  mapM_ (\a -> mapM_ (\b ->
           assertEqual (label ++ ": (" ++ show a ++ ", " ++ show b ++ ")")
                       (legacy a b) (optimized a b)) bs) as

assertMoneyPairs :: (Show a, Show b, Eq c, Show c)
                 => String -> (a -> b -> c) -> (a -> b -> c) -> [(a,b)] -> IO ()
assertMoneyPairs label optimized legacy pairs =
  mapM_ (\(a,b) ->
    assertEqual (label ++ ": " ++ show a ++ " / " ++ show b)
                (legacy a b) (optimized a b)) pairs

moneyMathLegacyTest :: TestTree
moneyMathLegacyTest =
  let
    balances :: [Balance]
    balances = [0, 0.01, -0.01, 1, -1, 100, -100, 123.45, -123.45
               ,999999999.99, -999999999.99]

    rates :: [Rate]
    rates = [0, 1, -1, 1 % 2, 1 % 3, 2 % 3, 1 % 7
            ,1 % 200, 3 % 200, -1 % 200, -3 % 200
            ,5 % 1000, 15 % 1000, 199 % 200, 17 % 200
            ,1 % 1000000, 123 % 1000, 1000000 % 3
            ,(10 ^ 18 + 1) % 10 ^ 18]

    iRates :: [IRate]
    iRates = [0, 0.000001, -0.000001, 0.085, -0.085
             ,1, 1.5, 12.345678]

    divisors :: [Int]
    divisors = [1, 2, 3, 7, 12, 365, 1000, -1, -3]

    intFactors :: [Int]
    intFactors = [0, 1, -1, 2, 3, 7, 12, 365, -3]

    -- Deterministic sweep over many cents/rate/divisor combinations. This is
    -- deliberately larger than the hand-picked edge cases above.
    sweepBalances :: [Balance]
    sweepBalances = [MkFixed (toInteger i * 37 - 5000) | i <- [0..100]]

    sweepRates :: [Rate]
    sweepRates = [(toInteger i * 13 - 50) % (toInteger i * 7 + 1) | i <- [1..50]]

    sweepIRates :: [IRate]
    sweepIRates = [MkFixed (toInteger i * 7919 - 100000) | i <- [0..50]]

    sweepDivisors :: [Int]
    sweepDivisors = [i | i <- [-50..50], i /= 0]
  in
    testGroup "Money math legacy compatibility"
    [ testGroup "mulBR"
      [ testCase "matches the legacy fromRational oracle on a grid" $
          assertMoneyGrid "mulBR" mulBR legacyMulBR balances rates
      , testCase "matches the legacy oracle over a deterministic sweep" $
          assertMoneyGrid "mulBR-sweep" mulBR legacyMulBR sweepBalances sweepRates
      , testCase "floors half cents for both signs" $ do
          assertEqual "positive half cent" 0.00 (mulBR 1.00 (1 % 200))
          assertEqual "positive 1.5 cents" 0.01 (mulBR 1.00 (3 % 200))
          assertEqual "negative half cent" (-0.01) (mulBR (-1.00) (1 % 200))
          assertEqual "negative 1.5 cents" (-0.02) (mulBR (-1.00) (3 % 200))
          assertEqual "positive 99.5 cents" 0.99 (mulBR 1.00 (199 % 200))
          assertEqual "negative 99.5 cents" (-1.00) (mulBR (-1.00) (199 % 200))
      , testCase "zero, identity and negation" $ do
          assertEqual "zero rate" 0 (mulBR 123.45 0)
          assertEqual "identity rate" 123.45 (mulBR 123.45 1)
          assertEqual "negative identity" (-123.45) (mulBR 123.45 (-1))
      ]
    , testGroup "mulBIR"
      [ testCase "matches the legacy fromRational oracle on a grid" $
          assertMoneyGrid "mulBIR" mulBIR legacyMulBIR balances iRates
      , testCase "matches the legacy oracle over a deterministic sweep" $
          assertMoneyGrid "mulBIR-sweep" mulBIR legacyMulBIR sweepBalances sweepIRates
      , testCase "floors half cents for both signs" $ do
          assertEqual "positive half cent" 0.00 (mulBIR 100.00 0.00005)
          assertEqual "positive 1.5 cents" 0.01 (mulBIR 100.00 0.00015)
          assertEqual "negative half cent" (-0.01) (mulBIR (-100.00) 0.00005)
          assertEqual "negative 1.5 cents" (-0.02) (mulBIR (-100.00) 0.00015)
      , testCase "zero and identity" $ do
          assertEqual "zero rate" 0 (mulBIR 123.45 0)
          assertEqual "identity rate" 123.45 (mulBIR 123.45 1)
      ]
    , testGroup "mulBI"
      [ testCase "is the same operation as mulBIR" $
          assertMoneyGrid "mulBI" mulBI legacyMulBIR balances iRates
      ]
    , testGroup "divideBI"
      [ testCase "matches the legacy fromRational oracle on a grid" $
          assertMoneyGrid "divideBI" divideBI legacyDivideBI balances divisors
      , testCase "matches the legacy oracle over a deterministic sweep" $
          assertMoneyGrid "divideBI-sweep" divideBI legacyDivideBI sweepBalances sweepDivisors
      , testCase "floors toward negative infinity" $ do
          assertEqual "1.00 / 3" 0.33 (divideBI 1.00 3)
          assertEqual "-1.00 / 3" (-0.34) (divideBI (-1.00) 3)
          assertEqual "identity" 123.45 (divideBI 123.45 1)
      , testCase "zero divisor preserves the legacy exception" $ do
          r <- try (evaluate (divideBI 1.00 0)) :: IO (Either ArithException Balance)
          case r of
            Left RatioZeroDenominator -> pure ()
            Left e -> assertFailure ("unexpected exception: " ++ show e)
            Right _ -> assertFailure "expected divideBI by zero to throw"
      ]
    , testGroup "divideBB"
      [ testCase "matches the legacy Rational ratio oracle on a grid" $
          assertMoneyPairs "divideBB" divideBB legacyDivideBB
            [(b1, b2) | b1 <- balances, b2 <- balances, b2 /= 0]
      , testCase "matches the legacy oracle over a deterministic sweep" $
          assertMoneyPairs "divideBB-sweep" divideBB legacyDivideBB
            [(b1, b2) | b1 <- take 31 sweepBalances, b2 <- take 31 sweepBalances, b2 /= 0]
      , testCase "exact ratios" $ do
          assertEqual "1.00 / 0.03" (100 % 3) (divideBB 1.00 0.03)
          assertEqual "-1.00 / 0.03" (-100 % 3) (divideBB (-1.00) 0.03)
          assertEqual "same balance" 1 (divideBB 123.45 123.45)
      , testCase "zero denominator preserves the legacy exception" $ do
          r <- try (evaluate (divideBB 1.00 0.00)) :: IO (Either ArithException Rational)
          case r of
            Left RatioZeroDenominator -> pure ()
            Left e -> assertFailure ("unexpected exception: " ++ show e)
            Right _ -> assertFailure "expected divideBB by zero to throw"
      ]
    , testGroup "splitBal"
      [ testCase "uses the legacy mulBR for both legs" $
          mapM_ (\b -> mapM_ (\r ->
            assertEqual ("splitBal " ++ show b ++ " " ++ show r)
              (legacyMulBR b r, legacyMulBR b (1 - r)) (splitBal r b)) rates) balances
      ]
    , testGroup "mulBInt"
      [ testCase "matches the legacy Rational oracle on a grid" $
          assertMoneyGrid "mulBInt" mulBInt legacyMulBInt balances intFactors
      ]
    ]

sliceTest = 
  testGroup "sliceTest"
  [ testCase "slice 1" $
    assertEqual ""
    [1]
    (slice 0 1 [1,2,3])
  , testCase "slice 2" $
    assertEqual ""
    []
    (slice 0 0 [1,2,3])
  , testCase "slice 2" $
    assertEqual ""
    [1,2,3]
    (slice 0 3 [1,2,3])
  , testCase "slice 3" $
    assertEqual ""
    [1,2,3]
    (slice 0 4 [1,2,3])
  ]

splitTsTest = 
  let 
    cashflow = [CF.CashFlow (toDate "20230901") 10, CF.CashFlow (toDate "20231001") 10,CF.CashFlow (toDate "20231101") 10]
  in 

    testGroup "split times series test"
    [ testCase "split before earliest" $
       assertEqual "" 
       ([],cashflow)
       (splitBy (toDate "20230801") Inc cashflow )
      ,testCase "split after latest" $
       assertEqual "" 
       (cashflow,[])
       (splitBy (toDate "20231201") Inc cashflow )
       ,testCase "split in middle,inclusive" $
       assertEqual "" 
       ([CF.CashFlow (toDate "20230901") 10, CF.CashFlow (toDate "20231001") 10],[CF.CashFlow (toDate "20231101") 10])
       (splitBy (toDate "20231001") Inc cashflow )
       ,testCase "split in middle,exclusive" $
       assertEqual "" 
       ([CF.CashFlow (toDate "20230901") 10],[CF.CashFlow (toDate "20231001") 10,CF.CashFlow (toDate "20231101") 10])
       (splitBy (toDate "20231001") Exc cashflow )
    ]

tableTest = 
  let 
    tbl = ThresholdTable [(5,100),(10,200),(15,300),(20,400)]
  in 
    testGroup "lookup table down"
    [ 
      testCase "down & inclusive" $
      assertEqual ""
      [Nothing,Just 100,Just 100]
      [lookupTable tbl Down (3 >=),lookupTable tbl Down (5 >=),lookupTable tbl Down (12 >=)]
    ,testCase "down & exclusive" $
      assertEqual ""
      [Nothing,Nothing,Just 100]
      [lookupTable tbl Down (3 >),lookupTable tbl Down (5 >),lookupTable tbl Down (6 >)]
    ,testCase "up & inclusive" $
      assertEqual ""
      [Nothing,Just 100,Just 100]
      [lookupTable tbl Up (3 >=),lookupTable tbl Up (5 >=),lookupTable tbl Up (6 >=)]
    ,testCase "up & exclusive" $
      assertEqual ""
      [Just 400, Just 300, Just 200, Nothing]
      [lookupTable tbl Up (20 >=),lookupTable tbl Up (16 >=),lookupTable tbl Up (11 >=),lookupTable tbl Up (3 >=) ]
    ,testCase "table interval" $
      assertEqual " Up first "
      (Just ((5,100),(10,200)))
      (lookupIntervalTable tbl Down (5 >=))
    ,testCase "table interval 1" $
      assertEqual " Up second "
      (Just ((15,300),(20,400)))
      (lookupIntervalTable tbl Down (\x -> x >= 12))
    ,testCase "table interval 2" $
      assertEqual " Up last" 
      Nothing 
      (lookupIntervalTable tbl Down (>= 20))
    ]

lastOftest = 
  let 
    a = [1,2,3,4,5]
    b = [[1],[3],[],[5],[],[]]
  in 
    testGroup "test on last of on list" [
      testCase "on non empty" $
      assertEqual "should be [5]"
      (Just [5])
      (Util.lastOf b (not . null))
    ]


paySeqTest = 
  let 
    d1 = toDate "20200101"
    bnd1 = L.Bond "A" L.Sequential (L.OriginalInfo 100 d1 0.06 Nothing) (L.Fix 0.05 DC_ACT_365F) Nothing 100 0.08 0 0 0 Nothing Nothing Nothing Nothing
    bnd2 = L.Bond "B" L.Sequential (L.OriginalInfo 100 d1 0.06 Nothing) (L.Fix 0.05 DC_ACT_365F) Nothing 100 0.08 0 0 0 Nothing Nothing Nothing Nothing
    writeAmt1 = 100 
  in 
  testGroup "write off on bond" [
    testCase "write off on bond 1" $
    assertEqual "only 1st bond is written off by 70"
    (Right ([bnd1 {L.bndBalance = 30,L.bndStmt = Just (Statement (DL.fromList ([BondTxn d1 30.00 0.00 0.00 0.08 0.00 0.00 0.00 Nothing (WriteOff "A" 70.00)])))}
            , bnd2],0))
    (paySeqM d1 70 L.bndBalance (writeOff d1 DuePrincipal) (Right []) [bnd1,bnd2])
    ,testCase "write off on bond 2" $
    assertEqual "2st bond is written off by 70"
    (Right ([bnd1 {L.bndBalance = 0,L.bndStmt = Just (Statement  (DL.fromList ([BondTxn d1 0.00 0.00 0.00 0.08 0.00 0.00 0.00 Nothing (WriteOff "A" 100.00)])))}
            , bnd2{L.bndBalance = 70,L.bndStmt = Just (Statement (DL.fromList ([BondTxn d1 70.00 0.00 0.00 0.08 0.00 0.00 0.00 Nothing (WriteOff "B" 30.00)])))}
            ],0))
    (paySeqM d1 130 L.bndBalance (writeOff d1 DuePrincipal) (Right []) [bnd1,bnd2])
    ,testCase "write off on all bonds " $
    assertEqual "over write off"
    (Right ([bnd1 {L.bndBalance = 0,L.bndStmt = Just (Statement (DL.fromList ([BondTxn d1 0.00 0.00 0.00 0.08 0.00 0.00 0.00 Nothing (WriteOff "A" 100.00)])))}
            , bnd2{L.bndBalance = 0,L.bndStmt = Just (Statement (DL.fromList ([BondTxn d1 0.00 0.00 0.00 0.08 0.00 0.00 0.00 Nothing (WriteOff "B" 100.00)])))}
            ],30))
    (paySeqM d1 230 L.bndBalance (writeOff d1 DuePrincipal) (Right []) [bnd1,bnd2])
  ]

scaleListTest = 
  let 
    a = 1
  in 
    testGroup "scale list test"
    [ testCase "" $
        assertEqual "scale list"
        [50.0, 37.5, 25.0]
        $ scaleByFstElement 50 [200.0,150.0,100]
      , testCase "" $
        assertEqual "scale list"
        []
        $ scaleByFstElement 50 []
    ]


mapUpdateTest = 
  let 
    m = Map.fromList [(1,100.0),(2,200.0),(3,300.0)]
    fn::(Double->Either String Double) = \x -> Right (x * 0.5)
    fn'::(Double->Either String Double) = \x -> Left "error"
  in 
    testGroup "adjustM"
    [ testCase "adjustM - Test" $
        assertEqual "adjustM - Test - 1" 
        (Right (Map.fromList [(1,100.0),(2,200.0),(3,150.0)]))
        (adjustM fn 3 m)
    , testCase "adjustM - Test" $
        assertEqual "adjustM - Test - 2"
        (Left "error")
        (adjustM fn' 3 m)
    , testCase "adjustM - Test" $
        assertEqual "adjustM - Test - 3"
        (Right (Map.fromList [(1,100.0),(2,100.0),(3,150.0)]))
        (adjustMs fn [2,3] m)
    , testCase "adjustM - Test" $
        assertEqual "adjustM - Test - 4"
        (Left "error")
        (adjustMs fn' [2] m)
    , testCase "adjustM - Test" $
        assertEqual "adjustM - Test - 5"
        (Right m)
        (adjustMs fn [4] m)
    , testCase "mapInMapM - Test" $
        assertEqual "mapInMapM - Test - 1"
        (Right (Map.fromList [(1,100.0),(2,200.0),(3,150.0)]))
        (mapInMapM fn [3] m)
    , testCase "mapInMapM - Test" $
        assertEqual "mapInMapM - Test - 1 - 1"
        (Right (Map.fromList [(1,50.0),(2,100.0),(3,300.0)]))
        (mapInMapM fn [1,2] m)
    , testCase "mapInMapM - Test" $
        assertEqual "mapInMapM - Test - 2"
        True
        (isLeft (mapInMapM fn [4] m))
    , testCase "mapInMapM - Test" $
        assertEqual "mapInMapM - Test - 3"
        True
        (isLeft (mapInMapM fn' [3] m))
    ]

boundedParMapTest = 
  let 
    m = Map.fromList [(1::Int,10::Int),(2,20),(3,30),(4,40),(5,50)]
    f x = x * x
  in 
    testGroup "boundedParMap"
    [ testCase "matches Map.map for a normal bound" $
        assertEqual "same result" (Map.map f m) (boundedParMap 4 f m)
    , testCase "matches Map.map with bound 1 (serial)" $
        assertEqual "same result" (Map.map f m) (boundedParMap 1 f m)
    , testCase "matches Map.map with bound larger than the map" $
        assertEqual "same result" (Map.map f m) (boundedParMap 100 f m)
    , testCase "empty map yields empty map" $
        assertEqual "empty" (Map.empty :: Map.Map Int Int) (boundedParMap 4 f (Map.empty :: Map.Map Int Int))
    , testCase "preserves keys order-independently" $
        assertEqual "keys" (Map.keys m) (Map.keys (boundedParMap 3 f m))
    ]

resolveWorkerCapTest :: TestTree
resolveWorkerCapTest =
  testGroup "resolveWorkerCap"
  [ testCase "missing env falls back to capability count" $
      assertEqual "fallback" 4 (resolveWorkerCap 4 Nothing)
  , testCase "valid override wins (may exceed capability count)" $
      assertEqual "override" 16 (resolveWorkerCap 4 (Just "16"))
  , testCase "override of 1 is the minimum usable bound" $
      assertEqual "one" 1 (resolveWorkerCap 4 (Just "1"))
  , testCase "zero override falls back" $
      assertEqual "zero" 4 (resolveWorkerCap 4 (Just "0"))
  , testCase "negative override falls back" $
      assertEqual "negative" 4 (resolveWorkerCap 4 (Just "-3"))
  , testCase "unparsable override falls back" $
      assertEqual "garbage" 4 (resolveWorkerCap 4 (Just "garbage"))
  ]

forceMapValuesTest :: TestTree
forceMapValuesTest =
  let m = Map.fromList [(1::Int,10::Int),(2,20),(3,30)]
  in
    testGroup "forceMapValues"
    [ testCase "preserves the map" $
        assertEqual "same map" m (forceMapValues m)
    , testCase "empty map stays empty" $
        assertEqual "empty" (Map.empty :: Map.Map Int Int) (forceMapValues Map.empty)
    , testCase "forces every value so exceptions surface eagerly" $ do
        r <- (try (evaluate (forceMapValues (Map.fromList [(1::Int, error "boom" :: Int)])))
              :: IO (Either ErrorCall (Map.Map Int Int)))
        case r of
          Left (ErrorCall msg) -> assertEqual "message" "boom" msg
          Right _ -> assertFailure "expected the forced value to throw"
    ]

workerNumTest :: TestTree
workerNumTest =
  testGroup "resolveWorkerNum"
  [ testCase "config wins over capability count" $
      assertEqual "config" 8 (resolveWorkerNum 4 (Just 8) Nothing)
  , testCase "env wins over config and capability count" $
      assertEqual "env" 16 (resolveWorkerNum 4 (Just 8) (Just "16"))
  , testCase "unparsable env falls back to config" $
      assertEqual "garbage-env" 8 (resolveWorkerNum 4 (Just 8) (Just "garbage"))
  , testCase "negative env falls back to config" $
      assertEqual "neg-env" 8 (resolveWorkerNum 4 (Just 8) (Just "-3"))
  , testCase "invalid config falls back to capability count" $
      assertEqual "bad-config" 4 (resolveWorkerNum 4 (Just 0) Nothing)
  , testCase "nothing set falls back to capability count" $
      assertEqual "caps" 4 (resolveWorkerNum 4 Nothing Nothing)
  ]

serverConfigLimitTest :: TestTree
serverConfigLimitTest =
  testGroup "ServerConfig run limits"
  [ testCase "resolveRunNum defaults when key is missing" $
      assertEqual "default" (Right 1000) (resolveRunNum Nothing)
  , testCase "resolveRunNum accepts a positive value" $
      assertEqual "positive" (Right 1) (resolveRunNum (Just 1))
  , testCase "resolveRunNum rejects zero" $
      assertBool "zero" (isLeft (resolveRunNum (Just 0)))
  , testCase "resolveRunNum rejects a negative value" $
      assertBool "negative" (isLeft (resolveRunNum (Just (-3))))
  , testCase "checkRunNumLimit passes at the limit" $
      assertEqual "at-limit" (Right ()) (checkRunNumLimit "runByCombo" 100 100)
  , testCase "checkRunNumLimit passes with zero runs" $
      assertEqual "zero" (Right ()) (checkRunNumLimit "runByCombo" 100 0)
  , testCase "checkRunNumLimit rejects over the limit with endpoint, key, limit and config suggestion" $
      case checkRunNumLimit "runByCombo" 100 150 of
        Left msg -> do
          assertBool "mentions endpoint and actual" ("runByCombo would run 150" `isInfixOf` msg)
          assertBool "mentions key path" ("exceeding maxRunNum.runByCombo 100" `isInfixOf` msg)
          assertBool "suggests config change" ("raise maxRunNum.runByCombo in config.yml" `isInfixOf` msg)
        Right () -> assertFailure "expected Left"
  , testCase "resolveRunLimits fills defaults for missing keys" $
      assertEqual "defaults" (Right (RunLimits 1000 1000 1000 1000 1000))
                             (resolveRunLimits (RunLimitsRaw Nothing Nothing Nothing Nothing Nothing))
  , testCase "resolveRunLimits keeps explicit values" $
      assertEqual "explicit" (Right (RunLimits 1 2 3 4 5))
                             (resolveRunLimits (RunLimitsRaw (Just 1) (Just 2) (Just 3) (Just 4) (Just 5)))
  , testCase "resolveRunLimits rejects a bad key naming it" $
      case resolveRunLimits (RunLimitsRaw Nothing Nothing Nothing (Just 0) Nothing) of
        Left msg -> assertBool "names the offending key" ("maxRunNum.runDealByRunScenarios" `isInfixOf` msg)
        Right _ -> assertFailure "expected Left"
  ]


-- | #322 review M1: HTTP 400 request rejections must use the standard JSON
-- error envelope, not a plain-text body without a content type.
badRequestJSONTest :: TestTree
badRequestJSONTest =
  let e = badRequestJSON "boom"
  in
    testGroup "badRequestJSON (HTTP 400 envelope)"
    [ testCase "status is 400" $
        assertEqual "400" 400 (errHTTPCode e)
    , testCase "content type is application/json" $
        assertEqual "json" (Just "application/json")
          (fmap BS8.unpack (lookup hContentType (errHeaders e)))
    , testCase "body is the standard Left envelope" $
        assertEqual "envelope" "{\"Left\":\"boom\"}" (BL8.unpack (errBody e))
    ]
