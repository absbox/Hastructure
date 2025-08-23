module UT.UtilTest(daycountTests1,daycountTests2,daycountTests3,daycountTests4
                  ,tsTest,ts2Test,ts3Test,dateVectorPatternTest,paddingTest,dateSliceTest
                  ,capTest,roundingTest,sliceTest,splitTsTest,tableTest,lastOftest,paySeqTest
                  ,scaleListTest)--,daycountTests3,daycountTests4)
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
import Stmt
import Data.Fixed
import qualified Data.DList as DL
import Data.Ratio ((%))

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
