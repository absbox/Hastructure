module UT.LibTest(curveTests
                 --,queryStmtTests
                 ,datesTests
                 ,prorataTests
                 ,tsOperationTests
                 ,pvTests,seqFunTest,periodCurveTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Types
import Util
import Stmt
import Data.Ratio

curveTests =
    let
     _ts = (FloatCurve [TsPoint (toDate "20210101") 0.01
                         ,TsPoint (toDate "20230101") 0.02])
     _r1 = getValByDate _ts Exc (toDate "20201231")
     _r2 = getValByDate _ts Exc (toDate "20210201")
     _r3 = getValByDate _ts Exc (toDate "20230102")
     _r4 = getValByDate _ts Exc (toDate "20231231")

     _priceTs = (PricingCurve
                  [TsPoint (toDate "20210101") 0.01
                  ,TsPoint (toDate "20210110") 0.02])
    in
  testGroup "Curve Tests"
  [
    testCase "Query interst rate curve by date" $
      assertEqual
        "test 4 dates"
        [_r1,_r2,_r3,_r4] [0, 0.01,0.02,0.02]
    ,testCase "Pricing Curve Test1" $
      assertEqual "left"
        0.01
        (getValByDate _priceTs Exc (toDate "20201231"))
    ,testCase "Pricing Curve Test2" $
      assertEqual "Right"
        0.02
        (getValByDate _priceTs Exc (toDate "20210121"))
    ,testCase "Pricing Curve Test3" $
      assertEqual "Mid"
        (13 % 900)
        (getValByDate _priceTs Exc (toDate "20210105"))
  ]

periodCurveTest = 
  let
    _ts = CurrentVal [PerPoint 0 100, PerPoint 1 200, PerPoint 2 300, PerPoint 4 400]
    _rs1 = getValFromPerCurve _ts Future Inc <$> [0,1,2,3,4,5]
    _rs2 = getValFromPerCurve _ts Future Exc <$> [0,1,2,3,4,5]
    _rs3 = getValFromPerCurve _ts Past Inc <$> [0,1,2,3,4,5]
    _rs4 = getValFromPerCurve _ts Past Exc <$> [0,1,2,3,4,5]
    _ts1 = WithTrailVal [PerPoint 0 100, PerPoint 1 200, PerPoint 2 300]
    _r3 = getValFromPerCurve _ts1 Future Inc 4
    _r4 = getValFromPerCurve _ts1 Future Exc 2
  in
  testGroup "Period Curve Tests"
  [
    testCase "Query period curve by period:Future" $
      assertEqual
        "test 5 period:Future:Inc"
        [Just 100, Just 200, Just 300, Just 400, Just 400, Nothing]
        _rs1 
    ,testCase "Query period curve by period:Future" $
      assertEqual
        "test 5 period:Future:Exc"
        [Just 200, Just 300, Just 400, Just 400, Nothing, Nothing]
        _rs2
    ,testCase "Query period curve by period:Past " $
      assertEqual
        "test 5 period:Past:Inc"
        [Just 100, Just 200, Just 300, Just 300, Just 400, Just 400]
        _rs3
    ,testCase "Query period curve by period:Past " $
      assertEqual
        "test 5 period:Past:Exc"
        [Nothing, Just 100, Just 200, Just 300, Just 300, Just 400]
        _rs4
  ]

--queryStmtTests = testGroup "queryStmtTest"
--  [
--   let
--    stmt1 = Statement [
--             AccTxn (toDate "20200101") 100 (-12) Empty
--             ,AccTxn (toDate "20200101") 100 10 Empty
--             ,AccTxn (toDate "20200101") 100 (-20) Empty]
--    r1 = queryStmtAmt (Just stmt1) Empty
--    r2 = queryStmtAmt (Just stmt1) Empty
--    r3 = queryStmtAmt Nothing Empty
--   in
--    testCase "Query With Plain String" $
--    assertEqual "Simple String Comment"
--             [r1,r2,r3]
--             [12,32,0]
--  ]

datesTests = testGroup "date related "
  [
   let
      d1 = genDates (toDate "20220801") Monthly 1
   in
     testCase "1 Month" $
     assertEqual "1 Month" [toDate "20220901"] d1
   ,
   let
      d2 = genDates (toDate "20220801") Monthly 0
   in
     testCase "zero extra" $
     assertEqual "1 Month" [] d2

  ]

prorataTests = testGroup "prorata Test"
  [
    let 
      bals1 = [100,200,300]
    in 
      testCase "3 bals" $
        assertEqual ""
          [10,20,30]
          (prorataFactors bals1 60)
    ,
    let 
      bals2 = [100,200,0]
    in 
      testCase "2 bals" $
        assertEqual ""
          [20,40,0]
          (prorataFactors bals2 60)
    ,
    let 
      bals3 = [100,100,100]
    in 
      testCase "residual goes to first on tie" $
        assertEqual ""
          [13.34,13.33,13.33]
          (prorataFactors bals3 40)
    ,
    let
      bals4 = replicate 12 0.01
      alloc4 = prorataFactors bals4 0.10
    in
      testCase "many small bals has no negative allocation" $ do
        assertBool "all allocations non-negative" (all (>= 0) alloc4)
        assertEqual "allocations sum to requested amount" 0.10 (sum alloc4)
    ,
    testCase "amt greater than total balance is capped" $
      assertEqual ""
        [100,200]
        (prorataFactors [100,200] 500)
    ,
    testCase "zero total balance gives zeros" $
      assertEqual ""
        [0,0]
        (prorataFactors [0,0] 40)
  ]

tsOperationTests =
  let 
   bcurve = BalanceCurve [TsPoint (toDate "20221101") 100
                         ,TsPoint (toDate "20221201") 50]
  in
   testGroup "operation on ts"
   [
     testCase "split ts by date" $ 
       assertEqual " split in middle "
       (BalanceCurve [TsPoint (toDate "20221101") 100]
       ,BalanceCurve [TsPoint (toDate "20221201") 50]) $
       splitTsByDate bcurve (toDate "20221110")
    ,testCase "split ts by date on left 1" $ 
       assertEqual " split on out of scope"
       (BalanceCurve []
       ,BalanceCurve [TsPoint (toDate "20221101") 100,TsPoint (toDate "20221201") 50]) $
       splitTsByDate bcurve (toDate "20221001")
    ,testCase "split ts by date on right 2" $ 
       assertEqual " split on out of scope"  
       (BalanceCurve [TsPoint (toDate "20221101") 100,TsPoint (toDate "20221201") 50] 
       ,(BalanceCurve [])) $ 
       splitTsByDate bcurve (toDate "20221202")
    ,testCase "split ts by date on left 3" $ 
       assertEqual " split on out of scope"
       (BalanceCurve [TsPoint (toDate "20221101") 100]
       ,BalanceCurve [TsPoint (toDate "20221201") 50]) $
       splitTsByDate bcurve (toDate "20221101")
   ]

pvTests = 
    testGroup "PV test"
    [testCase "PV 6 months" $
        assertEqual "6M"
        1
        1
    ,testCase "PV 1 Y" $
        assertEqual "12M"
        1
        1
    ]

seqFunTest = 
    let 
      a =1 
    in 
    testGroup "seq fun test"
    [
     testCase "clear:even" $
      assertEqual "Good for first"
      [100,20,0]
      (paySeqLiabilitiesAmt 120 [100,20,0])
    ,testCase "shortfall" $
      assertEqual "Good for first" 
      [100,20,0]
      (paySeqLiabilitiesAmt 120 [100,20,10])
    ,testCase "over " $
      assertEqual "Good for first"
      [100,10,0]
      (paySeqLiabilitiesAmt 120 [100,10,0])
    ]
