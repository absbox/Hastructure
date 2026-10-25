{-# LANGUAGE OverloadedStrings #-}
module UT.LibTest(curveTests
                 --,queryStmtTests
                 ,datesTests
                 ,prorataTests
                 ,tsOperationTests
                 ,pvTests,seqFunTest,periodCurveTest
                 ,libSafetyTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Data.Aeson as A
import Lib
import Types
import Util
import Stmt
import DateUtil
import Data.Ratio
import Data.List (isInfixOf, sortOn)
import Data.Fixed (Fixed(MkFixed))

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

   ,testCase "Weekly dates" $
     assertEqual "weekly schedule"
       (toDates ["20220808","20220815","20220822"])
       (genDates (toDate "20220801") Weekly 3)
   ,testCase "BiWeekly dates" $
     assertEqual "biweekly schedule"
       (toDates ["20220815","20220829","20220912"])
       (genDates (toDate "20220801") BiWeekly 3)
   ,testCase "Daily dates" $
     assertEqual "daily schedule"
       (toDates ["20220802","20220803","20220804"])
       (genDates (toDate "20220801") Daily 3)
   ,testCase "nextDate weekly is total" $
     assertEqual "next weekly" (toDate "20220808") (nextDate (toDate "20220801") Weekly)
   ,testCase "nextDate biweekly is total" $
     assertEqual "next biweekly" (toDate "20220815") (nextDate (toDate "20220801") BiWeekly)
   ,testCase "afterNPeriod weekly" $
     assertEqual "3 weekly" (toDate "20220822") (afterNPeriod (toDate "20220801") 3 Weekly)
   ,testCase "afterNPeriod biweekly" $
     assertEqual "2 biweekly" (toDate "20220829") (afterNPeriod (toDate "20220801") 2 BiWeekly)
   ,testCase "afterNPeriod daily" $
     assertEqual "5 daily" (toDate "20220806") (afterNPeriod (toDate "20220801") 5 Daily)
   ,testCase "period rate BiWeekly" $
     assertEqual "annual->biweekly" 1 (periodRateFromAnnualRate BiWeekly 26.0715)
   ,testCase "period rate Weekly" $
     assertEqual "annual->weekly" 1 (periodRateFromAnnualRate Weekly 52.143)
  ]

-- | The pre-optimization implementation, kept as an oracle for the IntSet
-- membership change.
legacyProrataFactors :: [Balance] -> Balance -> [Balance]
legacyProrataFactors bals amt
  | totalCents <= 0 = replicate (length bals) 0
  | otherwise = map (toEnum . fromInteger) $ zipWith (+) baseAdds extraCents
  where
    centList = max 0 . toInteger . fromEnum <$> bals
    totalCents = sum centList
    payCents = max 0 $ min totalCents (toInteger $ fromEnum amt)
    baseAdds = [ b * payCents `div` totalCents | b <- centList ]
    residual = payCents - sum baseAdds
    order = sortOn (negate . snd) $ zip [0..] [ b * payCents `mod` totalCents | b <- centList ]
    extraIdx = fst <$> take (fromInteger residual) order
    extraCents = [ if i `elem` extraIdx then 1 else 0 | i <- [0 .. length bals - 1] ]

assertProrataGrid :: String -> [[Balance]] -> [Balance] -> IO ()
assertProrataGrid label balss amts =
  mapM_ (\bs -> mapM_ (\a ->
    assertEqual (label ++ ": " ++ show bs ++ " / " ++ show a)
      (legacyProrataFactors bs a) (prorataFactors bs a)) amts) balss

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
    ,
    testCase "negative balance carries no weight" $
      assertEqual ""
        [50,0,50]
        (prorataFactors [100, -50, 100] 100)
    ,
    testCase "all-negative balances give zeros" $
      assertEqual ""
        [0,0]
        (prorataFactors [-10, -20] 30)
    ,
    let
      curatedBalances :: [[Balance]]
      curatedBalances =
        [ []
        , [0]
        , [1]
        , [0.01]
        , [100,200,300]
        , [100,200,0]
        , [100,-50,100]
        , [-10,-20]
        , [0,0]
        , [1,1,1]
        , [0.01,0.01,0.01]
        , [0.01,0.02,0.03]
        , replicate 12 0.01
        , [100,100,100]
        ]
      curatedAmounts :: [Balance]
      curatedAmounts = [0, 0.01, 0.10, 1, 2, 40, 60, 100, 1000, -1]

      generatedBalances :: [[Balance]]
      generatedBalances =
        [ [ MkFixed (toInteger ((i * 37 + j) `mod` 500)) | i <- [0 .. n - 1] ]
        | (j, n) <- [(0,0), (1,1), (2,3), (3,7), (5,13), (11,31)]
        ]
      generatedAmounts :: [Balance]
      generatedAmounts = [ MkFixed (toInteger a) | a <- [0, 1, 2, 7, 50, 123, 1000] ]
    in
      testGroup "IntSet membership equivalence and edge cases"
      [ testCase "matches the legacy elem oracle on a curated grid" $
          assertProrataGrid "curated" curatedBalances curatedAmounts
      , testCase "matches the legacy elem oracle on a generated grid" $
          assertProrataGrid "generated" generatedBalances generatedAmounts
      , testCase "matches the legacy oracle on a large list" $
          let
            balsL = [ MkFixed (toInteger ((i * 17) `mod` 10000 + 1)) | i <- [0..1999] ] :: [Balance]
            totalL = sum balsL
            amtL = MkFixed (toInteger (fromEnum totalL `div` 3))
            allocL = prorataFactors balsL amtL
          in do
            assertEqual "legacy match" (legacyProrataFactors balsL amtL) allocL
            assertEqual "length" 2000 (length allocL)
            assertEqual "sum preserved" amtL (sum allocL)
            assertBool "non-negative" (all (>= 0) allocL)
      , testCase "empty balances" $
          assertEqual "empty" ([] :: [Balance]) (prorataFactors [] 100)
      , testCase "single balance" $ do
          assertEqual "exact amount" [40] (prorataFactors [100] 40)
          assertEqual "capped amount" [100] (prorataFactors [100] 500)
      , testCase "zero amount gives zeros" $
          assertEqual "zeros" [0,0,0] (prorataFactors [1,2,3] 0)
      , testCase "negative amount gives zeros" $
          assertEqual "zeros" [0,0] (prorataFactors [1,2] (-1))
      , testCase "amount equal to total is fully allocated" $
          assertEqual "all" [100,200] (prorataFactors [100,200] 300)
      , testCase "residual tie goes to first indices" $
          assertEqual "two cents" [0.01,0.01,0.00] (prorataFactors [0.01,0.01,0.01] 0.02)
      , testCase "residual goes to the largest remainder" $
          assertEqual "largest" [0.00,0.00,0.01] (prorataFactors [0.01,0.02,0.03] 0.01)
      ]
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
    ,testCase "getValOnByDate keeps working for a balance curve" $
       assertEqual "value on date" 100 (getValOnByDate bcurve (toDate "20221115"))
    ,testCase "tryGetValOnByDate rejects a non-balance curve" $
        assertBool "error names both curves" $
          case tryGetValOnByDate (IRateCurve [TsPoint (toDate "20221101") 0.05]) (toDate "20221115") of
            Left e -> "BalanceCurve" `isInfixOf` e && "IRateCurve" `isInfixOf` e
            Right _ -> False
    ,testCase "tryGetIntValOnByDate reads an int curve" $
       assertEqual "int value" (Right 7) $
          tryGetIntValOnByDate (IntCurve [TsPoint (toDate "20221101") 7]) (toDate "20221115")
    ,testCase "tryGetIntValOnByDate rejects a balance curve" $
        assertBool "error mentions IntCurve" $
          either (isInfixOf "IntCurve") (const False) (tryGetIntValOnByDate bcurve (toDate "20221115"))
    ,testCase "trySplitTsByDate rejects a non-balance curve" $
        assertBool "error mentions BalanceCurve" $
          either (isInfixOf "BalanceCurve") (const False) (trySplitTsByDate (FloatCurve []) (toDate "20221115"))
    ,testCase "trySumValTs sums a balance curve" $
       assertEqual "sum" (Right 150) (trySumValTs bcurve)
    ,testCase "trySumValTs rejects a non-balance curve" $
        assertBool "error mentions BalanceCurve" $
          either (isInfixOf "BalanceCurve") (const False) (trySumValTs (IRateCurve []))
    ,testCase "subTsBetweenDates with no bounds returns the curve" $
       assertEqual "unchanged" bcurve (subTsBetweenDates bcurve Nothing Nothing)
    ,testCase "subTsBetweenDates filters both bounds" $
       assertEqual "only middle point"
          (BalanceCurve [TsPoint (toDate "20221201") 50])
          (subTsBetweenDates bcurve (Just (toDate "20221110")) (Just (toDate "20221202")))
    ,testCase "trySubTsBetweenDates rejects a non-balance curve" $
        assertBool "error mentions BalanceCurve" $
          either (isInfixOf "BalanceCurve") (const False) (trySubTsBetweenDates (BoolCurve []) Nothing Nothing)
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

libSafetyTests =
  let
    d = toDate "20220101"
    -- one witness per TxnComment constructor, incl. edge shapes
    -- (empty lists, multi-element lists, colon-bearing content, nested comments)
    txnWitnesses =
      [ PayInt [], PayInt ["B1","B2"], PayYield "B1", PayPrin ["B1","B2"]
      , PayGroupPrin ["B1"], PayGroupInt ["B1","B2"]
      , WriteOff "B1" 10.5, FundWith "B1" 20, PayPrinResidual ["B1","B2"]
      , PayFee "F1", SeqPayFee [], SeqPayFee ["F1","F2"], PayFeeYield "F1"
      , Transfer "A1" "A2", TransferBy "A1" "A2" (DueCapAmt 30)
      , BookLedgerBy Credit "L1", BookLedgerBy Debit "L2"
      , PoolInflow Nothing CollectedInterest
      , PoolInflow (Just [PoolName "p1", PoolConsol]) CollectedCash
      , LiquidationProceeds [PoolName "p1", PoolConsol]
      , LiquidationSupport "src", LiquidationDraw, LiquidationRepay "r"
      , LiquidationSupportInt 1.5 2.5, BankInt, SupportDraw, Empty, Tag "a:b,c"
      , UsingDS CurrentBondBalance, SwapAccrue, SwapInSettle "s1", SwapOutSettle "s2"
      , PurchaseAsset "P1" 5, IssuanceProceeds "nb"
      , TxnDirection Credit, TxnDirection Debit
      , TxnComments [BankInt, Transfer "a" "b"] ]
  in
  testGroup "library safety (total head/last)"
  [
    testCase "getValByDate on empty ThresholdCurve returns 0" $
      assertEqual "0" (0::Rational) (getValByDate (ThresholdCurve []) Inc d)
  , testCase "getValByDate on empty PricingCurve returns 0" $
      assertEqual "0" (0::Rational) (getValByDate (PricingCurve []) Inc d)
  , testCase "getDealStatType (Max []) returns RtnBalance" $
      assertEqual "RtnBalance" RtnBalance (getDealStatType (Max []))
  , testCase "getDealStatType (Min []) returns RtnBalance" $
      assertEqual "RtnBalance" RtnBalance (getDealStatType (Min []))
  , testCase "genSerialDatesTill2 with empty generated dates does not crash" $
      assertEqual "sd and ed included for II"
        [toDate "20220101", toDate "20221231"]
        (genSerialDatesTill2 II (toDate "20220101") (SingletonDate (toDate "20500101")) (toDate "20221231"))
  , testCase "parseTxn on invalid string fails instead of crashing" $
      assertBool "should be a parse error" $
        case (A.fromJSON (A.String "garbage") :: A.Result TxnComment) of
          A.Error _ -> True
          A.Success _ -> False
  , testCase "parseTxn on valid tag string still parses" $
      assertBool "should parse Tag" $
        case (A.fromJSON (A.String "<Tag:hello>") :: A.Result TxnComment) of
          A.Success (Tag t) -> t == "hello"
          _ -> False
  , testCase "TxnComment codec roundtrips every constructor" $
      mapM_ (\v -> assertEqual ("roundtrip of " ++ show v) (A.Success v)
                    (A.fromJSON (A.toJSON v) :: A.Result TxnComment))
            txnWitnesses
  , testCase "TxnComment codec rejects legacy and malformed forms cleanly" $
      mapM_ (\s -> assertBool ("should reject " ++ show s)
                    (case (A.fromJSON (A.String s) :: A.Result TxnComment) of
                       A.Error _ -> True
                       A.Success _ -> False))
            [ "<BankInt>"                                -- legacy literal, no colon
            , "<BankInterest:>"                          -- pre-0.52.8 encoder output
            , "<SupportExp:(Int:1.50,Fee:2.50)>"         -- legacy embedded-colon form
            , "<Pool:PoolName \"p1\":CollectedInterest>" -- legacy colon-separated ids/source
            , "<BookLedger:L1>"                          -- bare ledger name (no direction)
            , "<SupportDraw:x>"                          -- payload on a no-payload tag
            , "<Transfer:one>"                           -- wrong arity
            , "<WriteOff:a,zz>"                          -- malformed Balance
            , "<DS:not-a-stat>"                          -- malformed DealStats
            , "<NoSuchTag:x>"                            -- unknown tag
            ]
  ]
