module UT.AnalyticsTest(walTest,durationTest,fvTest,assetPricingTest,irrTest,survivorTest,hermiteTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import Analytics 
import Assumptions
import Types
import Asset (priceAsset)
import AssetClass.AssetBase
import AssetClass.Loan
import InterestRate

import Data.Ratio

walTest = 
  let 
    _ps = [(50,L.toDate "20230630"),(50,L.toDate "20231231")]
  in 
    testGroup "Calc WAL"
    [ 
      testCase "WAL by Month" $ 
        assertEqual ""
          9.06
          (calcWAL ByMonth 100 (L.toDate "20230101") _ps )
      ,testCase "WAL by Year" $ 
        assertEqual ""
          0.74
          (calcWAL ByYear 100 (L.toDate "20230101") _ps )
  ]

hermiteTest =
  testGroup "Hermite interpolation safety" [
    testCase "single yield point does not crash" $
        assertEqual "one point returns that yield"
            [1.0]
            (interpolateWithHermite (d0, [(d0, 1.0)]) [d0])
    ,testCase "empty yield curve does not crash" $
        assertEqual "no points returns zeros"
            [0.0]
            (interpolateWithHermite (d0, []) [d0])
    ,testCase "short curves work across multiple query dates" $
        assertEqual "singleton is constant and empty is zero"
            ([1.0, 1.0, 1.0], [0.0, 0.0, 0.0])
            ( interpolateWithHermite (d0, [(d0, 1.0)]) queryDates
            , interpolateWithHermite (d0, []) queryDates )
    ,testCase "two yield points still interpolate" $
        assertBool "interior point yields a value between the two yields" $
          case interpolateWithHermite (d0, [(d0, 1.0),(d1, 2.0)]) [L.toDate "20230701"] of
            [v] -> v > 1.0 && v < 2.0
            _ -> False
    ,testCase "multi-point curve reproduces non-terminal node values" $
        let ds = [d0, d1, d2, L.toDate "20260101"]
            ys = [1.0, 2.0, 4.0, 8.0]
            got = interpolateWithHermite (d0, zip ds ys) [d0, d1, d2]
        in assertBool "each non-terminal node returns its input yield" $
             length got == 3 && and (zipWith (\a b -> abs (a - b) < 1e-9) got [1.0, 2.0, 4.0])
    ,testCase "three yield points interpolate between adjacent nodes" $
        assertBool "interior point stays between adjacent yields" $
          case interpolateWithHermite (d0, [(d0, 1.0), (d1, 2.0), (d2, 4.0)]) [L.toDate "20230701"] of
            [v] -> v > 1.0 && v < 2.0
            _ -> False
    ,testCase "pv4 supports a single-point pricing curve" $
        assertEqual "single point behaves like a flat 5% curve"
            (Right (pv2 0.05 d0 d1 100))
            (pv4 (d0, L.mkRateTs [(d0, 0.05)]) [(d1, 100)])
    ,testCase "query exactly at the last node returns its yield (issue #359; was: interpolate throws)" $
        assertEqual "terminal node returns the node yield"
            [4.0]
            (interpolateWithHermite (d0, [(d0, 1.0), (d1, 2.0), (d2, 4.0)]) [d2])
    ,testCase "query beyond the last node holds the last yield (issue #359)" $
        assertEqual "one year past the curve end is flat at the last yield"
            [4.0]
            (interpolateWithHermite (d0, [(d0, 1.0), (d1, 2.0), (d2, 4.0)]) [T.addDays 365 d2])
    ,testCase "pv4 prices a cashflow dated exactly at the last curve node (issue #359)" $
        assertEqual "3-point curve reaches an exact terminal-node cashflow like a flat 5% curve"
            (Right (pv2 0.05 d0 d2 1000))
            (pv4 (d0, L.mkRateTs [(d0, 0.05), (d1, 0.05), (d2, 0.05)]) [(d2, 1000)])
    ,testCase "pv4 prices a cashflow dated beyond the last curve node (issue #359)" $
        assertEqual "cashflow past the curve end discounts off the last yield (flat 5%)"
            (Right (pv2 0.05 d0 (T.addDays 365 d2) 1000))
            (pv4 (d0, L.mkRateTs [(d0, 0.05), (d1, 0.05), (d2, 0.05)]) [(T.addDays 365 d2, 1000)])
  ]
  where
    d0 = L.toDate "20230101"
    d1 = L.toDate "20240101"
    d2 = L.toDate "20250101"
    queryDates = L.toDates ["20230101", "20230701", "20240101"]

durationTest = 
  testGroup "Duration Test" 
  [
    testCase "Duration 1" $ 
      assertEqual "10 Months bullet"
      (273 % 365)
      (calcDuration 
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231001",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  , testCase "Duration 2" $ 
      assertEqual "Multiple cf"
      (252921 % 289445)
      (calcDuration 
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231001",100),(L.toDate "20240101",100)]
        (L.mkRateTs [(L.toDate "20230101",0.01)]))
  , testCase "Duration 3" $
      assertEqual "12 Months bullet"
      (364 % 365)
      (calcDuration
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231231",104)]
        (L.mkRateTs [(L.toDate "20230101",0.05)]))
  , testCase "Convexity 1" $
      assertEqual "10 Months bullet"
      (4068161010949933 % 2251799813685248)
      (calcConvexity
        DC_ACT_365F
        (L.toDate "20230101")
        [(L.toDate "20231231",104)]
        (L.mkRateTs [(L.toDate "20230101",0.05)]))
  ]

fvTest = 
  testGroup "FV Test" [
    testCase "FV2 test" $ 
        assertEqual "1-year"
            108
            (fv2 0.08 (L.toDate "20230101") (L.toDate "20240101") 100) 
    ,testCase "FV2 test" $ 
        assertEqual "0.5-year"
            103.89
            (fv2 0.08 (L.toDate "20230101") (L.toDate "20230701") 100) 
  ]

assetPricingTest = 
  testGroup "Pricing on Asset" [
    testCase "Loan Pricing(Inc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.38 0.76 0.726208 0.0005369 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Inc)
    ,testCase "Loan Pricing(Exc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.17 0.76  0.72633840 0.00052012  0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Exc)
  ]

irrTest = 
  testGroup "Irr Test" [
    testCase "required Amount with 8%" $ 
        assertEqual "12 months"
            (Just 108.0)
            (calcRequiredAmtForIrrAtDate 0.08 (L.toDates ["20230101"])
                                                [-100] 
                                                (L.toDate "20240101"))
    ,testCase "IRR with 8%" $ 
        assertEqual "12 months"
            (Right (360287970912109 % 4503599627370496))
            (calcIRR (L.toDates ["20230101","20240101"]) [-100,108])
    ,testCase "IRR with custom" $ 
        assertEqual "3 months"
            (Right (7681459818792919 % 18014398509481984))
            (calcIRR (L.toDates ["20250101","20250301","20251018"]) [-100,50,70])
  ]
    -- ,testCase "FV2 test" $ 
    --     assertEqual "0.5-year"
    --         103.89
    --         (fv2 0.08 (L.toDate "20230101") (L.toDate "20230701") 100) 
survivorTest = 
  testGroup "Survivor Test" [
    testCase "Survivor 1" $ 
        assertEqual "12 months"
            [0.9]
            (calcSurvivorFactors (L.toDate "20230101") [(L.toDate "20240101")] 0.1)
    ,testCase "Survivor 2" $ 
        assertEqual "3 months"
            [0.9743552534572951,0.9]
            (calcSurvivorFactors (L.toDate "20230101") [(L.toDate "20230401"),(L.toDate "20240101")] 0.1)
  ]