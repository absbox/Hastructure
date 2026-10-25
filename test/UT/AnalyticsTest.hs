module UT.AnalyticsTest(walTest,durationTest,fvTest,assetPricingTest,irrTest,survivorTest,hermiteTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Data.Either (isLeft)
import qualified Lib as L
import Analytics 
import Assumptions
import Types
import Asset (priceAsset)
import AssetClass.AssetBase
import AssetClass.Loan
import InterestRate
import DateUtil (yearCountFraction)
import Util (getValByDate, getTsDates, getTsVals, divideBB)
import Data.Fixed (Micro)

import Data.Ratio

-- Independent oracle helpers for the curve-consistency tests. They evaluate
-- the curve with the public Hermite interpolant and re-derive PV/duration/
-- convexity from the formulas, without calling the functions under test.
oracleRates ts d dds =
  interpolateWithHermite (d, zip (getTsDates ts) (map (fromRational . toRational) (getTsVals ts))) dds

oraclePvAt :: Double -> T.Day -> T.Day -> Amount -> Amount
oraclePvAt r d dd amt = realToFrac $ (fromRational (toRational amt) :: Double) * (1 / ((1 + r) ** t))
  where
    t = fromIntegral (T.diffDays dd d) / 365 :: Double

oracleDurRates :: [Double] -> T.Day -> [(T.Day, Amount)] -> Double
oracleDurRates zs d ps =
  let pvs = zipWith (\z (dd,amt) -> oraclePvAt z d dd amt) zs ps
      total = sum pvs
  in fromRational (sum [ divideBB pvi total * yearCountFraction DC_ACT_365F d dd | (pvi,(dd,_)) <- zip pvs ps ])

oracleConvRates :: [Double] -> T.Day -> [(T.Day, Amount)] -> Double
oracleConvRates zs d ps =
  let pvs = zipWith (\z (dd,amt) -> oraclePvAt z d dd amt) zs ps
      total = sum pvs
      pvD = 1 / (fromRational (toRational total) :: Double)
      tms = [ fromRational (yearCountFraction DC_ACT_365F d dd) | (dd,_) <- ps ]
      pays = [ toRational a | (_,a) <- ps ]
  in pvD * sum [ t*(t+1)*fromRational c / ((1+f)**(t+2)) | (t,c,f) <- zip3 tms pays zs ]

oracleDur ts d ps = oracleDurRates (oracleRates ts d (map fst ps)) d ps
oracleConv ts d ps = oracleConvRates (oracleRates ts d (map fst ps)) d ps

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

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
      ,testCase "WAL by Quarter" $ 
        assertEqual ""
          2.98
          (calcWAL ByQuarter 100 (L.toDate "20230101") _ps )
      ,testCase "WAL by Quarter uses 91-day quarters" $ 
        assertEqual "a single cashflow 91 days out has WAL 1.0"
          1.00
          (calcWAL ByQuarter 100 (L.toDate "20230101") [(100,T.addDays 91 (L.toDate "20230101"))] )
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
    ,testCase "pv4 rejects an empty cashflow list" $
        assertEqual "no cashflows returns Left"
            (Left "pv4: No cashflow to get present value")
            (pv4 (d0, L.mkRateTs [(d0, 0.05)]) [])
    ,testCase "pv4 rejects an empty pricing curve" $
        assertBool "empty curve returns Left instead of throwing"
            (isLeft (pv4 (d0, L.mkRateTs []) [(d1, 100)]))
    ,testCase "pv4 rejects a curve starting before the pricing date" $
        assertEqual "Left names the first curve date"
            (Left "pv4: cashflow date 2023-01-01 is before pricing date 2023-06-01")
            (pv4 (L.toDate "20230601", L.mkRateTs [(d0, 0.05)]) [(d1, 100)])
    ,testCase "pv4 rejects a cashflow before the pricing date" $
        assertEqual "Left names the cashflow date"
            (Left "pv4: cashflow date 2023-01-01 is before pricing date 2023-06-01")
            (pv4 (L.toDate "20230601", L.mkRateTs [(L.toDate "20230601", 0.05)]) [(d0, 100)])
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
    ,testCase "pv4 accepts a curve that starts at the first cashflow (issue #359 follow-up #2)" $
        assertEqual "coverage from the first cashflow is enough; the node rate is exact"
            (Right (pv2 0.05 (L.toDate "20230601") d1 1000))
            (pv4 (L.toDate "20230601", L.mkRateTs [(d1, 0.05), (d2, 0.06)]) [(d1, 1000)])
    ,testCase "pv4 rejects a cashflow before the pricing-curve start (issue #359 follow-up #2)" $
        assertEqual "Left names the first cashflow and the curve start"
            (Left "pv4: first cashflow date 2023-06-01 is before pricing curve start 2024-01-01")
            (pv4 (d0, L.mkRateTs [(d1, 0.05), (d2, 0.06)]) [(L.toDate "20230601", 1000)])
    ,testCase "pv4 keeps the single-point flat-rate curve for a pre-node cashflow (issue #359 follow-up #2)" $
        assertEqual "a one-point curve is a flat rate everywhere"
            (Right (pv2 0.05 d0 (L.toDate "20230601") 1000))
            (pv4 (d0, L.mkRateTs [(d1, 0.05)]) [(L.toDate "20230601", 1000)])
    ,testCase "pv uses the same curve as pv4 (single-cashflow parity)" $
        let cases = [ (L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05)], L.toDate "20240601")
                    , (L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05)], L.toDate "20250101")
                    , (L.mkRateTs [(d0,0.05),(d2,0.05)], L.toDate "20250601")
                    , (L.mkRateTs [(d0,0.05)], L.toDate "20230701") ]
        in mapM_ (\(c,dd) -> assertEqual ("pv vs pv4 at " ++ show dd)
                    (pv4 (d0, c) [(dd, 1000)]) (Right (pv c d0 dd 1000))) cases
    ,testCase "pv3 equals the sum of pv over its cashflows" $
        let c = L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05),(T.addDays 365 d2,0.045)]
            ds = [d1,d2,T.addDays 365 d2]
            amts = [50,50,1050]
        in assertEqual "pv3 = sum of pv"
             (sum [pv c d0 dd amt | (dd,amt) <- zip ds amts]) (pv3 c d0 ds amts)
    ,testCase "pv matches an independent Hermite oracle" $
        let c = L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05),(T.addDays 365 d2,0.045)]
            dd = L.toDate "20240601"
        in case oracleRates c d0 [dd] of
             [z] -> assertEqual "pv uses the Hermite rate" (oraclePvAt z d0 dd 1000) (pv c d0 dd 1000)
             _   -> assertFailure "oracle rate lookup"
    ,testCase "pv on a single-point curve before the node uses the node rate" $
        let c  = L.mkRateTs [(d1,0.05)]
            dd = L.toDate "20230601"
        in assertEqual "single-point curve extends left" (oraclePvAt 0.05 d0 dd 1000) (pv c d0 dd 1000)
    ,testCase "flat curve pv matches the constant-rate pv2" $
        let c  = L.mkRateTs [(d0,0.05),(d2,0.05)]
            dd = L.toDate "20240601"
        in assertEqual "flat curve = constant rate" (pv2 0.05 d0 dd 1000) (pv c d0 dd 1000)
    ,testCase "pv4 returns Left for unsupported curve types" $
        do assertEqual "ThresholdCurve"
             (Left "pv4: unsupported pricing curve type: ThresholdCurve")
             (pv4 (d0, ThresholdCurve [TsPoint d1 0.02, TsPoint d2 0.05]) [(d1, 1000)])
           assertEqual "FactorCurveClosed"
             (Left "pv4: unsupported pricing curve type: FactorCurveClosed")
             (pv4 (d0, FactorCurveClosed [TsPoint d1 0.9, TsPoint d2 0.8] d2) [(d1, 1000)])
    ,testCase "pv4 accepts every supported Hermite curve type" $
        let expR = Right (pv2 0.05 d0 d1 1000)
            cases = [ ("IRateCurve", L.mkRateTs [(d0,0.05),(d2,0.05)])
                    , ("RatioCurve", RatioCurve [TsPoint d0 0.05, TsPoint d2 0.05])
                    , ("FloatCurve", FloatCurve [TsPoint d0 0.05, TsPoint d2 0.05])
                    , ("BalanceCurve", BalanceCurve [TsPoint d0 0.05, TsPoint d2 0.05])
                    , ("PricingCurve", PricingCurve [TsPoint d0 0.05, TsPoint d2 0.05]) ]
        in mapM_ (\(n,c) -> assertEqual ("supported " ++ n) expR (pv4 (d0,c) [(d1,1000)])) cases
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
  , testCase "multi-point curve metrics follow the Hermite PV curve" $
      let c  = L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05),(T.addDays 365 d2,0.045)]
          ps = [(d1,50),(d2,50),(T.addDays 365 d2,1050)]
      in do
        assertEqual "duration on the PV curve" 2.863001
          (fromRational (calcDuration DC_ACT_365F d0 ps c) :: Micro)
        assertEqual "convexity sums all cashflows on the PV curve" 10.323628
          (fromRational (calcConvexity DC_ACT_365F d0 ps c) :: Micro)
  , testCase "convexity sums every cashflow (2-CF)" $
      let c = L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05),(T.addDays 365 d2,0.045)]
      in assertEqual "both cashflow terms are included" 3.599626
           (fromRational (calcConvexity DC_ACT_365F d0 [(d1,50),(d2,50)] c) :: Micro)
  , testCase "node-date cashflows use the node rate" $
      let c  = L.mkRateTs [(d0,0.02),(d1,0.04),(d2,0.05)]
          ps = [(d1,100),(d2,1100)]
      in do
        assertBool "duration uses the node rate, not the previous node"
          (near (fromRational (calcDuration DC_ACT_365F d0 ps c)) (oracleDur c d0 ps))
        assertBool "convexity uses the node rate too"
          (near (fromRational (calcConvexity DC_ACT_365F d0 ps c)) (oracleConv c d0 ps))
  , testCase "flat multi-point curve matches a single-point curve" $
      let ps = [(d1,50),(d2,1050)]
          cFlat = L.mkRateTs [(d0,0.05),(d2,0.05)]
          cOne  = L.mkRateTs [(d0,0.05)]
      in do
        assertEqual "duration parity" (calcDuration DC_ACT_365F d0 ps cOne) (calcDuration DC_ACT_365F d0 ps cFlat)
        assertEqual "convexity parity" (calcConvexity DC_ACT_365F d0 ps cOne) (calcConvexity DC_ACT_365F d0 ps cFlat)
  , testCase "duplicate-date flat curve behaves like a single point" $
      let ps = [(d1,50),(d2,1050)]
          cDup = L.mkRateTs [(d0,0.05),(d0,0.05)]
          cOne = L.mkRateTs [(d0,0.05)]
      in assertEqual "MakeWhole-style [d,d] curve"
           (calcDuration DC_ACT_365F d0 ps cOne) (calcDuration DC_ACT_365F d0 ps cDup)
  , testCase "empty curve keeps the zero-rate fallback" $
      let ps = [(d1,50),(d2,1050)]
          c  = L.mkRateTs []
      in do
        assertBool "duration at 0%" (near (fromRational (calcDuration DC_ACT_365F d0 ps c)) (oracleDur c d0 ps))
        assertBool "convexity at 0%" (near (fromRational (calcConvexity DC_ACT_365F d0 ps c)) (oracleConv c d0 ps))
  , testCase "curve starting after the valuation date uses the first node rate" $
      let c  = L.mkRateTs [(d1,0.02),(d2,0.05)]
          pd = L.toDate "20230601"
          ps = [(L.toDate "20231001",100),(L.toDate "20240601",1100)]
      in do
        assertBool "metrics extend the first node rate to the left"
          (near (fromRational (calcDuration DC_ACT_365F pd ps c)) (oracleDur c pd ps))
        assertBool "convexity same"
          (near (fromRational (calcConvexity DC_ACT_365F pd ps c)) (oracleConv c pd ps))
        assertBool "pv4 still rejects an under-covered curve"
          (isLeft (pv4 (pd, c) ps))
  , testCase "non-rate curve types fall back to getValByDate" $
      let c  = ThresholdCurve [TsPoint d0 0.02, TsPoint d2 0.05]
          ps = [(L.toDate "20230701",50),(L.toDate "20240601",1050)]
          zs = map (fromRational . getValByDate c Exc . fst) ps
      in do
        assertBool "duration fallback"
          (near (fromRational (calcDuration DC_ACT_365F d0 ps c)) (oracleDurRates zs d0 ps))
        assertBool "convexity fallback"
          (near (fromRational (calcConvexity DC_ACT_365F d0 ps c)) (oracleConvRates zs d0 ps))
  ]
  where
    d0 = L.toDate "20230101"
    d1 = L.toDate "20240101"
    d2 = L.toDate "20250101"

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
        (Right (AssetPrice 1037.38 0.76 0.72622 1.189754 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Inc)
    ,testCase "Loan Pricing(Exc Int)" $
      assertEqual "Loan Pricing"
        (Right (AssetPrice 1037.17 0.76 0.72635 1.189978 0.21))
        (priceAsset (PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current)
                    (L.toDate "20241002") 
                    (PvRate 0.03) 
                    (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
                    Nothing 
                    Exc)
    ,testCase "PVCurve on a flat curve matches PvRate pricing" $
      let loan = PersonalLoan (LoanOriginalInfo 1200 (Fix DC_30_360_US 0.08) 12 Monthly (L.toDate "20240701") I_P Nothing) 1000 0.08 10 Current
          d = L.toDate "20241002"
          assumps = (LoanAssump Nothing Nothing Nothing Nothing,DummyDelinqAssump,DummyDefaultAssump)
      in assertEqual "asset PV curve path uses the same rates"
           (priceAsset loan d (PvRate 0.03) assumps Nothing Inc)
           (priceAsset loan d (PVCurve (L.mkRateTs [(d,0.03)])) assumps Nothing Inc)
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