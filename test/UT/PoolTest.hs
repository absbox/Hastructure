module UT.PoolTest (poolTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (isInfixOf)

import qualified AssetClass.AssetBase as AB
import qualified Assumptions as A
import qualified Cashflow as CF
import qualified Lib as L
import qualified Pool as P

import InterestRate (RateType (Fix))
import Types (DayCount (DC_ACT_365F), DatePattern (MonthEnd))

poolTest :: TestTree
poolTest =
  testGroup
    "Pool test"
    [ testCase "pool DefaultByAmt is allocated proportional to current balance" $
        case P.runPool pool (Just defaultAss) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual
              "a total default of 40 (25% and 75%) across the asets"
              [10, 30]
              (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt with many small balances allocates no negative amounts" $
        case P.runPool smallPool (Just smallDefaultAss) Nothing of
          Left err -> assertFailure err
          Right proj ->
            let defaults = totalDefaults <$> proj
            in do
              assertBool "no negative default allocations" (all (>= 0) defaults)
              assertEqual
                "a total default of 0.10 across 12 assets of 0.01 balance"
                (replicate 10 0.01 ++ replicate 2 0)
                defaults
    , testCase "pool DefaultByAmt with ScheduleMortgageFlow assets" $
        case P.runPool schedulePool (Just defaultAss) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual
              "a total default of 40 (25% and 75%) across the schedule assets"
              [10, 30]
              (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt greater than total balance fails with Left" $
        case P.runPool pool (Just overDefaultAss) Nothing of
          Left err ->
            assertBool
              ("error should mention the pool total: " ++ err)
              ("exceeds total current balance" `isInfixOf` err)
          Right _ ->
            assertFailure
              "expected Left when DefaultByAmt total exceeds total current balance"
    , testCase "pool DefaultByAmt equal to total balance is allowed" $
        case P.runPool pool (Just exactDefaultAss) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual
              "each asset defaults its full current balance"
              [100, 300]
              (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt with a zero-balance asset allocates nothing to it" $
        case P.runPool zeroBalPool (Just (assumpWith 80 [1])) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual
              "pro-rata over balances [0,100,300] for a total default of 80"
              [0, 20, 60]
              (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt rounding residual goes to the first asset and preserves the total" $
        case P.runPool tiePool (Just (assumpWith 100 [1])) Nothing of
          Left err -> assertFailure err
          Right proj ->
            let ds = totalDefaults <$> proj
            in do
              assertEqual
                "largest-remainder tie breaks by asset index (first asset gets the extra cent)"
                [33.34, 33.33, 33.33]
                ds
              assertEqual "allocations sum to the requested total" 100 (sum ds)
    , testCase "pool DefaultByAmt across a single asset allocates the full amount" $
        case P.runPool singlePool (Just (assumpWith 40 [1])) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual "one asset gets the whole default" [40] (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt with an empty pool and positive total fails with Left" $
        case P.runPool emptyPool (Just (assumpWith 40 [1])) Nothing of
          Left err ->
            assertBool
              ("error should mention the pool total: " ++ err)
              ("exceeds total current balance" `isInfixOf` err)
          Right _ ->
            assertFailure "expected Left for an empty pool with a positive default"
    , testCase "pool DefaultByAmt with an empty pool and zero total returns no projections" $
        case P.runPool emptyPool (Just (assumpWith 0 [1])) Nothing of
          Left err -> assertFailure err
          Right proj -> assertEqual "no assets to project" [] proj
    , testCase "pool DefaultByAmt of zero allocates zero to every asset" $
        case P.runPool pool (Just (assumpWith 0 [1])) Nothing of
          Left err -> assertFailure err
          Right proj -> assertEqual "no defaults" [0, 0] (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt applies multi-period weights per asset" $
        case P.runPool pool (Just (assumpWith 40 [0.5, 0.5])) Nothing of
          Left err -> assertFailure err
          Right proj -> do
            assertEqual
              "positive per-period defaults follow the weights of each allocated amount"
              [[5, 5], [15, 15]]
              (positiveDefaults <$> proj)
            assertEqual "per-asset totals" [10, 30] (totalDefaults <$> proj)
    , testCase "pool DefaultByAmt with a negative total currently clamps to zero allocations" $
        case P.runPool pool (Just (assumpWith (-5) [1])) Nothing of
          Left err -> assertFailure err
          Right proj ->
            assertEqual "negative total is clamped to no defaults" [0, 0] (totalDefaults <$> proj)
    ]
  where
    pool =
      P.Pool
        { P.assets = [mortgage 100, mortgage 300]
        , P.futureCf = Nothing
        , P.futureScheduleCf = Nothing
        , P.asOfDate = L.toDate "20240101"
        , P.issuanceStat = Nothing
        , P.extendPeriods = Nothing
        }

    smallPool = pool {P.assets = replicate 12 (mortgage 0.01)}

    zeroBalPool = pool {P.assets = [mortgage 0, mortgage 100, mortgage 300]}

    tiePool = pool {P.assets = [mortgage 100, mortgage 100, mortgage 100]}

    singlePool = pool {P.assets = [mortgage 100]}

    emptyPool = pool {P.assets = ([] :: [AB.Mortgage])}

    schedulePool =
      pool
        { P.assets = [scheduleMortgage 100, scheduleMortgage 300]
        , P.asOfDate = L.toDate "20240101"
        }

    defaultAss = assumpWith 40 [1]

    smallDefaultAss = assumpWith 0.10 [1]

    overDefaultAss = assumpWith 500 [1]

    exactDefaultAss = assumpWith 400 [1]

    assumpWith total vs =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (total, vs)))
            Nothing
            Nothing
            Nothing
        , A.DummyDelinqAssump
        , A.DummyDefaultAssump
        )

    mortgage balance =
      AB.Mortgage
        ( AB.MortgageOriginalInfo
            balance
            (Fix DC_ACT_365F 0.08)
            12
            L.Monthly
            (L.toDate "20240101")
            AB.Level
            Nothing
            Nothing
        )
        balance
        0.08
        12
        Nothing
        AB.Current

    scheduleMortgage balance =
      AB.ScheduleMortgageFlow
        (L.toDate "20240101")
        [ CF.MortgageFlow (L.toDate d) balance 0 0 0 0 0 0 0.08 Nothing Nothing Nothing
        | d <- ["20240101", "20240201", "20240301"]
        ]
        MonthEnd

    totalDefaults (CF.CashFlowFrame _ txns, _) =
      sum (CF.mflowDefault <$> txns)

    positiveDefaults (CF.CashFlowFrame _ txns, _) =
      filter (> 0) (CF.mflowDefault <$> txns)
