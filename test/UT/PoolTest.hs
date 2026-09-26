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

    schedulePool =
      pool
        { P.assets = [scheduleMortgage 100, scheduleMortgage 300]
        , P.asOfDate = L.toDate "20240101"
        }

    defaultAss =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (40, [1])))
            Nothing
            Nothing
            Nothing
        , A.DummyDelinqAssump
        , A.DummyDefaultAssump
        )

    smallDefaultAss =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (0.10, [1])))
            Nothing
            Nothing
            Nothing
        , A.DummyDelinqAssump
        , A.DummyDefaultAssump
        )

    overDefaultAss =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (500, [1])))
            Nothing
            Nothing
            Nothing
        , A.DummyDelinqAssump
        , A.DummyDefaultAssump
        )

    exactDefaultAss =
      A.PoolLevel
        ( A.MortgageAssump
            (Just (A.DefaultByAmt (400, [1])))
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
