module UT.RateHedgeTest(capRateTests)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Types
import Util
import Stmt
import Deal.DealAction (accrueRC)
import Data.Ratio 
import UT.DealTest (td2)
import Deal.DealBase as D

import Hedge (RateSwap(..),RateCap(..),RateSwapType(..),RateSwapBase(..),SRT(..),SrtType(..),rcNetCash)
import qualified InterestRate as IR
import qualified Interface as I

capRateTests =
  let
    rc = RateCap LIBOR6M (mkRateTs [(Lib.toDate "20240101",0.035)
                                ,(Lib.toDate "20250101",0.040)])
                          (Fixed 1000)
                  (Lib.toDate "20240101") QuarterEnd (Lib.toDate "20270101")
                  0.03 Nothing 0 Nothing
    indexAssump = [RateFlat LIBOR6M 0.04]
    ctx = D.defaultRunContext
    rc1 = accrueRC td2 ctx (Lib.toDate "20241231") indexAssump rc
  in
    testGroup "Cap Rate Tests"
    [
      testCase "Accure out of scope" $
        assertEqual "before"
          (Right rc)
          (accrueRC td2 ctx (Lib.toDate "20231201") indexAssump rc)
      ,testCase "Accure out of scope" $
        assertEqual "after" 
          (Right rc)
          (accrueRC td2 ctx (Lib.toDate "20280101") indexAssump rc)
      ,testCase "Accrue on flat curve" $
        assertEqual "netCash" 
          (Right 5.0)
          (rcNetCash <$> rc1)
    -- regression tests for Liable/UseRate class completeness on RateSwap/SRT/RateCap
    ,let
      rs = RateSwap{rsType = FloatingToFixed (LIBOR6M,0.01) 0.05
                    ,rsDayCount = DC_ACT_365F
                    ,rsSettleDates = Nothing
                    ,rsUpdateDates = MonthEnd
                    ,rsNotional = Fixed 1000
                    ,rsRefBalance = 1000
                    ,rsPayingRate = 0.05
                    ,rsReceivingRate = 0.03
                    ,rsNetCash = -50.0
                    ,rsStartDate = Lib.toDate "20220101"
                    ,rsLastStlDate = Nothing
                    ,rsStmt = Nothing}
      srt = SRT "Srt0" (SrtByEndDay CurrentBondBalance MonthEnd) (IR.Fix DC_ACT_365 0.03)
            80 0.03 100 Nothing 1.5 (Lib.toDate "20220101") Nothing Nothing
    in
      testGroup "Class-method completeness on hedge instruments"
      [
        testCase "Liable on RateSwap" $
          assertEqual "class methods on rate swap"
            (False, 50, 50, 0, 0, 0, 0.05, (Lib.toDate "20220101"), (Lib.toDate "20220101"))
            (I.isPaidOff rs
             ,I.getCurBalance rs
             ,I.getOutstandingAmount rs
             ,I.getOriginBalance rs
             ,I.getDueInt rs
             ,I.getDueIntOverInt rs
             ,I.getCurRate rs
             ,I.getOriginDate rs
             ,I.getAccrueBegDate rs)
      ,testCase "Liable on SRT" $
          assertEqual "class methods on SRT"
            (False, 100, 100, 0.03, 1.5, 0, 1.5, 101.5, (Lib.toDate "20220101"), (Lib.toDate "20220101"))
            (I.isPaidOff srt
             ,I.getCurBalance srt
             ,I.getOriginBalance srt
             ,I.getCurRate srt
             ,I.getDueInt srt
             ,I.getDueIntOverInt srt
             ,I.getTotalDueInt srt
             ,I.getOutstandingAmount srt
             ,I.getOriginDate srt
             ,I.getAccrueBegDate srt)
      ,testCase "UseRate on RateSwap & SRT & RateCap" $
        assertEqual "UseRate on hedge instruments"
          (Just LIBOR6M, Just [LIBOR6M], Just 0.01, True
          ,Nothing, Nothing, False
          ,Just LIBOR6M, Just [LIBOR6M], True)
          (IR.getIndex rs
           ,IR.getIndexes rs
           ,IR.getSpread rs
           ,IR.isAdjustableRate rs
           ,IR.getIndex srt
           ,IR.getSpread srt
           ,IR.isAdjustableRate srt
           ,IR.getIndex rc
           ,IR.getIndexes rc
           ,IR.isAdjustableRate rc)
      ,testCase "getSpread of a floating SRT" $
        assertEqual "spread from SRT premium rate type"
          (Just 0.01)
          (IR.getSpread (srt{srtPremiumType = IR.Floater DC_ACT_365F LIBOR6M 0.01 0.03 MonthEnd Nothing Nothing Nothing}))
      ]
    ]
