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
import Hedge (RateSwap(..),RateCap(..),RateSwapBase(..),rcNetCash)

capRateTests =
  let
    rc = RateCap LIBOR6M (mkRateTs [(Lib.toDate "20240101",0.035)
                               ,(Lib.toDate "20250101",0.040)])
                         (Fixed 1000)
                 (Lib.toDate "20240101") QuarterEnd (Lib.toDate "20270101")
                 0.03 Nothing 0 Nothing
    indexAssump = [RateFlat LIBOR6M 0.04]
    rc1 = accrueRC td2 (Lib.toDate "20241231") indexAssump rc
  in
    testGroup "Cap Rate Tests"
    [
      testCase "Accure out of scope" $
        assertEqual "before"
          (Right rc)
          (accrueRC td2 (Lib.toDate "20231201") indexAssump rc)
      ,testCase "Accure out of scope" $
        assertEqual "after" 
          (Right rc)
          (accrueRC td2 (Lib.toDate "20280101") indexAssump rc)
      ,testCase "Accrue on flat curve" $
        assertEqual "netCash" 
          (Right 5.0)
          (rcNetCash <$> rc1)
    ]
