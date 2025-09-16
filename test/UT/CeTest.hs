module UT.CeTest(liqTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Util
import Stmt
import Data.Ratio
import qualified Data.DList as DL
import Types
import CreditEnhancement
import qualified InterestRate as IR



liqTest = testGroup "Pricing Tests"
  [
    let
        liqStmt1 = [
            -- SupportTxn (toDate "20220101") 100,
            ]
        liq0 = LiqFacility "Liq0" (FixSupport 1000) 100 (ByAvailAmount 500) Nothing Nothing Nothing 
                (Just 0.03) (Just 0.08) Nothing 0 0 (toDate "20220101")
                Nothing Nothing
    in
      testCase "First Accure" $
        assertEqual "First Accure"  
          (Just (Statement (DL.fromList [SupportTxn (toDate "20220101") (ByAvailAmount 500) 100 0 0 0 Empty
                            ,SupportTxn (toDate "20220201") (ByAvailAmount 500) 100 0.25 3.39 0 (LiquidationSupportInt 0.25 3.39)])))
          (liqStmt (accrueLiqProvider (toDate "20220201") liq0 ))
    ,let
        liqStmt1 = DL.fromList [
            SupportTxn (toDate "20220101") (ByAvailAmount 500) 100 0 0 0 Empty
            ,SupportTxn (toDate "20220201") (ByAvailAmount 800) 100 0.25 3.39 0 (LiquidationSupportInt 0.25 3.39)
            ]
        liq1 = LiqFacility "Liq1" (FixSupport 1000) 100 (ByAvailAmount 800) Nothing (Just (IR.Fix DC_ACT_365 0.03)) (Just (IR.Fix DC_ACT_365 0.08))  
                (Just 0.03) (Just 0.08) Nothing 0.25 3.39 (toDate "20220201")
                Nothing (Just (Statement liqStmt1))

    in
      testCase "Accure on unused balance" $
        assertEqual "with one history txn"  
          (Just (Statement (DL.fromList [SupportTxn (toDate "20220101") (ByAvailAmount 500) 100 0 0 0 Empty
                            ,SupportTxn (toDate "20220201") (ByAvailAmount 800) 100 0.25 3.39 0 (LiquidationSupportInt 0.25 3.39)
                            ,SupportTxn (toDate "20220301") (ByAvailAmount 800) 100 0.48 8.29 0 (LiquidationSupportInt 0.23 4.9)])))
          (liqStmt (accrueLiqProvider (toDate "20220301") liq1 )) 
    ,let
        liqStmt1 = DL.fromList [
            SupportTxn (toDate "20220101") (ByAvailAmount 500) 100 0 0 0 Empty
            ,SupportTxn (toDate "20220201") (ByAvailAmount 800) 100 0.25 3.39 0 (LiquidationSupportInt 0.25 3.39)
            ,SupportTxn (toDate "20220301") (ByAvailAmount 1000) 100 0.48 8.29 0 (LiquidationSupportInt 0.23 4.9)
            ]
        liq1 = LiqFacility "Liq2" (FixSupport 1000) 100 (ByAvailAmount 1000) Nothing (Just (IR.Fix DC_ACT_365 0.03)) (Just (IR.Fix DC_ACT_365 0.08))  
                (Just 0.03) (Just 0.08) Nothing 0.25 3.39 (toDate "20220101")
                Nothing (Just (Statement liqStmt1))

    in
      testCase "Accure on unused balance " $
        assertEqual "with multiple history txn"  
          (Just (Statement (DL.fromList [SupportTxn (toDate "20220101") (ByAvailAmount 500) 100 0 0 0 Empty
                            ,SupportTxn (toDate "20220201") (ByAvailAmount 800) 100 0.25 3.39 0 (LiquidationSupportInt 0.25 3.39)
                            ,SupportTxn (toDate "20220301") (ByAvailAmount 1000) 100 0.48 8.29 0 (LiquidationSupportInt 0.23 4.9)
                            ,SupportTxn (toDate "20220401") (ByAvailAmount 1000) 100 0.99 18.49 0 (LiquidationSupportInt 0.74 15.10)
                            ])))
          (liqStmt (accrueLiqProvider (toDate "20220401") liq1 ))    
  ]
