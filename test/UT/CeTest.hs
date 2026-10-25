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
import qualified Interface as I



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
    -- regression tests for Liable/Drawable/UseRate class completeness on LiqFacility
    ,let
      liqCeMethod = LiqFacility "LiqCeMethod" (FixSupport 1000) 100 (ByAvailAmount 800) Nothing (Just (IR.Fix DC_ACT_365 0.03)) (Just (IR.Fix DC_ACT_365 0.08))
                    (Just 0.03) (Just 0.08) Nothing 0.25 3.39 (toDate "20220201")
                    Nothing Nothing
      liqFloat = LiqFacility "LiqFloat" (FixSupport 1000) 100 (ByAvailAmount 500) Nothing
                    (Just (IR.Floater DC_ACT_365F LIBOR6M 0.01 0.03 MonthEnd Nothing Nothing Nothing)) Nothing
                    (Just 0.03) Nothing (Just (toDate "20220131")) 0 0 (toDate "20220101")
                    (Just (toDate "20221231")) Nothing
    in
      testGroup "Class-method completeness on LiqFacility"
      [
        testCase "Liable & Drawable on LiqFacility" $
          assertEqual "class methods on liquidity facility"
            (False, 100, 0.25, 0, (0.25+3.39), 0.03, (100+0.25+3.39)
            ,(toDate "20220201"), (toDate "20220201"), (ByAvailAmount 800))
            (I.isPaidOff liqCeMethod
             ,I.getCurBalance liqCeMethod
             ,I.getDueInt liqCeMethod
             ,I.getDueIntOverInt liqCeMethod
             ,I.getTotalDueInt liqCeMethod
             ,I.getCurRate liqCeMethod
             ,I.getOutstandingAmount liqCeMethod
             ,I.getOriginDate liqCeMethod
             ,I.getAccrueBegDate liqCeMethod
             ,I.availForDraw (toDate "20220301") liqCeMethod)
      ,testCase "UseRate on LiqFacility" $
        assertEqual "UseRate on liquidity facility"
          (Nothing, Nothing, False, [], Just 0.01, Just [LIBOR6M], Just LIBOR6M, True, 12)
          (IR.getIndexes liqCeMethod
           ,IR.getSpread liqCeMethod
           ,IR.isAdjustableRate liqCeMethod
           ,IR.getResetDates liqCeMethod
           ,IR.getSpread liqFloat
           ,IR.getIndexes liqFloat
           ,IR.getIndex liqFloat
           ,IR.isAdjustableRate liqFloat
           ,length (IR.getResetDates liqFloat))
      ]
    -- regression tests for UseRate class completeness on CreditDefaultSwap
    ,let
      cds = CDS "Cds0" (Just QuarterEnd) CurrentBondBalance 0 Nothing
              CurrentBondBalance 0.03
              (IR.Floater DC_ACT_365F LIBOR6M 0.01 0.03 MonthEnd Nothing Nothing Nothing)
              0 Nothing Nothing Nothing (-10) (toDate "20220101") (Just (toDate "20221231")) Nothing
    in
      testGroup "Class-method completeness on CreditDefaultSwap"
      [
        testCase "UseRate on CreditDefaultSwap" $
          assertEqual "UseRate on CDS"
            (Just LIBOR6M, Just [LIBOR6M], True, Just 0.01, 12)
            (IR.getIndex cds, IR.getIndexes cds, IR.isAdjustableRate cds, IR.getSpread cds
            ,length (IR.getResetDates cds))
      ]
  ]
