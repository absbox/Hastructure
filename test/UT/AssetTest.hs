module UT.AssetTest(mortgageTests,mortgageCalcTests,loanTests,leaseTests,installmentTest,armTest,ppyTest
                   ,delinqScheduleCFTest,delinqMortgageTest,btlMortgageTest,nonPayMortgageTest,receivableTest,fixedAssetTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as Ast
import qualified Pool as P
import qualified AssetClass.AssetBase as AB
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.Loan as ACL
import qualified AssetClass.Lease as ACR
import qualified AssetClass.Installment as ACI
import qualified AssetClass.Receivable as AR
import qualified Assumptions as A
import qualified Cashflow as CF
import qualified Deal as D
import Types
import Data.Either
import InterestRate

import Debug.Trace
import Control.Lens hiding (element)
import Control.Lens.TH
debug = flip trace

dummySt = (0,L.toDate "19000101",Nothing)

tm = AB.Mortgage
     (AB.MortgageOriginalInfo 10000 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Level Nothing Nothing)
     8000 0.08 19 
     Nothing
     AB.Current

tm1 = AB.Mortgage
     (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Even Nothing Nothing)
     240 0.08 19 
     Nothing
     AB.Current

tm2 = AB.Mortgage
     (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Even Nothing Nothing)
     240 0.08 19 
     Nothing 
     (AB.Defaulted Nothing)

tm4 = AB.Mortgage
        (AB.MortgageOriginalInfo 240 (Fix DC_30_360_US 0.08) 36 L.Monthly (L.toDate "20210701") (AB.Balloon 120) Nothing Nothing)
        120 0.08 36
        Nothing 
        AB.Current

tm5 = AB.Mortgage
        (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 36 L.Monthly (L.toDate "20210101") (AB.Balloon 120)  Nothing Nothing)
        100 0.08 24 
        Nothing 
        AB.Current


tm6 = AB.Mortgage
        (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 36 L.Monthly (L.toDate "20210101") (AB.Balloon 120) Nothing Nothing)
        120 0.08 36
        Nothing 
        AB.Current

tm7 = AB.Mortgage
        (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 36 L.Monthly (L.toDate "20210101") (AB.Balloon 120)  Nothing Nothing)
        120 0.08 24
        Nothing 
        AB.Current



asOfDate = L.toDate "20210605"

(tmcf_00,_) = case Ast.projCashflow tm asOfDate (A.MortgageAssump Nothing Nothing Nothing Nothing,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of
                Left _ -> undefined
                Right x -> x
trs = tmcf_00^.CF.cashflowTxn  
(tmcf_default,_) = case Ast.projCashflow tm asOfDate (A.MortgageAssump (Just (A.DefaultConstant 0.015)) Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of
                     Left _ -> undefined
                     Right x -> x


mortgageCalcTests = testGroup "Mortgage Calc Test" 
  [
    testCase "Calc Pmt 1" $
        assertEqual "PMT 01"
           154.15 (AB.calcPmt 1200 0.12 24),
    testCase "Calc Pmt 2" $
        assertEqual "PMT 02"
           100.0 (AB.calcPmt 1200 0.0 12)
  ]


mortgageTests = testGroup "Mortgage cashflow Tests"
  [
    testCase "Fix rate mortgage" $
     --  19 @=? (CF.sizeCashFlowFrame tmcf_00)
     assertEqual "total size of cf" 19 (CF.sizeCashFlowFrame tmcf_00) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "first Date" $
     assertEqual "first date" (L.toDate "20210701")  (CF.getDate (head trs)) -- `debug` ("result"++show(tmcf_00))
     --assertEqual "total size of cf" 19 19
     ,
     testCase "Even Principal Type of Mortgage" $
     let
        tm1cf_00 = case Ast.calcCashflow tm1 asOfDate Nothing of
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn  
     in
        assertEqual "first row" 12.63  (CF.mflowPrincipal (head trs)) -- `debug` ("result"++show(tmcf_00))
     ,
     testCase "Default asset won't have cashflow if no assumption" $
     let
        asDay = L.toDate "20220101"
        (tm2cf_00, _) = case Ast.projCashflow tm2 asDay (A.MortgageAssump Nothing Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of
                         Left _ -> undefined
                         Right x -> x
        trs = tm2cf_00 ^. CF.cashflowTxn  
     in
        assertEqual "Empty for principal"
                    (0.0, asDay, 1)
                    (CF.mflowPrincipal (head trs)
                    ,(view CF.tsDate (head trs))
                    ,length trs)
     ,
     testCase "Balloon Mortgage test 1" $
     let
        tm1cf_00 = case Ast.calcCashflow tm4 asOfDate Nothing of-- `debug` (">>>")
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn  
     in
        assertEqual "first & last row row" 
                    [94.29,0.62,0.66, 0.79] 
                    [CF.mflowPrincipal (last trs)
                    ,CF.mflowInterest (last trs)
                    ,(CF.mflowPrincipal . head . tail) trs
                    ,(CF.mflowInterest . head . tail) trs ] -- `debug` ("trs for balloon"++show tm1cf_00)
     ,
     testCase "Balloon Mortgage test 2" $
     let
        tm1cf_00 = case Ast.calcCashflow tm5 asOfDate Nothing of
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn 
     in
        assertEqual "first & last row row" 
                    [84.19,0.56,0.64, 0.66] 
                    [CF.mflowPrincipal (last trs)
                    ,CF.mflowInterest (last trs)
                    ,(CF.mflowPrincipal . head . tail) trs
                    ,(CF.mflowInterest . head . tail) trs ] -- `debug` ("trs for balloon"++show tm1cf_00)
     ,testCase "Balloon Mortgage test 3" $
     let
        (tm1cf_00,_) = case Ast.projCashflow tm6 (L.toDate "20201205")
                         (A.MortgageAssump Nothing (Just (A.PrepaymentCPR 0.1)) Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of 
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn 
     in
        assertEqual "first & last row row" 
                    [68.77, 0.45, 1.06, 0.65, 0.79] 
                    [CF.mflowPrincipal (last trs)
                    ,CF.mflowInterest (last trs)
                    ,(CF.mflowPrepayment) (trs!!1)
                    ,(CF.mflowPrincipal) (trs!!1)
                    ,(CF.mflowInterest) (trs!!1) ] -- `debug` ("trs for balloon"++show tm1cf_00)
    ,testCase "Balloon Mortgage test 4" $
     let
        (tm1cf_00,_) = case Ast.projCashflow tm7 (L.toDate "20201205")
                         (A.MortgageAssump Nothing (Just (A.PrepaymentCPR 0.1)) Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn
     in
        assertEqual "first & last row row" 
                    ([82, 0.73, 0.54, 1.06, 0.75, 0.79], 25) 
                    ([CF.mflowPrincipal (last trs)
                    ,CF.mflowPrepayment (last trs)
                    ,CF.mflowInterest (last trs)
                    ,(CF.mflowPrepayment) (trs!!1)
                    ,(CF.mflowPrincipal) (trs!!1)
                    ,(CF.mflowInterest) (trs!!1) 
                    ], CF.sizeCashFlowFrame tm1cf_00) -- `debug` ("trs for balloon"++show tm1cf_00)
    ,testCase "Balloon Mortgage test 5" $
     let
        (tm1cf_00,_) = case Ast.projCashflow tm7 (L.toDate "20201205")
                         (A.MortgageAssump (Just (A.DefaultAtEndByRate 0.05 0.1)) Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of
                         Left _ -> undefined
                         Right x -> x
        trs = tm1cf_00 ^. CF.cashflowTxn 
     in
        assertEqual "first & last row row" 
                    ([74.34, 17.43, 0.49, 0.52, 0.76, 0.79], 25) 
                    ([CF.mflowPrincipal (last trs)
                    ,CF.mflowDefault (last trs)
                    ,CF.mflowInterest (last trs)
                    ,(CF.mflowDefault) (trs!!1)
                    ,(CF.mflowPrincipal) (trs!!1)
                    ,(CF.mflowInterest) (trs!!1) 
                    ], CF.sizeCashFlowFrame tm1cf_00) -- `debug` ("trs for balloon"++show tm1cf_00)        
  ]

loanTests = 
    let 
      loan1 =  AB.PersonalLoan
                 (AB.LoanOriginalInfo 180 (Fix DC_ACT_365F 0.08) 36 L.Monthly (L.toDate "20200101") AB.I_P Nothing) 
                 120
                 0.08
                 24
                 AB.Current
      asofDate = L.toDate "20200615"
      loan1Cf = case Ast.calcCashflow loan1 asofDate Nothing of
                  Left _ -> undefined
                  Right x -> x
      (loan2Cf,_) = case Ast.projCashflow loan1 asofDate (A.LoanAssump Nothing Nothing Nothing Nothing ,A.DummyDelinqAssump,A.DummyDefaultAssump) Nothing of 
                  Left _ -> undefined
                  Right x -> x

    in 
      testGroup "Loan cashflow Tests" [ 
       testCase "Loan 1" $
           assertEqual "project period"
             25
             (CF.sizeCashFlowFrame loan1Cf)
      -- ,testCase "First cashflow" $
      --     assertEqual ""
      --      (Just (CF.LoanFlow (L.toDate "20210201") 120 0 0.82 0 0 0 0 0.08))
      --      (CF.cfAt loan1Cf 0)
      -- ,testCase "Last Principal Amount" $
      --     assertEqual ""
      --      (Just (CF.LoanFlow (L.toDate "20230101") 0 120 0.82 0 0 0 0 0.08))
      --      (CF.cfAt loan1Cf 23)
      -- ,testCase "calcCashflow == projCashflow when assump = []" $
      --     assertEqual ""
      --     loan1Cf
      --     loan2Cf
     ]

leaseTests = 
    let 
      lease1 = AB.RegularLease
                (AB.LeaseInfo (L.toDate "20230101") 12 (AB.ByDayRate 1 MonthEnd) Nothing)
                100
                12
                AB.Current
      asofDate = L.toDate "20230615"
      cf1 = case Ast.calcCashflow lease1 asofDate Nothing of 
              Left _ -> undefined
              Right x -> x
 
      lease2 = AB.StepUpLease
                (AB.LeaseInfo (L.toDate "20230601") 12 (AB.ByDayRate 1 MonthEnd) Nothing)
                (AB.FlatRate 1.02)
                100
                12
                AB.Current
      cf2 = case Ast.calcCashflow lease2 asofDate Nothing of -- 2020 06 15
              Left _ -> undefined
              Right x -> x
      
      lease3 = AB.StepUpLease
                (AB.LeaseInfo (L.toDate "20230401") 4 (AB.ByDayRate 1 MonthEnd) Nothing)
                (AB.ByRateCurve [1.04,1.05,1.06])
                100
                4
                AB.Current
      cf3_0 = case Ast.calcCashflow lease3 (L.toDate "20230415") Nothing of
              Left _ -> undefined
              Right x -> x
      cf3 = case Ast.calcCashflow lease3 asofDate Nothing of
              Left _ -> undefined
              Right x -> x
      (cf4,_) = case Ast.projCashflow lease1 asofDate 
                  (A.LeaseAssump Nothing
                                 (A.GapDays 45)
                                 (A.BaseAnnualRate 0.0)
                                 (A.CutByDate (L.toDate "20240601"))
                                 
                   ,A.DummyDelinqAssump,A.DummyDefaultAssump)
                   Nothing of 
                  Left _ -> undefined
                  Right x -> x
      -- (cf5,_) =  case Ast.projCashflow lease1 asofDate 
      --              (A.LeaseAssump Nothing
      --                             (A.GapDaysByAmount [(0.5,12),(1,22),(2,62),(3,82)] 92)
      --                             (A.BaseAnnualRate 0.0)
      --                             (A.CutByDate (L.toDate "20240601"))
      --                             
      --              ,A.DummyDelinqAssump,A.DummyDefaultAssump)
      --              Nothing of
      --             Left _ -> undefined
      --             Right x -> x
    in 
      testGroup "Lease CF Test" [
        testCase "1 year Regular Lease sum of rentals/dates" $
            assertEqual "Dates"
                (L.toDates ["20230131","20230228","20230331","20230430","20230531","20230630"
                            ,"20230731","20230831","20230930","20231031","20231130","20231231"])
                (Ast.getPaymentDates lease1 0)
        ,testCase "1 year Regular Lease sum of rentals/dates" $
            assertEqual "cf dates"
                (L.toDates ["20230630","20230731","20230831","20230930","20231031","20231130","20231231"])
                (CF.getDate <$> (cf1 ^. CF.cashflowTxn))
        ,testCase "1 year Regular Lease sum of rentals/first" $
            assertEqual "First flow"
                (CF.LeaseFlow (L.toDate "20230630") 184.00 30.0 0.0)
                (head (cf1 ^. CF.cashflowTxn))
        ,testCase "1 year Regular Lease sum of rentals/last" $
            assertEqual "Last flow"
                (CF.LeaseFlow (L.toDate "20231231") 0.00 31.0 0.0)
                (last (cf1 ^. CF.cashflowTxn))
        ,testCase "1 year Regular Lease sum of rentals" $
            assertEqual "total rental"
                214
                (sum $ map CF.tsTotalCash (cf1 ^. CF.cashflowTxn))
        ,testCase "1 year Regular Lease first pay date" $
            assertEqual "first date of regular lease"
                (L.toDate "20230630")
                (head (CF.getDatesCashFlowFrame cf1))
        ,testCase "1 year Stepup lease first pay" $
            assertEqual "first pay"
                (CF.LeaseFlow (L.toDate "20230630") 376.24 29 0.0)
                (head (cf2 ^. CF.cashflowTxn))
        ,testCase "1 year Stepup lease" $
            assertEqual "total rental"
                405.24
                (sum $ map CF.tsTotalCash (cf2 ^. CF.cashflowTxn))
        ,testCase "1 year Stepup lease" $
            assertEqual "first rental step up at Month 2"
                (CF.LeaseFlow (L.toDate "20230731") 344.62 31.62 0.0)
                ((cf2 ^. CF.cashflowTxn)!!1)
        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 0"
                (CF.LeaseFlow (L.toDate "20230430") 100.59 29.0 0.0)
                (head (cf3_0 ^. CF.cashflowTxn )) 
        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 1"
                (CF.LeaseFlow (L.toDate "20230630") 35.65 32.7 0.0)
                (head (cf3 ^. CF.cashflowTxn)) -- `debug` ("CF3->"++show cf3)
        ,testCase "1 year Stepup Curve lease" $
            assertEqual "first rental step up at Month 2"
                (CF.LeaseFlow (L.toDate "20230731") 0 35.65 0.0)
                ((cf3 ^. CF.cashflowTxn)!!1)
        ,testCase "Lease with Assumptions" $ 
            assertEqual "Month Gap=45 days"
            ((CF.LeaseFlow (L.toDate "20240630") 215 30.0 0.0),(CF.LeaseFlow (L.toDate "20250131") 0 31 0))
            (((cf4 ^. CF.cashflowTxn)!!11),(last (cf4 ^. CF.cashflowTxn))) -- `debug` ("CF4->"++show cf4)
        -- ,testCase "Lease with Assumptions" $ 
        --     assertEqual "Month Gap by Table : New Lease at period 0"
        --     (CF.LeaseFlow (L.toDate "20240131") 335 8)
        --     ((cf5 ^. CF.cashflowTxn)!!7) `debug` ("CF5->"++show cf5)
        -- ,testCase "Lease with Assumptions" $ 
        --     assertEqual "Month Gap by Table : New Lease at period 1"
        --     (CF.LeaseFlow (L.toDate "20240229") 306 29)
        --     ((cf5 ^. CF.cashflowTxn)!!8)
      ]

installmentTest = 
    let 
      loan1 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P Nothing)
                 1000 
                 12 
                 AB.Current
      asofDate1 = L.toDate "20220115"
      loan1Cf = case Ast.calcCashflow loan1 asofDate1 Nothing of
                  Left _ -> undefined
                  Right x -> x

      loan2 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P Nothing) 
                 500 
                 12
                 AB.Current
      loan2Cf = case Ast.calcCashflow loan2 asofDate1 Nothing of
                  Left _ -> undefined
                  Right x -> x

      asofDate2 = L.toDate "20220815"
      loan3 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P Nothing) 
                 416.69 
                 5
                 AB.Current
      loan3Cf = case Ast.calcCashflow loan3 asofDate2 Nothing of
                  Left _ -> undefined
                  Right x -> x

      loan4 =  AB.Installment
                 (AB.LoanOriginalInfo 1000 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") AB.F_P Nothing) 
                 208.35 
                 5
                 AB.Current
      loan4Cf = case Ast.calcCashflow loan4 asofDate2 Nothing of
                  Left _ -> undefined
                  Right x -> x
      
      loan5 =  AB.Installment
                 (AB.LoanOriginalInfo 1200 (Fix DC_ACT_365F 0.01) 12 L.Monthly (L.toDate "20220101") (AB.PO_FirstN 4) Nothing) 
                 1000
                 10
                 AB.Current
      loan5Cf = case Ast.calcCashflow loan5 (L.toDate "20220101") Nothing of 
                  Left _ -> undefined
                  Right x -> x
    in 
      testGroup "Installment cashflow Tests" [ 
       testCase "Loan 1" $
           assertEqual "project period size"
             12 
             (CF.sizeCashFlowFrame loan1Cf)
      ,testCase "Loan 1 (on schedule)" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220201") 916.67 83.33 10 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan1Cf 0)
      ,testCase "Stressed Loan 1" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220201") 458.33 41.66 5 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan2Cf 0)
      ,testCase "Loan 2 with aging(on schedule)" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220901") 333.36 83.33 10 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan3Cf 0)
      ,testCase "Stress Loan 2 with aging" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220901") 166.68 41.66 5 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan4Cf 0)
      ,testCase "First No Fee Loan at first period" $
           assertEqual "Balance/Principal/Int at period 1"
             (Just (CF.LoanFlow (L.toDate "20220301") 1000 0 0 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan5Cf 0)
      ,testCase "First No Fee Loan at first period" $
           assertEqual "Balance/Principal/Int at period 3"
             (Just (CF.LoanFlow (L.toDate "20220501") 800 100 0 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan5Cf 2)
      ,testCase "First No Fee Loan at first period" $
           assertEqual "Balance/Principal/Int at period 4"
             (Just (CF.LoanFlow (L.toDate "20220601") 700 100 12 0 0 0 0 0.01 Nothing))
             (CF.cfAt loan5Cf 3) -- `debug` (show loan5Cf)
      ]


armTest = 
  let 
    arm1 = AB.AdjustRateMortgage
            (AB.MortgageOriginalInfo 
              240 
              (Floater DC_ACT_365F SOFR3M 0.01 0.03 (EveryNMonth (L.toDate "20240801") 2) Nothing Nothing Nothing)
              30
              Monthly
              (L.toDate "20230501")
              AB.Level
              Nothing
              Nothing)
            (ARM 12 (Just 0.015) (Just 0.01) (Just 0.09) (Just 0.02) )  
            240 0.08 19 
            Nothing 
            AB.Current
    assump1 = RateCurve 
                SOFR3M
                (IRateCurve [TsPoint (L.toDate "20240501") 0.05 
                            ,TsPoint (L.toDate "20240901") 0.065
                            ,TsPoint (L.toDate "20241215") 0.07
                            ,TsPoint (L.toDate "20250315") 0.10
                            ,TsPoint (L.toDate "20251001") 0.12
                            ])
                
    (arm1_cf,_) = let 
                    cf = Ast.projCashflow arm1 (L.toDate "20230601") (A.MortgageAssump Nothing Nothing Nothing Nothing
                                                            ,A.DummyDelinqAssump,A.DummyDefaultAssump) 
                                                            (Just [assump1])
                  in 
                    case cf of 
                      Left _ -> undefined 
                      Right x -> x

  in 
    testGroup "ARM cashflow tests" [
      testCase "ARM case 1/ cf length" $
        assertEqual "should be 19"
        20
        (CF.sizeCashFlowFrame arm1_cf)
      ,testCase "ARM case 1/ first cash" $
        assertEqual "first cash row"
        (Just (CF.MortgageFlow (L.toDate "20240501") 227.66 12.34 0.6 0 0 0 0 0.03 Nothing Nothing (Just (12.34,0.00,0.00,0.00,0.00,0.00)) ))
        (CF.cfAt arm1_cf 1)
      ,testCase "ARM case 1/ frist reset" $
        assertEqual "first rate"
        (Just (CF.MortgageFlow (L.toDate "20240601") 215.41 12.25 0.85 0 0 0 0 0.045 Nothing Nothing (Just (24.59,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 2)
      ,testCase "ARM case 1/periodic reset " $
        assertEqual "first rate"
        (Just (CF.MortgageFlow (L.toDate "20240801") 190.85 12.26 0.93 0 0 0 0 0.055 Nothing Nothing (Just (49.15,0.00,0.00,0.00,0.00,0.00)) ))
        (CF.cfAt arm1_cf 4)
      ,testCase "ARM case 1/remains same before next reset" $
        assertEqual "period before first reset"
        (Just (CF.MortgageFlow (L.toDate "20240901") 178.53 12.32 0.87 0 0 0 0 0.055 Nothing Nothing (Just (61.47,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 5)
      ,testCase "ARM case 1" $
        assertEqual "reset with periodic cap"
        (Just (CF.MortgageFlow (L.toDate "20241201") 141.47 12.38 0.96 0 0 0 0 0.075 Nothing Nothing (Just (98.53,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 8)
      ,testCase "ARM case 1" $
        assertEqual "Period 9"
        (Just (CF.MortgageFlow (L.toDate "20250101") 129.01 12.46 0.88 0 0 0 0 0.075 Nothing Nothing (Just (110.99,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 9)
      ,testCase "ARM case 1" $
        assertEqual "Period 10"
        (Just (CF.MortgageFlow (L.toDate "20250201") 116.49 12.52 0.85 0 0 0 0 0.08 Nothing Nothing (Just (123.51,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 10)
      ,testCase "ARM case 1" $
        assertEqual "life cap"
        (Just (CF.MortgageFlow (L.toDate "20250401") 91.24 12.65 0.77 0 0 0 0 0.09 Nothing Nothing (Just (148.76,0.00,0.00,0.00,0.00,0.00))))
        (CF.cfAt arm1_cf 12)

    ]

---- prepayment penalty 

ppy_1 = Just $ AB.ByTerm 3 0.1 0.01
ppy_2 = Just $ AB.FixAmount 100 Nothing
ppy_2_1 = Just $ AB.FixAmount 100 (Just 2)
ppy_3 = Just $ AB.FixPct 0.01 Nothing
ppy_3_1 = Just $ AB.FixPct 0.01 (Just 2)
ppy_4 = Just $ AB.Sliding 0.1 0.01
ppy_5 = Just $ AB.StepDown [(2,0.5),(12,0.2)]

origin_info = AB.MortgageOriginalInfo 10000 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.Level Nothing Nothing

tm_ppy_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_2 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_2}) 10000 0.08 24 Nothing AB.Current
tm_ppy_2_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_2_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_3 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_3}) 10000 0.08 24 Nothing AB.Current
tm_ppy_3_1 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_3_1}) 10000 0.08 24 Nothing AB.Current
tm_ppy_4 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_4}) 10000 0.08 24 Nothing AB.Current
tm_ppy_5 = AB.Mortgage (origin_info { AB.prepaymentPenalty = ppy_5}) 10000 0.08 24 Nothing AB.Current

ppyTest = 
  let 
    assump1 = (A.MortgageAssump Nothing (Just (A.PrepaymentCPR 0.03)) Nothing Nothing,A.DummyDelinqAssump,A.DummyDefaultAssump)

    (ppy_cf_1,_) = case Ast.projCashflow tm_ppy_1 (L.toDate "20210101") assump1 Nothing of 
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_2,_) = case Ast.projCashflow tm_ppy_2 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_2_1,_) = case Ast.projCashflow tm_ppy_2_1 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_3,_) = case Ast.projCashflow tm_ppy_3 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_3_1,_) = case Ast.projCashflow tm_ppy_3_1 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_4,_) = case Ast.projCashflow tm_ppy_4 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x
    (ppy_cf_5,_) = case  Ast.projCashflow tm_ppy_5 (L.toDate "20210101") assump1 Nothing of
                    Left _ -> undefined
                    Right x -> x

  in 
    testGroup "Prepay Penalty tests" [
      testCase "ppy case 1" $
        assertEqual " using rate0"
        (Just (CF.MortgageFlow (L.toDate "20210201") 9589.55 384.62 66.48 25.83 0 0 0 0.08 Nothing (Just 2.58) (Just (384.62,25.83,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_1 1)
      ,testCase "ppy case 1" $
        assertEqual " using rate1"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just 0.21 ) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_1 4)
      ,testCase "ppy case 2" $
        assertEqual " using fix amount"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just 100 ) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_2 4)
      ,testCase "ppy case 2 1_0" $
        assertEqual " using fix amount in period"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0.08 Nothing  (Just 100 ) (Just (384.62,25.83,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_2_1 1)
      ,testCase "ppy case 2 1" $
        assertEqual " using fix amount out of period"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just 0 ) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_2_1 4)
      ,testCase "ppy case 3" $
        assertEqual " using life time pct"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just 0.21 ) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_3 4)
      ,testCase "ppy case 3 1_0" $
        assertEqual " using pct in period"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0.08 Nothing  (Just 0.25 ) (Just (384.62,25.83,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_3_1 1)
      ,testCase "ppy case 3 1" $
        assertEqual " using pct out of period"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just 0 ) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_3_1 4)
      ,testCase "ppy case 4" $
        assertEqual " using slide at period 0"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0.08 Nothing  (Just 2.58 ) (Just (384.62,25.83,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_4 1)
      ,testCase "ppy case 4 1" $
        assertEqual " using slide at period 1"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just (0.07*21.92)) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_4 4)     
      ,testCase "ppy case 5" $
        assertEqual " using rate 0 before 2 periods"
        (Just (CF.MortgageFlow (L.toDate "20210201")  9589.55 384.62 66.48 25.83 0 0 0 0.08 Nothing  (Just (25.83*0.5) ) (Just (384.62,25.83,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_5 1)
      ,testCase "ppy case 5 1" $
        assertEqual " using rate 1 after 2 periods"
        (Just (CF.MortgageFlow (L.toDate "20210501") 8357.98 389.45 58.31 21.92 0 0 0 0.08 Nothing (Just (0.2*21.92)) (Just (1548.18,93.84,0.00,0.00,0.00,0.00))))
        (CF.cfAt ppy_cf_5 4)   
    ]

delinqScheduleCFTest = 
  let 
    cfs = [CF.MortgageDelinqFlow (L.toDate "20230901") 1000  0 0 0 0 0 0 0 0.08 Nothing Nothing Nothing
          ,CF.MortgageDelinqFlow (L.toDate "20231001") 500 500 0 0 0 0 0 0 0.08 Nothing Nothing Nothing
          ]
    pool = P.Pool ([]::[AB.Mortgage])
                  (Just (CF.CashFlowFrame dummySt cfs,Nothing))
                  Nothing
                  (L.toDate "20230801")
                  Nothing
                  (Just MonthEnd)
    assump1 = Just (A.PoolLevel 
                      (A.MortgageDeqAssump (Just (A.DelinqCDR 0.05 (5,0.3))) Nothing Nothing Nothing
                      ,A.DummyDelinqAssump
                      ,A.DummyDefaultAssump))
    assump2 = Just (A.PoolLevel 
                      (A.MortgageDeqAssump (Just (A.DelinqCDR 0.05 (5,0.3))) (Just (A.PrepaymentCPR 0.08)) Nothing Nothing
                      ,A.DummyDelinqAssump
                      ,A.DummyDefaultAssump))

    poolCf = fst . head $ 
               case P.runPool pool assump1 Nothing of
                 Left errorMsg -> undefined `debug` ("Error in pool run"++show errorMsg)
                 Right x -> x  `debug` ("pool run resp"++show x)
    poolCf2 = fst . head $ 
                case P.runPool pool assump2 Nothing of
                  Left _ -> undefined
                  Right x -> x
  in 
    1
    -- testGroup "delinq run on schedule flow" [
    --   testCase "case 01" $
    --     assertEqual "size of cashflow" 
    --     7
    --     (CF.sizeCashFlowFrame poolCf) -- `debug` ("\n>>>>> Pool cf from test schedule delinq\n >>>>"++ show poolCf)
    --   ,testCase "case 01_Dates" $
    --     assertEqual "Dates of cashflow" 
    --     (L.toDate <$> ["20230901","20231001","20231031","20231130","20231231","20240131","20240229"])
    --     (CF.getDatesCashFlowFrame poolCf)
    --   ,testCase "case 02" $
    --     assertEqual "first row of cf"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20230901") 995.66  0 0 0 4.34 0 0 0 0.08 Nothing Nothing (Just (0.00,0.00,4.34,0.00,0.00,0.00))))
    --     (CF.cfAt poolCf 0)
    --   ,testCase "case 03" $
    --     assertEqual "second row of cf"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20231001") 493.66  497.82 0 0 4.18 0 0 0 0.08 Nothing Nothing (Just (497.82,0.00,8.52,0.00,0.00,0.00))))
    --     (CF.cfAt poolCf 1)
    --   ,testCase "case 04" $
    --     assertEqual "first extended cf, nothing"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20231031") 493.66  0.0 0 0 0 0 0 0 0.00 Nothing Nothing (Just (497.82,0.00,8.52,0.00,0.00,0.00))))
    --     (CF.cfAt poolCf 2)
    --   ,testCase "case 05" $
    --     assertEqual "first default from delinq"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20240131") 499.61  0.0 0 0 0 1.3 0 1.3 0.000952 Nothing Nothing (Just (497.82,0.00,8.52,1.30,0.00,1.30))))
    --     (CF.cfAt poolCf 5)
    --   ,testCase "case 06" $
    --     assertEqual "first loss/recovery from default & first back to perf"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20240229") 496.64  2.97 0 0 0 1.25 0 1.25 0.000952 Nothing Nothing (Just (500.79,0.00,8.52,2.55,0.00,2.55))))
    --     (CF.cfAt poolCf 6)
    --   -- ,testCase "case 07" $
    --   --   assertEqual "first loss/recovery from default & first back to perf"
    --   --   (Just (CF.MortgageFlow (L.toDate "20240229") 492.36  0.0 0 0 0 1.25 0 1.25 0.0 Nothing Nothing))
    --   --   (CF.cfAt poolCf 7)
    --   ,testCase "case with prepay assump" $
    --     assertEqual "01"
    --     (Just (CF.MortgageDelinqFlow (L.toDate "20230901") 988.64 0 0 7.02 4.34  0.0 0.0 0.0 0.08 Nothing Nothing  (Just (0.00,7.02,4.34,0.00,0.00,0.00))))
    --     (CF.cfAt poolCf2 0)
    -- ]

delinqMortgageTest = 
  let 
    tm1 = AB.Mortgage
           (AB.MortgageOriginalInfo 12 (Fix DC_ACT_365F 0.08) 12 L.Monthly (L.toDate "20210101") AB.Level Nothing Nothing)
           240 0.08 3
           Nothing
           AB.Current
    assump1 = (A.MortgageDeqAssump   
                        (Just (A.DelinqCDR 0.05 (2,0.3)))
                        -- (Just (A.PrepaymentCPR 0.08))
                        Nothing
                        Nothing 
                        Nothing
              ,A.DummyDelinqAssump,A.DummyDefaultAssump)
    (CF.CashFlowFrame _ txns,m) = case Ast.projCashflow tm1 (L.toDate "20200101") assump1 Nothing of
                                    Left _ -> undefined
                                    Right x -> x

  in 
    testGroup "Mortgage Delinq Projection" [
      testCase "" $
        assertEqual "Length of cf"
        5
        (length txns) -- `debug` ("Delinq CF"++show txns)
      ,testCase "first row" $
        assertEqual "delinq = 1"
        (CF.MortgageDelinqFlow (L.toDate "20211101") 159.84 79.12 1.59 0 1.04 0.0 0.0 0.0 0.08 Nothing Nothing (Just (79.12,0.00,1.04,0.00,0.00,0.00)))
        (txns!!0)
      ,testCase "second row" $
        assertEqual "with first default/loss/recovery"
        (CF.MortgageDelinqFlow (L.toDate "20211201") 79.85 79.32 1.06  0 0.67 0.0 0.0 0.0 0.08 Nothing Nothing (Just (158.44,0.00,1.71,0.00,0.00,0.00)))
        (txns!!1)
      ,testCase "last row" $
        assertEqual "with first default/loss/recovery"
        (CF.MortgageDelinqFlow (L.toDate "20220101") 1.17 79.75 0.53  0 0.34 0.31 0.0 0.31 0.08 Nothing Nothing (Just (238.19,0.00,2.05,0.31,0.00,0.31)))
        (txns!!2)
      ,testCase "extend 1st flow" $
        assertEqual "check default"
        (CF.MortgageDelinqFlow (L.toDate "20220201") 0.70 0.47 0.0  0.0 0.0 0.20 0.0 0.2 0.08 Nothing Nothing (Just (238.66,0.00,2.05,0.51,0.00,0.51)))
        (txns!!3)
      -- ,testCase "extend 2st flow" $
      --   assertEqual "check default"
      --   (CF.MortgageDelinqFlow (L.toDate "20220201") 1.08 0.36 0.0  0.0 0.0 0.11 0.0 0.11 0.08 Nothing Nothing)
      --   (txns!!4)
      -- ,testCase "extend 3st flow" $
      --   assertEqual "check default"
      --   (CF.MortgageDelinqFlow (L.toDate "20220201") 1.08 0.36 0.0  0.0 0.0 0.0 0.0 0.0 0.08 Nothing Nothing)
      --   (txns!!5)
    ]

btlMortgageTest = 
  let 
    btl = AB.Mortgage
            (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") AB.I_P Nothing Nothing)
            240 0.08 2
            Nothing
            AB.Current
    assump1 = (A.MortgageAssump   
                        Nothing
                        -- (Just (A.PrepaymentCPR 0.08))
                        Nothing
                        Nothing 
                        Nothing
              ,A.DummyDelinqAssump,A.DummyDefaultAssump)            
    (CF.CashFlowFrame _ txns,m) = case Ast.projCashflow btl (L.toDate "20200101") assump1 Nothing of 
                                    Left _ -> undefined
                                    Right x -> x
  in 
    testGroup "Buy to let Mortgage Projection" [
      testCase "" $
        assertEqual "Length of cf"
        3
        (length txns)
      ,testCase "extend 1st flow" $
        assertEqual "1st row"
        (CF.MortgageFlow (L.toDate "20221201") 240 0 1.59  0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (0.0,0.00,0.0,0.0,0.00,0.0)))
        (txns!!1)
      ,testCase "extend 1st flow" $
        assertEqual "last row"
        (CF.MortgageFlow (L.toDate "20230101") 0 240 1.59  0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (240,0.00,0.0,0.0,0.00,0.0)))
        (txns!!2)
    ]

nonPayMortgageTest = 
  let 
    m = AB.Mortgage
          (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") (AB.NO_FirstN 3 AB.Level) Nothing Nothing)
          240 0.08 24
          Nothing
          AB.Current
    assump1 = (A.MortgageAssump   
                        Nothing
                        Nothing
                        Nothing 
                        Nothing
              ,A.DummyDelinqAssump,A.DummyDefaultAssump)
    (CF.CashFlowFrame _ txns,_) = case Ast.projCashflow m (L.toDate "20200101") assump1 Nothing of 
                                    Left _ -> undefined
                                    Right x -> x
    m1 = AB.Mortgage
          (AB.MortgageOriginalInfo 240 (Fix DC_ACT_365F 0.08) 24 L.Monthly (L.toDate "20210101") (AB.IO_FirstN 3 AB.Level) Nothing Nothing)
          240 0.08 24
          Nothing
          AB.Current
    (CF.CashFlowFrame _ txns2,_) = case Ast.projCashflow m1 (L.toDate "20200101") assump1 Nothing of 
                                     Left _ -> undefined
                                     Right x -> x
 
  in 
    testGroup "Non Payment Mortgage Projection" [
      testCase "NonPay" $
        assertEqual "Length of cf"
        25
        (length txns)
      ,testCase "first accured" $
        assertEqual "1st row"
        (CF.MortgageFlow (L.toDate "20210201") 241.59 (-1.59) 0 0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (-1.59,0.00,0.0,0.0,0.00,0.0)))
        (txns!!1)
      ,testCase "first amort" $
        assertEqual "4st row"
        (CF.MortgageFlow (L.toDate "20210501") 233.92 10.9 1.63 0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (6.08,0.00,0.0,0.0,0.00,0.0)))
        (txns!!4)
      ,testCase "IO" $
        assertEqual "Length of cf"
        25
        (length txns)
      ,testCase "first accured" $
        assertEqual "1st row"
        (CF.MortgageFlow (L.toDate "20210201") 240 0.0 1.59 0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (0,0.0,0.0,0.0,0.00,0.0)))
        (txns2!!1)
      ,testCase "first amort" $
        assertEqual "4st row"
        (CF.MortgageFlow (L.toDate "20210501") 229.31 10.69 1.59 0.0 0.0 0.0 0.0 0.08 Nothing Nothing (Just (10.69,0.00,0.0,0.0,0.00,0.0)))
        (txns2!!4)
      
    ]

receivableTest = 
  let 
    invoice1 = AB.Invoice (AB.ReceivableInfo (L.toDate "20240401") 1500 1000 (L.toDate "20240601") Nothing Nothing) AB.Current
    invoice2 = AB.Invoice (AB.ReceivableInfo (L.toDate "20240401") 1500 1000 (L.toDate "20240601") (Just (AB.FixedFee 50)) Nothing ) AB.Current
    invoice0 = AB.Invoice (AB.ReceivableInfo (L.toDate "20240401") 1500 1000 (L.toDate "20240601") Nothing Nothing) (AB.Defaulted Nothing)
    invoiceAssump = (A.ReceivableAssump   
                        Nothing
                        Nothing 
                        Nothing
                    ,A.DummyDelinqAssump,A.DummyDefaultAssump)
  in 
    testGroup "Invoice CF test" [
      testCase "Plain Receivable" $
        assertEqual "Last Payment"
        (Right (Just (CF.ReceivableFlow (L.toDate "20240601") 0 0 1500 0 0 0 0 (Just (0.0,0.0,0.0,0.0,0.0,0.0)))))
        ((`CF.cfAt` 1) <$>  (fst <$> Ast.projCashflow invoice1 (L.toDate "20240101") invoiceAssump Nothing) )
      ,testCase "Fix Fee" $
        assertEqual "Last Payment"
        (Right (Just (CF.ReceivableFlow (L.toDate "20240601") 0 0 1450 50 0 0 0 (Just (0.0,0.0,0.0,0.0,0.0,0.0)))))
        ((`CF.cfAt` 1) <$> (fst <$> Ast.projCashflow invoice2 (L.toDate "20240101") invoiceAssump Nothing))
      ,testCase "Defaulted invoice" $
        assertEqual "Defauted invoice "
        (Right (Just (CF.ReceivableFlow (L.toDate "20240501") 0 0 0 0 1500 0 1500.0 (Just (0.0,0.0,0.0,1500.0,0.0,1500.0)))))
        ((`CF.cfAt` 0) <$> (fst <$> Ast.projCashflow invoice0 (L.toDate "20240501") invoiceAssump Nothing))
    ]
  
fixedAssetTest = 
  let 
    assetInfo = AB.FixedAssetInfo (L.toDate "20250101") 11000 1000 10 Monthly AB.StraightLine (AB.FixedCapacity 100)
    assetInfo2 = AB.FixedAssetInfo (L.toDate "20250101") 10000 1000 10 Monthly AB.DecliningBalance (AB.FixedCapacity 100)
    asset = AB.FixedAsset assetInfo 11000 10
    priceCurve = L.mkTs [(L.toDate "20250101",50), (L.toDate "20251231", 150)]
    utilCurve = L.mkTs [(L.toDate "20250101",1.0), (L.toDate "20251231", 1.0)]
  in 
    testGroup "fixed Asset Test" [
      testCase "StraightLine:init Asset: size" $ 
        assertEqual "StraightLine:init Asset: size"
        (Right 10)
        (let 
            asset1 = asset
          in 
            (CF.sizeCashFlowFrame <$> (fst <$> (Ast.projCashflow asset1 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve  Nothing) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "StraightLine:init Asset with ext " $ 
        assertEqual "StraightLine:init Asset"
        (Right (Just (CF.FixedFlow (L.toDate "20260201") 1000 0 10000 100.0 15000.0)))
        (let 
            asset1 = asset
          in 
            ((`CF.cfAt` 12) <$> (fst <$> (Ast.projCashflow asset1 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve (Just 3)) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "StraightLine:init Asset with diff cur balance " $ 
        assertEqual "StraightLine:init Asset"
        (Right (Just (CF.FixedFlow (L.toDate "20250701") 3400 600.0 7600 100.0 5000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo 4000 5
          in 
            ((`CF.cfAt` 0) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve Nothing) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "StraightLine:init Asset with diff cur balance " $ 
        assertEqual "StraightLine:init Asset"
        (Right (Just (CF.FixedFlow (L.toDate "20260201") 1000 0 10000 100.0 15000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo 4000 5
          in 
            ((`CF.cfAt` 7) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve (Just 3)) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "StraightLine:init Asset with diff cur balance " $ 
        assertEqual "StraightLine:init Asset"
        (Right (Just (CF.FixedFlow (L.toDate "20251101") 1000 3000 10000 100.0 5000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo 4000 1
          in 
            ((`CF.cfAt` 0) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve (Just 3)) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "Double Decline:size" $ 
        assertEqual "Double Decline:size "
        (Right 10)
        (let 
            asset2 = AB.FixedAsset assetInfo2 10000 10
          in 
            (CF.sizeCashFlowFrame <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve Nothing) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "Double Decline:first row with full cur bal" $ 
        assertEqual "Double Decline:init Asset"
        (Right (Just (CF.FixedFlow (L.toDate "20250201") 8000 2000 2000 100.0 5000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo2 10000 10
          in 
            ((`CF.cfAt` 0) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve (Just 3)) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "Double Decline:init Asset :last" $ 
        assertEqual "Double Decline:init Asset :last"
        (Right (Just (CF.FixedFlow (L.toDate "20251101") 1000.0 338.86 9000.0 100.0 5000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo2 10000 10
          in 
            ((`CF.cfAt` 9) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve Nothing) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
      ,testCase "Double Decline:init Asset: with ext periods" $ 
        assertEqual "Double Decline:init Asset: with ext periods"
        (Right (Just (CF.FixedFlow (L.toDate "20260201") 1000.00 0.0 9000 100.0 15000.0)))
        (let 
            asset2 = AB.FixedAsset assetInfo2 10000 10
          in 
            ((`CF.cfAt` 12) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
                                  ((A.FixedAssetAssump utilCurve priceCurve (Just 3)) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))

      -- ,testCase "Double Decline" $ 
      --   assertEqual "Double Decline:init Asset : current with less balance "
      --   (Right (Just (CF.FixedFlow (L.toDate "20251101") 1073.73 268.44 8926.27 100.0 5000.0)))
      --   (let 
      --       asset2 = AB.FixedAsset assetInfo2 5000 5
      --     in 
      --       ((`CF.cfAt` 9) <$> (fst <$> (Ast.projCashflow asset2 (L.toDate "20240101") 
      --                             ((A.FixedAssetAssump utilCurve priceCurve Nothing) ,A.DummyDelinqAssump ,A.DummyDefaultAssump) Nothing))))
    ]
