module UT.BondTest(pricingTests,bndConsolTest,writeOffTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Liability as B
import qualified Deal as D
import qualified Lib as L
import qualified Stmt  as S
import qualified Asset as P
import qualified Assumptions as A
import qualified Cashflow as CF
import qualified Data.DList as DL
import Util
import Types
import Data.Ratio

import Debug.Trace
debug = flip trace

b1Txn =  DL.fromList [ BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 0 0 Nothing S.Empty
          ,BondTxn (L.toDate "20220801") 0 10 1500 0.08 1510 0 0 Nothing S.Empty ]
b1 = B.Bond{B.bndName="A"
            ,B.bndType=B.Sequential
            ,B.bndOriginInfo= B.OriginalInfo{
                               B.originBalance=3000
                               ,B.originDate= T.fromGregorian 2021 1 1
                               ,B.originRate= 0.08
                               ,B.maturityDate = Nothing}
            ,B.bndInterestInfo= B.Fix 0.08 DC_ACT_365F
            ,B.bndBalance=3000
            ,B.bndRate=0.08
            ,B.bndDuePrin=0.0
            ,B.bndStepUp = Nothing
            ,B.bndDueInt=0.0
            ,B.bndDueIntOverInt=0.0
            ,B.bndDueIntDate=Nothing
            ,B.bndLastIntPay = Just (T.fromGregorian 2021 1 1)
            ,B.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndStmt=Just (S.Statement b1Txn)}

bfloat = B.Bond{B.bndName="A"
            ,B.bndType=B.Sequential
            ,B.bndOriginInfo= B.OriginalInfo{
                               B.originBalance=3000
                               ,B.originDate= T.fromGregorian 2022 1 1
                               ,B.originRate= 0.08
                               ,B.maturityDate = Nothing}
            ,B.bndInterestInfo= B.Floater 0.02 LPR5Y 0.015 (MonthDayOfYear 1 1) DC_ACT_365F Nothing Nothing
            ,B.bndBalance=3000
            ,B.bndRate=0.08
            ,B.bndStepUp = Nothing
            ,B.bndDuePrin=0.0
            ,B.bndDueInt=0.0
            ,B.bndDueIntDate=Nothing
            ,B.bndDueIntOverInt=0.0
            ,B.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
            ,B.bndStmt=Just $ S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 0 0 Nothing S.Empty])}


pricingTests = testGroup "Pricing Tests"
  [
    let
      _ts = L.PricingCurve [L.TsPoint (L.toDate "20210101") 0.05, L.TsPoint (L.toDate "20240101") 0.05]
      _pv_day = L.toDate "20220201"
      _f_day = L.toDate "20230201"
      _pv = B.pv _ts _pv_day _f_day 103
    in
      testCase "PV test" $
        assertEqual "simple PV with flat curve"  
          98.09
          _pv,
    let
        _pv_day = L.toDate "20220201"
        _f_day = L.toDate "20230201"
        _ts1 = L.PricingCurve [L.TsPoint (L.toDate "20210101") 0.01, L.TsPoint (L.toDate "20230101") 0.03]
        _pv1 = B.pv _ts1 _pv_day _f_day 103
        _diff1 = _pv1 - 100.0
    in
      testCase "PV test with curve change in middle" $
      assertEqual "simple PV with latest rate point" 100.0 _pv1
   ,
    let
      pr = B.priceBond (L.toDate "20210501")
                       (L.PricingCurve [L.TsPoint (L.toDate "20210101") 0.01, L.TsPoint (L.toDate "20230101") 0.02])
                       b1
    in
      testCase "flat rate discount " $
      assertEqual "Test Pricing on case 01" 
        (Right (PriceResult 1978.47 65.949000 1.18 1.1881448 0.4906438 52.60 (DL.toList b1Txn)))
        pr
    ,
     let
       b2Txn =  DL.fromList [BondTxn (L.toDate "20220301") 3000 10 300 0.08 310 0 0 Nothing S.Empty
                           ,BondTxn (L.toDate "20220501") 2700 10 500 0.08 510 0 0 Nothing S.Empty
                           ,BondTxn (L.toDate "20220701") 0 10 3200 0.08 3300 0 0 Nothing S.Empty]
       b2 = b1 { B.bndStmt = Just (S.Statement b2Txn)}

       pr = B.priceBond (L.toDate "20220201")
                        (L.PricingCurve
                            [L.TsPoint (L.toDate "20220101") 0.01
                            ,L.TsPoint (L.toDate "20220401") 0.03
                            ,L.TsPoint (L.toDate "20220601") 0.05])
                        b2
     in
       testCase " discount curve with two rate points " $
       assertEqual "Test Pricing on case 01" 
            (Right (PriceResult 4049.10 134.97 0.44 0.364564 0.006030 286.42 (DL.toList b2Txn)))
            pr  --TODO need to confirm in UI
    ,
    let
      b4 = b1
      pday = L.toDate "20220801"
    in
      testCase "pay prin to a bond" $
      assertEqual "pay down prin" 2400  $ B.bndBalance (B.payPrin pday 600 b4)
    ,
    let
      b5 = b1
      pday = L.toDate "20220801"
    in
      testCase "pay int to 2 bonds" $
      assertEqual "pay int" 2400  $ B.bndBalance (B.payPrin pday 600 b5)
    ,
    let 
      newCfStmt = Just $ S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 1500 300 2800 0.08 3100 0 0 Nothing S.Empty]) 
      b6 = b1 {B.bndStmt = newCfStmt}
      pday = L.toDate "20220301" -- `debug` ("stmt>>>>>"++ show (B.bndStmt b6))
      rateCurve = IRateCurve [TsPoint (L.toDate "20220201") 0.03 ,TsPoint (L.toDate "20220401") 0.04]
      --rateCurve = IRateCurve [TsPoint (L.toDate "20220201") 0.03::IRate]
    in 
      testCase "Z spread test" $
      assertEqual "Z spread test 01" 
      (Right 0.176754)
      (B.calcZspread  (100.0,pday) b6 rateCurve)
      --(B.calcZspread  (500.0,pday) (103.0,1/100) Nothing rateCurve)

  ]

bndTests = testGroup "Float Bond Tests" [
    let
       r1 = B.isAdjustable  (B.bndInterestInfo bfloat)
       r2 = B.isAdjustable (B.bndInterestInfo bfloat)
    in
      testCase "Adjust rate by Month of Year " $
      assertEqual "" [True,False] [r1,r2]
    ,
    let 
       bfloatResetInterval = bfloat {B.bndInterestInfo = B.Floater 
                                                         0.01
                                                         LPR5Y 
                                                         0.015 
                                                         QuarterEnd
                                                         DC_ACT_365F   
                                                         Nothing Nothing}
       r1 = B.isAdjustable $ B.bndInterestInfo bfloatResetInterval
       r2 = B.isAdjustable $ B.bndInterestInfo bfloatResetInterval
    in 
      testCase "Adjust rate by quarter  " $
      assertEqual "" [True,False] [r1,r2]
 ]


bndConsolTest = testGroup "Bond consoliation & patchtesting" [
    let 
      b1f = S.getTxns . B.bndStmt $ B.patchBondFactor b1
    in 
      testCase "test on patching bond factor" $
      assertEqual ""
      (DL.fromList [ BondTxn (L.toDate "20220501") 1500 10 500 0.08 510 0 0 (Just 0.5) S.Empty
       ,BondTxn (L.toDate "20220801") 0 10 1500 0.08 1510 0 0 (Just 0.0) S.Empty
      ])
      b1f,
    let 
      txns = DL.fromList [BondTxn (L.toDate "20220501") 1500 0 (-500) 0.08 0 0 0 (Just 0.5) S.Empty
              ,BondTxn (L.toDate "20220501") 2000 0 (-500) 0.08 0 0 0 (Just 0.0) S.Empty]
      bTest = b1 {B.bndStmt = Just (S.Statement txns)}
      bTestConsol = B.bndStmt $ B.consolStmt bTest
    in
      testCase "merge txn with two drawdowns" $
      assertEqual ""
      (Just (S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 2000 0 (-1000) 0.08 0 0 0 (Just 0.0) (S.TxnComments [S.Empty, S.Empty])])))
      bTestConsol,
    let 
      txns = DL.fromList [ BondTxn (L.toDate "20220501") 1500 0 (-500) 0.08 0 0 0 (Just 0.5) S.Empty
              ,BondTxn (L.toDate "20220501") 1500 0 500 0.08 0 0 0 (Just 0.0) S.Empty]
      bTest = b1 {B.bndStmt = Just (S.Statement txns)}
      bTestConsol = B.bndStmt $ B.consolStmt bTest
    in
      testCase "merge txn with one drawdown at begin" $
      assertEqual ""
      (Just (S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 1500 0 0 0.08 0 0 0 (Just 0.0) (S.TxnComments [S.Empty, S.Empty])])))
      bTestConsol,
    let 
      txns = DL.fromList [BondTxn (L.toDate "20220501") 1500 0 500 0.08 0 0 0 (Just 0.0) S.Empty,
              BondTxn (L.toDate "20220501") 2000 0 (-500) 0.08 0 0 0 (Just 0.5) S.Empty]
      bTest = b1 {B.bndStmt = Just (S.Statement txns)}
      bTestConsol = B.bndStmt $ B.consolStmt bTest
    in
      testCase "merge txn with one drawdown at end" $
      assertEqual ""
      (Just (S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 2000 0 0 0.08 0 0 0 (Just 0.5) (S.TxnComments [S.Empty, S.Empty])])))
      bTestConsol,
    let 
      txns = DL.fromList [BondTxn (L.toDate "20220501") 1500 0 500 0.08 0 0 0 (Just 0.0) S.Empty,
              BondTxn (L.toDate "20220501") 1000 0 500 0.08 0 0 0 (Just 0.5) S.Empty]
      bTest = b1 {B.bndStmt = Just (S.Statement txns)}
      bTestConsol = B.bndStmt $ B.consolStmt bTest
    in
      testCase "merge txn with one drawdown at end" $
      assertEqual ""
      (Just (S.Statement (DL.fromList [ BondTxn (L.toDate "20220501") 1000 0 1000 0.08 0 0 0 (Just 0.5) (S.TxnComments [S.Empty, S.Empty])])))
      bTestConsol
    ]


writeOffTest = 
  let 
    d1 = L.toDate "20200101"
    bnd1 = B.Bond "A" B.Sequential (B.OriginalInfo 100 d1 0.06 Nothing) (B.Fix 0.05 DC_ACT_365F) Nothing 100 0.08 0 0 0 Nothing Nothing Nothing Nothing
    writeAmt1 = 70 
    writeAmt2 = 120 
  in 
  testGroup "write off on bond" [
    testCase "write off on bond 1" $
    assertEqual "only 1st bond is written off by 70"
    (Right (bnd1 {B.bndBalance = 30,B.bndStmt = Just (S.Statement (DL.fromList [S.BondTxn d1 30.00 0.00 0.00 0.000000 0.00 0.00 0.00 Nothing (S.WriteOff "A" 70.00)]))}))
    (B.writeOff d1 writeAmt1 bnd1),
    testCase "over write off on bond 1" $
    assertEqual "over write off on bond 1"
    (Left "Insufficient balance to write off 120.00\" bond name \"\"A\"")
    (B.writeOff d1 writeAmt2 bnd1)
  ]
