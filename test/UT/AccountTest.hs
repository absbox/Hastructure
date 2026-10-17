module UT.AccountTest(intTests,reserveAccTest,investTests)
where

import Test.Tasty
import Test.Tasty.HUnit
import Accounts 
import Lib
import Stmt
import Util
import DateUtil
import Types
import Deal
import Deal.DealQuery (queryCompound)
import Deal.DealBase
import qualified Cashflow as CF

import qualified Pool as P
import Control.Lens hiding (element,Empty)
import Control.Lens.TH
import Data.Map.Lens


import qualified Data.Time as T
import qualified Data.DList as DL 
import qualified Data.Map as Map
import UT.DealTest (td2)

dummySt = (0,Lib.toDate "19000101",Nothing)


intTests =
  let 
    acc1 = Account 200 "A1" (Just (BankAccount 0.03 QuarterEnd (toDate "20221001"))) Nothing Nothing
    acc2 = Account 150 "A1" (Just (BankAccount 0.03 MonthEnd (toDate "20220301"))) Nothing 
          (Just (Statement (DL.fromList [ AccTxn (toDate "20220715") 120 10 Empty
                          ,AccTxn (toDate "20220915") 150 30 Empty ])))
  in 
    testGroup "Interest on Bank Account Test"
      [
        testCase "Build EarnIntAction" $
          assertEqual "QuarterEnd" 
            [("A1",(genSerialDates QuarterEnd Inc (toDate "20221001") 5))] $ 
            buildEarnIntAction [acc1] (toDate "20231231") []
        ,testCase "Build EarnIntAction Same Year" $
          assertEqual "QuarterEnd Same Year" 
            [("A1",(genSerialDates QuarterEnd Inc (toDate "20221001") 1))] $ 
            buildEarnIntAction [acc1] (toDate "20221231") []
        ,testCase "Validate Interest Calculation 1" $
          assertEqual "MonthEnd with No txn"
          200.5
          (accBalance (depositInt (toDate "20221101") acc1 ))
        ,testCase "Validate Interest Calculation 2" $
          assertEqual "MonthEnd with txns"
          152.40
          (accBalance (depositInt (toDate "20221101") acc2 ))
      ]

investTests =
  let 
    rc = mkTs [(toDate "20211201",0.03),(toDate "20221201",0.03)]
    acc1 = Account 2000 "A1" (Just (InvestmentAccount SOFR1Y 0.015 QuarterEnd QuarterEnd (toDate "20221001") 0.04)) Nothing Nothing
    acc2 = Account 150 "A1" (Just (InvestmentAccount SOFR1Y 0.01 QuarterEnd QuarterEnd (toDate "20220301") 0.03)) Nothing 
          (Just (Statement (DL.fromList [ AccTxn (toDate "20220715") 120 10 Empty
                            ,AccTxn (toDate "20220915") 150 30 Empty ])))
  in 
    testGroup "Interest on Invest Account Test"
      [
        testCase "Validate Interest Calculation 1" $
          assertEqual "MonthEnd with No txn"
          2006.66
          (accBalance (depositInt (toDate "20221101") acc1))
        ,testCase "Validate Interest Calculation 2" $
          assertEqual "MonthEnd with txns"
          152.40
          (accBalance (depositInt (toDate "20221101") acc2 ))
      ]


reserveAccTest = 
  let 
    acc1 = Account 200 "A1" Nothing (Just (PctReserve (CurrentPoolBalance Nothing) 0.01)) Nothing
    acc2 = Account 150 "A2" Nothing (Just (FixReserve 210)) Nothing
    accMap = Map.fromList [("A1",acc1),("A2",acc2)]
    ctx = defaultRunContext
    testCFs = CF.CashFlowFrame dummySt
                [CF.MortgageFlow (toDate "20220601") 150 20 10 0 0 0 0 0 Nothing Nothing Nothing
                ,CF.MortgageFlow (toDate "20220701") 130 20 10 0 0 0 0 0 Nothing Nothing Nothing
                ,CF.MortgageFlow (toDate "20220801") 110 20 10 0 0 0 0 0 Nothing Nothing Nothing
                ,CF.MortgageFlow (toDate "20220901") 90 20 10 0 0 0 0 0 Nothing Nothing Nothing
                ,CF.MortgageFlow (toDate "20221001") 70 20 10 0 0 0 0 0 Nothing Nothing Nothing]
    ttd = set (dealPool . poolTypePool . (ix PoolConsol) . P.poolFutureCf) (Just (testCFs, Nothing)) td2 {accounts = accMap}
  in 
    testGroup "Test On Reserve Acc"
      [
        testCase "Test on Pct Reserve" $
          assertEqual "shall be " 
            (Right 0.7)
            (calcTargetAmount ttd ctx (toDate "20220826") acc1)
      ,testCase "Test on fix Reserve" $
          assertEqual "shall be " 
            (Right 210)
            (calcTargetAmount ttd ctx  (toDate "20220801") acc2)
      ,testCase "test on reserve account gap" $
          assertEqual "pct reserve gap "
            (Right 0)
            (queryCompound ttd ctx (toDate "20220826") (ReserveGapAt (toDate "20220826") ["A1"]))
      ,testCase "test on reserve account gap" $
          assertEqual "fix reserve gap "
            (Right 60)
            (queryCompound ttd ctx (toDate "20220801") (ReserveGapAt (toDate "20220801") ["A2"]))
      ]


