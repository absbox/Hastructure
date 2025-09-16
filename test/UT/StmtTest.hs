module UT.StmtTest(txnTest)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import Lib
import Util
import Stmt
import Data.Ratio
import Types

txnTest = 
    testGroup "Txn test" 
    [
       testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne " 
           7.44 $
           weightAvgBalance 
             (toDate "20221101") 
             (toDate "20221201")
             [(AccTxn (toDate "20221115") 100 20 Empty)]
      ,testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne "
           7.26 $
           weightAvgBalance 
             (toDate "20221101") 
             (toDate "20221201")
             [AccTxn (toDate "20221115") 100 20 Empty
             ,AccTxn (toDate "20221125") 90 (negate 10) Empty
             ]
      ,testCase "Weight Average Balance " $ 
           assertEqual "Weight Average Balacne by dates"
           [ 7.26, 5.64 ] $
           weightAvgBalanceByDates 
             [toDate "20221101",toDate "20221201",toDate "20221225"]
             [AccTxn (toDate "20221115") 100 20 Empty
             ,AccTxn (toDate "20221125") 90 (negate 10) Empty
             ,AccTxn (toDate "20221215") 80 (negate 10) Empty]
      , let
          testTxns = [AccTxn (toDate "20221115") 100 20 Empty
                      ,AccTxn (toDate "20221125") 90 (negate 10) Empty
                      ,AccTxn (toDate "20221215") 80 (negate 10) Empty]
        in
        testCase "Get Txn As Of" $ 
           assertEqual "Get Txn Asof 1"
            Nothing $
            getTxnAsOf testTxns (toDate "20221101")
      , let
          testTxns = [AccTxn (toDate "20221115") 100 20 Empty
                      ,AccTxn (toDate "20221125") 90 (negate 10) Empty
                      ,AccTxn (toDate "20221215") 80 (negate 10) Empty]
        in
        testCase "Get Txn As Of" $
           assertEqual "Get Txn Asof 2"
            [(Just (AccTxn (toDate "20221115") 100 20 Empty)) 
            , (Just (AccTxn (toDate "20221215") 80 (negate 10) Empty))
            , (Just (AccTxn (toDate "20221115") 100 20 Empty))
            ] $
            [(getTxnAsOf testTxns (toDate "20221115"))
             ,(getTxnAsOf testTxns (toDate "20221216"))
             ,(getTxnAsOf testTxns (toDate "20221120"))
            ]
      , let 
          testTxns = [AccTxn (toDate "20221115") 100 20 Empty
                      ,AccTxn (toDate "20221125") 90 (negate 10) Empty
                      ,AccTxn (toDate "20221125") 80 (negate 10) Empty
                      ,AccTxn (toDate "20221215") 80 (negate 10) Empty]
        in 
          testCase "Get Txn As Of" $
            assertEqual "Get Txn Asof on duplicate date" 
            (Just (AccTxn (toDate "20221125") 80 (negate 10) Empty))
            (getTxnAsOf testTxns (toDate "20221201"))

      , let 
          testTxns = [AccTxn (toDate "20221115") 100 20 Empty
                      ,AccTxn (toDate "20221125") 90 (negate 10) Empty
                      ,AccTxn (toDate "20221215") 80 (negate 10) Empty]
        in 
          testCase "Test View balance as of " $
            assertEqual "View balance as of 1"
            [80,100,100,80] $
            [viewBalanceAsOf (toDate "20221114") testTxns,
            viewBalanceAsOf (toDate "20221115") testTxns,
            viewBalanceAsOf (toDate "20221116") testTxns,
            viewBalanceAsOf (toDate "20221225") testTxns]

      ,testCase "weight Average Balance 0 ' " $ 
            assertEqual "Weight Average Balacne '"
            0.27 $
            weightAvgBalance' (toDate "20221115") (toDate "20221116") 
              [BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty ]
      ,testCase "weight Average Balance 1" $ 
            assertEqual "Weight Average Balacne '"
            8.21 $
            weightAvgBalance' (toDate "20221115") (toDate "20221215") 
              [BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty
              ,BondTxn (toDate "20221215") 50 50 10 0.02 30 0 0 Nothing Empty]
      ,testCase "weight Average Balance 2" $ 
            assertEqual "Weight Average Balacne '"
            14.74 $
            weightAvgBalance' (toDate "20221101") (toDate "20230101") 
              [BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty
              ,BondTxn (toDate "20221215") 50 50 10 0.02 30 0 0 Nothing Empty]
      ,testCase "weight Average Balance 3" $ 
            assertEqual "Weight Average Balacne '"
            12.03 $
            weightAvgBalance' (toDate "20221110") (toDate "20230101")
              [(BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty)
              ,(BondTxn (toDate "20221215") 50 50 10 0.02 30 0 0 Nothing Empty)]
      ,testCase "weight Average Balance 4" $ 
            assertEqual "Weight Average Balacne '"
            8.86 $
            weightAvgBalance'  (toDate "20220101") (toDate "20220201")
              [(BondTxn (toDate "20220115") 100 20 10 0.02 30 0 0 Nothing Empty) ]
      ,testCase " view balance test" $ 
            assertEqual "View balance test(bond) 1 "
            110 $
            viewBalanceAsOf (toDate "20221114") 
                            [BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty]
      ,testCase " view balance test" $ 
            assertEqual "View balance test(bond) 2"
            100 $
            viewBalanceAsOf (toDate "20221116") 
                            [BondTxn (toDate "20221115") 100 20 10 0.02 30 0 0 Nothing Empty]
      ,testCase " view balance test" $ 
            assertEqual "View balance test(supportTxn)"
            20 $
            viewBalanceAsOf (toDate "20221201") 
                            [SupportTxn (toDate "20221115") (ByAvailAmount 100) 20 0 0 0 Empty
                            ,SupportTxn (toDate "20221215") (ByAvailAmount 20) 30 0 0 0 Empty
                            ]
      ,testCase " view balance test" $ 
            assertEqual "View balance test( same day txns)"
            30 $
            viewBalanceAsOf (toDate "20221201") 
                            [SupportTxn (toDate "20220915") (ByAvailAmount 100) 20 0 0 0 Empty
                            ,SupportTxn (toDate "20221015") (ByAvailAmount 90) 10 0 0 0 Empty
                            ,SupportTxn (toDate "20221015") (ByAvailAmount 80) 30 0 0 0 Empty
                            ,SupportTxn (toDate "20221215") (ByAvailAmount 70) 30 0 0 0 Empty
                            ]

    ]

