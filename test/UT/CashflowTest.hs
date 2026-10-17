module UT.CashflowTest(cfTests,tsSplitTests,testMergePoolCf,combineTest,testHaircut
                      ,testMergeTsRowsFromTwoEntities,testCumStat,testClawIntTest,testPoolAggTest
                      ,testMflowAccessors,testCollectedCfSafety,testEmptyInputSafety)
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Time as T
import qualified Lib as L
import qualified Asset as P
import qualified Pool
import qualified Data.Map as Map
import qualified Assumptions as A
import qualified Cashflow as CF
import Types
import Util
import DateUtil
import Data.List.NonEmpty (NonEmpty(..))

import Debug.Trace
debug = flip trace

dummySt = (0,L.toDate "19000101",Nothing)

trs = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
      , CF.MortgageFlow (L.toDate "20220201") 90 10 10 0 0 0 0 0 Nothing Nothing Nothing
      , CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0 0 0 Nothing Nothing Nothing
      , CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0 0 0 Nothing Nothing Nothing]

cf = CF.CashFlowFrame dummySt trs

aggTs1 = CF.aggTsByDates trs [L.toDate "20220110"]
aggTs2 = CF.aggTsByDates trs [L.toDate "20220210"]
aggTs3 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220208"]
aggTs4 = CF.aggTsByDates trs [L.toDate "20220101",L.toDate "20220218"]

findLatestCf1 = CF.getTxnLatestAsOf cf (L.toDate "20220215")
findLatestCf2 = CF.getTxnLatestAsOf cf (L.toDate "20220315")
findLatestCf3 = CF.getTxnLatestAsOf cf (L.toDate "20210315")

cfTests = testGroup "Cashflow Utils"
  [
    testCase "Cashflow Aggregation only one return" $
     assertEqual "only one ts" 1 (length aggTs1)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 100"
       (CF.MortgageFlow (L.toDate "20220110") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing)
       (head aggTs1)
   ,testCase "Cashflow Aggregation Sum up" $
     assertEqual "Test Sum up" 1 (length aggTs2)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 90"
       (CF.MortgageFlow (L.toDate "20220210") 90 20 20 0 0 0 0 0 Nothing Nothing Nothing)
       (head aggTs2)

   ,testCase "Cashflow Aggregation with two dates" $
     assertEqual "Test Sum up" 2 (length aggTs3)
   ,testCase "Cashflow Aggregation agg correct amount" $
     assertEqual "which bal is 90"
        [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
        ,CF.MortgageFlow (L.toDate "20220208") 90 10 10 0 0 0 0 0  Nothing Nothing Nothing]
        aggTs3

   ,testCase "Cashflow Aggregation with two flows at second cutoff" $
     assertEqual "include two cf in one cutoff date"
       [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
       ,CF.MortgageFlow (L.toDate "20220218") 80 20 20 0 0 0 0 0  Nothing Nothing Nothing]
       aggTs4
   ,testCase "Cashflow Aggregation" $
     assertEqual "aggregate period with no cf"
       [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
       ,CF.MortgageFlow (L.toDate "20220102") 100 0 0 0 0 0 0 0 Nothing Nothing Nothing
       ,CF.MortgageFlow (L.toDate "20220111") 100 0 0 0 0 0 0 0 Nothing Nothing Nothing
       ]
       (CF.aggTsByDates trs (L.toDates ["20220101","20220102","20220111"]))

   ,testCase "Get Latest Cashflow 1" $
     assertEqual "Found one"
       (Just $ CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0 0 0 Nothing Nothing Nothing)
       findLatestCf1
   ,testCase "Get Latest Cashflow 2" $
     assertEqual "Found one"
       (Just (CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0 0 0 Nothing Nothing Nothing))
       findLatestCf2
   ,testCase "Get Latest Cashflow 3" $
     assertEqual "Nothing found"
       Nothing
       findLatestCf3
    ]


tsSplitTests = 
    let 
      cf1 = CF.CashFlow (L.toDate "20230101") 100
      cf2 = CF.CashFlow (L.toDate "20230201") 100
      cf3 = CF.CashFlow (L.toDate "20230301") 100
      cf4 = CF.CashFlow (L.toDate "20230401") 100
      ts1 = [cf1,cf2,cf3,cf4]
      ts2 = [cf1,cf2,cf2,cf3,cf4]
      cff = CF.CashFlowFrame dummySt [cf1,cf2,cf3,cf4]
    in 
      testGroup "Slice Time Series" 
      [ testCase "Cashflow" $
          assertEqual "by middle left"
          ([cf1,cf2],[cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230215") EqToLeft
        ,testCase "Cashflow" $
          assertEqual "on left" 
          ([cf1,cf2,cf3],[cf4]) $
          splitByDate ts1 (L.toDate "20230301") EqToLeft
        ,testCase "Cashflow" $
          assertEqual "on right"
          ([cf1,cf2],[cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230301") EqToRight
        ,testCase "Cashflow" $
          assertEqual "by middle right"
          ([cf1],[cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230110") EqToRight
        ,testCase "Cashflow" $
          assertEqual "Keep previous one"
          ([cf1],[cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230210") EqToLeftKeepOne
        ,testCase "Cashflow" $
          assertEqual "Keep previous one"
          ([],[cf1,cf2, cf3,cf4]) $
          splitByDate ts1 (L.toDate "20230201") EqToLeftKeepOne
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf2, cf3,cf4] $
          sliceBy II (L.toDate "20230201") (L.toDate "20230401") ts1
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf3,cf4] $
          sliceBy EI (L.toDate "20230201") (L.toDate "20230401") ts1
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf2, cf3] $
          sliceBy IE (L.toDate "20230201") (L.toDate "20230401") ts1
        ,testCase "Range of Ts" $
          assertEqual "get subset of Ts between two dates"
          [cf3] $
          sliceBy EE (L.toDate "20230201") (L.toDate "20230401") ts1
      ]

combineTest = 
  let 
    txn1 = CF.MortgageFlow (L.toDate "20230101") 100 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
    txn2 = CF.MortgageFlow (L.toDate "20230201") 90 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
    txn3 = CF.MortgageFlow (L.toDate "20230301") 50 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
    txn4 = CF.MortgageFlow (L.toDate "20230401") 40 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
    cf1 = CF.CashFlowFrame dummySt [txn1,txn2] 
    cf2 = CF.CashFlowFrame dummySt [txn3,txn4] 
  in 
    testGroup "Combine Cashflow Test"
    [ testCase "No overlap combine" $
        assertEqual "No overlap combine"
        (CF.CashFlowFrame dummySt 
          [CF.MortgageFlow (L.toDate "20230101") 160 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          ,CF.MortgageFlow (L.toDate "20230201") 150 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          ,CF.MortgageFlow (L.toDate "20230301") 140 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          ,CF.MortgageFlow (L.toDate "20230401") 130 10 10 0 0 0 0 0.0 Nothing Nothing Nothing])
        (CF.combine cf1 cf2)
      ,testCase "Overlap combine" $
        let 
          txn1 = CF.MortgageFlow (L.toDate "20230101") 100 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          txn2 = CF.MortgageFlow (L.toDate "20230201") 90 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          txn3 = CF.MortgageFlow (L.toDate "20230301") 80 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          cf1 = CF.CashFlowFrame dummySt [txn1,txn2] 
          cf2 = CF.CashFlowFrame dummySt [txn2,txn3] 
        in 
          assertEqual "Overlap combine"
          (CF.CashFlowFrame dummySt $
            [CF.MortgageFlow (L.toDate "20230101") 200 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
            ,CF.MortgageFlow (L.toDate "20230201") 180 20 20 0 0 0 0 0.0 Nothing Nothing Nothing
            ,CF.MortgageFlow (L.toDate "20230301") 170 10 10 0 0 0 0 0.0 Nothing Nothing Nothing])
          (CF.combine cf1 cf2)
       ,testCase "Intersection" $
        let 
          txn1 = CF.MortgageFlow (L.toDate "20230101") 100 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          txn2 = CF.MortgageFlow (L.toDate "20230201") 80 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          txn3 = CF.MortgageFlow (L.toDate "20230301") 90 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          txn4 = CF.MortgageFlow (L.toDate "20230401") 70 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
          cf1 = CF.CashFlowFrame dummySt [txn1,txn3] 
          cf2 = CF.CashFlowFrame dummySt [txn2,txn4] 
        in 
          assertEqual "Intersection CF"
          (CF.CashFlowFrame dummySt $
            [CF.MortgageFlow (L.toDate "20230101") 190 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
            ,CF.MortgageFlow (L.toDate "20230201") 180 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
            ,CF.MortgageFlow (L.toDate "20230301") 170 10 10 0 0 0 0 0.0 Nothing Nothing Nothing
            ,CF.MortgageFlow (L.toDate "20230401") 160 10 10 0 0 0 0 0.0 Nothing Nothing Nothing])
          (CF.combine cf1 cf2)
    ]


testMergeTsRowsFromTwoEntities = 
  let 
    txn1 = CF.MortgageDelinqFlow (L.toDate "20230101") 100 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
    txn4 = CF.MortgageDelinqFlow (L.toDate "20230401") 90 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
    
    txn2 = CF.MortgageDelinqFlow (L.toDate "20230201") 100 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
    txn3 = CF.MortgageDelinqFlow (L.toDate "20230301") 90 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
  in
    testGroup "Merge Two CF from two entities"
    [testCase "txn1 + txn 2" $ 
      assertEqual "Merge Two CF from two entities"
      [CF.MortgageDelinqFlow (L.toDate "20230101") 210 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
       ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing]
      (CF.combineTss [] [txn1] [txn2])
    ,testCase "txn1 + txn 2/3" $ 
      assertEqual "Merge Two CF from two entities"
      [CF.MortgageDelinqFlow (L.toDate "20230101") 210 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing]
      (CF.combineTss [] [txn1] [txn2,txn3])
    ,testCase "txn1/4 + txn 2/3" $ 
      assertEqual "Merge Two CF from two entities"
      [CF.MortgageDelinqFlow (L.toDate "20230101") 210 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230401") 180 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing ]
      (CF.combineTss [] [txn1,txn4] [txn2,txn3])
    ,testCase "txn1/2 + txn 1/2" $
      assertEqual "Merge Two CF from two entities with same dates"
      [CF.MortgageDelinqFlow (L.toDate "20230101") 200 20 20 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230201") 180 20 20 0 0 0 0 0 0.0 Nothing Nothing Nothing]
      (CF.combineTss [] [txn1,txn2] [txn1,txn2])
    ,testCase "txn1/2/3 + txn 1/2" $
      assertEqual "Merge Two CF from two entities with same dates"
      [CF.MortgageDelinqFlow (L.toDate "20230101") 200 20 20 0 0 0 0 0 0.0 Nothing Nothing Nothing 
       ,CF.MortgageDelinqFlow (L.toDate "20230201") 180 20 20 0 0 0 0 0 0.0 Nothing Nothing Nothing
       ,CF.MortgageDelinqFlow (L.toDate "20230301") 170 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
       ]
      (CF.combineTss [] [txn1,txn2,txn3] [txn1,txn2])
      
      
      ]



testMergePoolCf = 
  let 
    txn1 = CF.MortgageDelinqFlow (L.toDate "20230101") 100 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
    txn2 = CF.MortgageDelinqFlow (L.toDate "20230201") 100 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
    txn3 = CF.MortgageDelinqFlow (L.toDate "20230301") 90 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
    txn4 = CF.MortgageDelinqFlow (L.toDate "20230401") 90 10 10 0 0 0 0 0 0.0 Nothing Nothing Nothing
    
    cf1 = CF.CashFlowFrame (110, L.toDate "20221201", Nothing) [txn1,txn4]
    cf2 = CF.CashFlowFrame (110, L.toDate "20230101", Nothing) [txn2,txn3]

    cf3 = CF.CashFlowFrame (110, L.toDate "20221201", Nothing) [txn2,txn4]
    cf4 = CF.CashFlowFrame (110, L.toDate "20221201", Nothing) [txn1,txn3]

    cf5 = CF.CashFlowFrame (100, L.toDate "20221201", Nothing) [txn3,txn4]
    cf6 = CF.CashFlowFrame (110, L.toDate "20230101", Nothing) [txn2]

  in 
    testGroup "Merge Cashflow Test from two entities"  -- merge cashflow into existing one without update previous balance
    [ testCase "Merge Cashflow Test 1" $
        assertEqual ""
        (CF.CashFlowFrame (110 , L.toDate "20221201", Nothing)
                           [(CF.MortgageDelinqFlow (L.toDate "20230101") 210 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (10,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (20,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230301") 190 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (30,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230401") 180 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (40,0,0,0,0,0)))]) 
        (CF.mergePoolCf2 cf1 cf2)
      ,testCase "Merge Cashflow with same begin date 1" $
        assertEqual ""
        (CF.CashFlowFrame (220 , L.toDate "20221201", Nothing)
                           [(CF.MortgageDelinqFlow (L.toDate "20230101") 210 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (10,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (20,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230301") 190 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (30,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230401") 180 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (40,0,0,0,0,0)))]) 
        (CF.mergePoolCf2 cf3 cf4)
      ,testCase "Merge Cashflow with diff begin date 2" $
        assertEqual ""
        (CF.CashFlowFrame (100 , L.toDate "20221201", Nothing)
                           [(CF.MortgageDelinqFlow (L.toDate "20230201") 200 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (10,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230301") 190 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (20,0,0,0,0,0)))
                           ,(CF.MortgageDelinqFlow (L.toDate "20230401") 180 10 10 0 0 0 0 0 0.0 Nothing Nothing (Just (30,0,0,0,0,0)))]) 
        (CF.mergePoolCf2 cf5 cf6)
      ]

testHaircut = 
  let 
    cflow = CF.CashFlowFrame dummySt [(CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 0 0 5  0 0.0 Nothing (Just 10) Nothing)
                             ,(CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 0 0 10 0 0.0 Nothing (Just 15) Nothing)
                             ,(CF.MortgageDelinqFlow (L.toDate "20230301") 190 40 30 40 0 0 15 0 0.0 Nothing (Just 20) Nothing)
                             ,(CF.MortgageDelinqFlow (L.toDate "20230401") 180 50 40 50 0 0 20 0 0.0 Nothing (Just 30) Nothing)]
  in 
    testGroup "Test on Haircut"
    [ testCase "Haircut of Nothing" $
        assertEqual "" 
        cflow 
        (P.applyHaircut Nothing cflow)
    ,testCase "Haircut on principal" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 10 10 20 0 0 5  0 0.0 Nothing (Just 10) Nothing ))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedPrincipal,0.5)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ,testCase "Haircut on interest" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 7 20 0 0 5  0 0.0 Nothing (Just 10) Nothing))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedInterest,0.3)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ,testCase "Haircut on prepayment" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 12 0 0 5  0 0.0 Nothing (Just 10) Nothing))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedPrepayment,0.4)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ,testCase "Haircut on recoveries" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 0 0 4.5  0 0.0 Nothing (Just 10) Nothing))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedRecoveries,0.1)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ,testCase "Haircut on prepay penalty" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 0 0 5  0 0.0 Nothing (Just 8) Nothing))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedPrepaymentPenalty,0.2)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ,testCase "Haircut on mix" $
        assertEqual "" 
        (Just (CF.MortgageDelinqFlow (L.toDate "20230101") 100 10 7 20 0 0 5  0 0.0 Nothing (Just 8) Nothing))
        (CF.cfAt (P.applyHaircut (Just A.ExtraStress{A.poolHairCut = Just [(CollectedPrepaymentPenalty,0.2)
                                                                          ,(CollectedPrincipal,0.5)
                                                                          ,(CollectedInterest,0.3)], A.defaultFactors = Nothing, A.prepaymentFactors = Nothing }) cflow) 0)
    ]

testCumStat = 
  let 
    cflow =[CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 1 2 3 4 0.0 Nothing (Just 10) Nothing
           ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 1 2 3 4 0.0 Nothing (Just 15) Nothing
           ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 40 30 40 1 2 3 4 0.0 Nothing (Just 20) Nothing
           ,CF.MortgageDelinqFlow (L.toDate "20230401") 180 50 40 50 1 2 3 4 0.0 Nothing (Just 30) Nothing]

    cflow1 = [CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 1 2 3 4 0.0 Nothing (Just 15) (Just (30,30,1,2,3,4))
             ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 40 30 40 1 2 3 4 0.0 Nothing (Just 20) (Just (70,70,2,4,6,8))
             ,CF.MortgageDelinqFlow (L.toDate "20230401") 180 50 40 50 1 2 3 4 0.0 Nothing (Just 30) (Just (120,120,3,6,9,12))]
    
    cflow2 =[CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 1 2 3 4 0.0 Nothing (Just 10) Nothing
        ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 1 2 3 4 0.0 Nothing (Just 15) Nothing
        ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 40 30 40 1 2 3 4 0.0 Nothing (Just 20) Nothing
        ,CF.MortgageDelinqFlow (L.toDate "20230401") 180 50 40 50 1 2 3 4 0.0 Nothing (Just 30) Nothing]
  in 
    testGroup "Test on calc CumStat"
    [ testCase "MortDelinq CumStat" $
        assertEqual "" 
        cflow1
        (fst (CF.cutoffTrs (L.toDate "20230201") cflow))
      ,testCase "Sum on pool fields-prin" $
        assertEqual "sum principal"
        120.0
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedPrincipal)
      ,testCase "Sum on pool fields-int" $
        assertEqual "sum interest"
        90.0
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedInterest)
      ,testCase "Sum on pool fields-ppy" $
        assertEqual "sum prepayment"
        120.0
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedPrepayment)
      ,testCase "Sum on pool fields-delinq" $
        assertEqual "sum delinq"
        3.0
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) NewDelinquencies)
      ,testCase "Sum on pool fields-default" $
        assertEqual "sum default"
        6
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) NewDefaults)
      ,testCase "Sum on pool fields-recovery" $
        assertEqual "sum recovery"
        9
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedRecoveries)
      ,testCase "Sum on pool fields-loss" $
        assertEqual "sum loss"
        12
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) NewLosses)
      ,testCase "Sum on pool fields-cash" $
        assertEqual "sum cash"
        404.0
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedCash)
      ,testCase "Sum on pool fields-prepay penalty" $
        assertEqual "sum prepayment penalty"
        65
        (CF.sumPoolFlow (CF.CashFlowFrame dummySt cflow1) CollectedPrepaymentPenalty)
      ,testCase "Patch Cumulative 0" $
        assertEqual "patch cum stats"
        [CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 1 2 3 4 0.0 Nothing (Just 10) (Just (20,20,1,2,3,4))
        ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 1 2 3 4 0.0 Nothing (Just 15) (Just (50,50,2,4,6,8))
        ,CF.MortgageDelinqFlow (L.toDate "20230301") 190 40 30 40 1 2 3 4 0.0 Nothing (Just 20) (Just (90,90,3,6,9,12))
        ,CF.MortgageDelinqFlow (L.toDate "20230401") 180 50 40 50 1 2 3 4 0.0 Nothing (Just 30) (Just (140,140,4,8,12,16))
        ]
        (CF.patchCumulative (0,0,0,0,0,0) [] cflow2)
    ]

testClawIntTest = 
  let 
    cflow =[CF.MortgageDelinqFlow (L.toDate "20230101") 100 20 10 20 1 2 3 4 0.0 Nothing (Just 10) Nothing
           ,CF.MortgageDelinqFlow (L.toDate "20230201") 200 30 20 30 1 2 3 4 0.0 Nothing (Just 15) Nothing ]
  in         
    testGroup "test on interest claw"   
    [
      testCase "claw in first" $
        assertEqual "AA"
        [0,0]
        (CF.mflowInterest <$> CF.clawbackInt 30 cflow)
      ,testCase "claw in second" $
        assertEqual "AA"
        [0,15]
        (CF.mflowInterest <$> CF.clawbackInt 15 cflow)
      ,testCase "claw in all" $
        assertEqual "AA"
        [5,20]
        (CF.mflowInterest <$> CF.clawbackInt 5 cflow)
    ]

testPoolAggTest = 
  let 
    trs = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing (Just (10,0,0,0,0,0))
          , CF.MortgageFlow (L.toDate "20220301") 70 10 10 0 0 0 0 0 Nothing Nothing (Just (20,0,0,0,0,0)) ]
    trs1 = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing (Just (10,0,0,0,0,0))
          , CF.MortgageFlow (L.toDate "20220401") 70 10 10 0 0 0 0 0 Nothing Nothing (Just (20,0,0,0,0,0)) ]
    trs2 = [CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing (Just (10,0,0,0,0,0))
          , CF.MortgageFlow (L.toDate "20220401") 70 10 10 0 10 0 0 0 Nothing Nothing (Just (20,0,0,10,0,0)) ]
          
    cf = CF.CashFlowFrame dummySt trs    
    cf1 = CF.CashFlowFrame dummySt trs1
    cf2 = CF.CashFlowFrame dummySt trs2
    
  in 
    testGroup "test on combine cashflow with stats"   
    [
      testCase "combineCF one extra row" $
        assertEqual "cum stats should patch at last"
        (CF.CashFlowFrame dummySt
          [
            CF.MortgageFlow (L.toDate "20220101") 200 20 20 0 0 0 0 0 Nothing Nothing (Just (20,0,0,0,0,0))
            ,CF.MortgageFlow (L.toDate "20220301") 190 10 10 0 0 0 0 0 Nothing Nothing (Just (30,0,0,0,0,0))
            ,CF.MortgageFlow (L.toDate "20220401") 180 10 10 0 0 0 0 0 Nothing Nothing (Just (40,0,0,0,0,0))
          ]
          )
        (fst (Pool.aggPool Nothing [(cf,Map.empty),(cf1,Map.empty)]))
      ,testCase "pool agg with init default=100" $
        assertEqual "cum stats with default=100,no default on cfs"
        (Map.fromList [(HistoryDefaults,100)])
        (snd (Pool.aggPool (Just (Map.fromList [(HistoryDefaults,100)])) 
                            [(cf,Map.empty),(cf1,Map.empty)]
                            ))
      ,testCase "pool agg with init default=100 and projected cf default" $
        assertEqual "cum stats with default=100, projected default on cfs"
        (Map.fromList [(HistoryDefaults,200)])
        (snd (Pool.aggPool (Just (Map.fromList [(HistoryDefaults,100)])) 
                            [(cf, (Map.fromList [(HistoryDefaults,100)])),(cf1,Map.empty)]
                            ))
          ,testCase "pool agg with init default=100 and projected cf default2" $
            assertEqual "cum stats with default=100, projected default on cfs"
            (Map.fromList [(HistoryDefaults,200)])
            (snd (Pool.aggPool (Just (Map.fromList [(HistoryDefaults,100)])) 
                                [(cf, (Map.fromList [(HistoryDefaults,100)])),(cf2, Map.empty)]
                                ))                            
    ]

testMflowAccessors = 
  let 
    loanRow = CF.LoanFlow (L.toDate "20220101") 100 10 5 0 0 0 0 0.0 Nothing
    leaseRow = CF.LeaseFlow (L.toDate "20220101") 100 5 0
    mtgRow = CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 (Just 3) (Just 5) Nothing
    mtgRowNoB = CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
  in 
    testGroup "mflow accessor totality"
    [ testCase "borrower num on non-mortgage row returns Nothing instead of undefined" $
        assertEqual "loan flow has no borrower field" Nothing (CF.mflowBorrowerNum loanRow)
      ,testCase "borrower num on mortgage row still works" $
        assertEqual "mortgage borrower num" (Just 3) (CF.mflowBorrowerNum mtgRow)
      ,testCase "borrower num on mortgage row without info is Nothing" $
        assertEqual "mortgage borrower num" Nothing (CF.mflowBorrowerNum mtgRowNoB)
      ,testCase "prepayment penalty on non-mortgage row returns 0 instead of undefined" $
        assertEqual "lease flow has no penalty field" 0 (CF.mflowPrepaymentPenalty leaseRow)
      ,testCase "prepayment penalty on mortgage row still works" $
        assertEqual "mortgage penalty" 5 (CF.mflowPrepaymentPenalty mtgRow)
    ]

testCollectedCfSafety = 
  let 
    row1 = CF.MortgageFlow (L.toDate "20220101") 100 10 10 0 0 0 0 0 Nothing Nothing Nothing
    d = L.toDate "20220301"
  in 
    testGroup "buildCollectedCF safety"
    [ testCase "does not crash when last accumulated row is empty" $
        let r = CF.buildCollectedCF [[row1],[]] [d] []
        in assertEqual "last row balance comes from the last non-empty accumulated row" 100
                      (CF.mflowBegBalance (last (concat r)))
      ,testCase "empty trs with dates returns empty rows instead of crashing" $
        assertEqual "two empty rows" [[],[]] (CF.buildCollectedCF [] [L.toDate "20220101",L.toDate "20220201"] [])
    ]

-- | #332: empty-input and CashFlow-row safety for the cashflow aggregation
testEmptyInputSafety = testGroup "Cashflow empty-input safety"
  [
    testCase "firstDate of an empty frame returns its begin date" $
      assertEqual "begin date" (L.toDate "20200101")
        (CF.firstDate (CF.CashFlowFrame (0, L.toDate "20200101", Nothing) []))

   ,testCase "firstDate of emptyCashflow returns epocDate" $
      assertEqual "epocDate" epocDate (CF.firstDate CF.emptyCashflow)

   ,testCase "firstDate of DummyCF returns epocDate" $
      assertEqual "epocDate" epocDate (CF.firstDate CF.DummyCF)

   ,testCase "aggTsByDates over an empty pool returns []" $
      assertEqual "empty" ([] :: [CF.TsRow]) (CF.aggTsByDates [] [L.toDate "20220101"])

   ,testCase "aggTsByDates drops groups with no rows (rows after the cutoffs)" $
      assertEqual "no groups" ([] :: [CF.TsRow])
        (CF.aggTsByDates [CF.MortgageFlow (L.toDate "20220211") 80 10 10 0 0 0 0 0 Nothing Nothing Nothing]
                         [L.toDate "20220101"])

   ,testCase "sumTsCF sums a NonEmpty group" $
      assertEqual "summed" (CF.CashFlow (L.toDate "20220103") 15)
        (CF.sumTsCF (CF.CashFlow (L.toDate "20220101") 10 :| [CF.CashFlow (L.toDate "20220102") 5])
                    (L.toDate "20220103"))

   ,testCase "aggTsByDates handles CashFlow-shaped rows (zero-row synthesis)" $
      assertEqual "cashflow rows"
        [CF.CashFlow (L.toDate "20220101") 100, CF.CashFlow (L.toDate "20220201") 0]
        (CF.aggTsByDates [CF.CashFlow (L.toDate "20220101") 100]
                         [L.toDate "20220101", L.toDate "20220201"])

   ,testCase "extendCashFlow appends a zeroed CashFlow row" $
      assertEqual "extended"
        (CF.CashFlowFrame dummySt [CF.CashFlow (L.toDate "20220101") 100, CF.CashFlow (L.toDate "20220201") 0])
        (CF.extendCashFlow (L.toDate "20220201") (CF.CashFlowFrame dummySt [CF.CashFlow (L.toDate "20220101") 100]))
  ]
