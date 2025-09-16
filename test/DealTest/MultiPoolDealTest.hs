module DealTest.MultiPoolDealTest(baseCase,mPoolbaseTests)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import qualified Accounts as A
import qualified Stmt as S
import qualified Asset as Ast 
import qualified Pool as P
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
import qualified Deal as DR
import qualified Liability as L
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified Call as C
import InterestRate
import qualified CreditEnhancement as CE
import qualified Triggers as Trg
import Lib
import Types
import Deal.DealCollection (CollectionRule(..))

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S

import Debug.Trace
debug = flip trace

dummySt = (0,Lib.toDate "19000101",Nothing)

multiPool = Map.fromList [(PoolName "PoolA",P.Pool {P.assets=[AB.Mortgage
                                                                   AB.MortgageOriginalInfo{ AB.originBalance=4000 ,AB.originRate=Fix DC_ACT_365F 0.085 ,AB.originTerm=60 ,AB.period=Monthly ,AB.startDate=T.fromGregorian 2022 1 1 ,AB.prinType= AB.Level ,AB.prepaymentPenalty = Nothing}
                                                                   1000 0.085 60 Nothing AB.Current]
                                                      ,P.futureCf= Nothing
                                                      ,P.asOfDate = T.fromGregorian 2022 1 1
                                                      ,P.issuanceStat = Just $ Map.fromList [(IssuanceBalance,1000)]
                                                      ,P.extendPeriods = Nothing
                                                      })
                         ,(PoolName "PoolB",(P.Pool {P.assets=[AB.Mortgage
                                                                   AB.MortgageOriginalInfo{ AB.originBalance=4000 ,AB.originRate=Fix DC_ACT_365F 0.085 ,AB.originTerm=60 ,AB.period=Monthly ,AB.startDate=T.fromGregorian 2022 1 1 ,AB.prinType= AB.Level ,AB.prepaymentPenalty = Nothing}
                                                                   3000 0.085 60 Nothing AB.Current]
                                                        ,P.futureCf=Nothing
                                                        ,P.asOfDate = T.fromGregorian 2022 1 1
                                                        ,P.issuanceStat = Just $ Map.fromList [(IssuanceBalance,3000)]
                                                        ,P.extendPeriods = Nothing}))]


baseCase = D.TestDeal {
  D.name = "base case"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
  ,D.accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing }))])
  ,D.fees = Map.empty
  ,D.bonds = (Map.fromList [("A"
                             ,L.Bond{
                              L.bndName="A"
                             ,L.bndType=L.Sequential
                             ,L.bndOriginInfo= L.OriginalInfo{
                                                L.originBalance=3000
                                                ,L.originDate= (T.fromGregorian 2022 1 1)
                                                ,L.originRate= 0.08
                                                ,L.maturityDate = Nothing}
                             ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                             ,L.bndBalance=3000
                             ,L.bndRate=0.08
                             ,L.bndDuePrin=0.0
                             ,L.bndDueInt=0.0
                             ,L.bndStepUp = Nothing
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndDueIntOverInt = 0
                             ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = D.MultiPool multiPool 
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [Collect (Just [PoolName "PoolA",PoolName "PoolB"]) CollectedInterest "General"
               ,Collect (Just [PoolName "PoolA",PoolName "PoolB"]) CollectedPrincipal "General"]
 ,D.liqProvider = Nothing
 ,D.rateCap = Nothing
 ,D.triggers = Nothing
 ,D.ledgers = Nothing
 ,D.stats = (Map.empty,Map.empty,Map.empty,Map.empty)
}

mPoolbaseTests = 
  let 
    inspectVars = [AP.InspectRpt MonthEnd [FutureCurrentPoolBalance Nothing
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolA"])
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolB"])
                                          ,FutureCurrentPoolBalance (Just [PoolName "PoolB",PoolName "PoolA"])]
                  ]
    nonRunAssump = AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing (Just inspectVars) Nothing Nothing Nothing Nothing Nothing Nothing
    (dealAfterRun,poolCf,rcs,_,_) = case DR.runDeal baseCase S.empty Nothing nonRunAssump of 
                                          Right x -> x
                                          Left y -> error ("Error in running deal"++ show y)
    inspects = [ rc | rc@(InspectBal {}) <- rcs ] 
  in 
   testGroup "Multi Pool Deal Test" 
   [testCase "pool current balance (run time)" $
     assertEqual "pool current balance (run time)"
      (InspectBal (toDate "20220101") (FutureCurrentPoolBalance Nothing) 4000)
      (inspects!!0)
     ,testCase "pool current balance (run time)" $
     assertEqual "pool current balance (run time)"
      (InspectBal (toDate "20220101") (FutureCurrentPoolBalance (Just [PoolName "PoolA"])) 1000)
      (inspects!!1)
     ,testCase "pool current balance (run time)" $
     assertEqual "pool current balance (run time)"
      (InspectBal (toDate "20220101") (FutureCurrentPoolBalance (Just [PoolName "PoolB"])) 3000)
      (inspects!!2)
     ,testCase "pool current balance (run time)" $
     assertEqual "pool current balance (run time)"
      (InspectBal (toDate "20220101") (FutureCurrentPoolBalance (Just [PoolName "PoolB",PoolName "PoolA"])) 4000)
      (inspects!!3)
   ]
