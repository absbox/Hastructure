module DealTest.DealTest(baseCase,baseTests,emptyCase)

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
import Numeric.Lens (base)
import qualified Types as P

dummySt = (0,Lib.toDate "19000101",Nothing)

emptyCase = D.TestDeal {
  D.name = "empty case"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
  ,D.accounts = Map.empty
  ,D.fees = Map.empty
  ,D.bonds = Map.empty
  ,D.pool = D.MultiPool $ Map.fromList [(PoolConsol, (P.Pool {P.assets=[]}))] 
  ,D.waterfall = Map.empty
  ,D.collects = []
}

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
                             ,L.bndStepUp = Nothing
                             ,L.bndDueInt=0.0
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing
                             ,L.bndDueIntOverInt = 0})
                         ]
           )
  ,D.pool = D.MultiPool $
              (Map.fromList [(PoolConsol, (P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix DC_ACT_365F 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=T.fromGregorian 2022 1 1
                                           ,AB.prinType= AB.Level
                                           ,AB.prepaymentPenalty = Nothing}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         AB.Current]
                               ,P.futureCf=Nothing
                               ,P.asOfDate = T.fromGregorian 2022 1 1
                               ,P.issuanceStat = Just $ Map.fromList [(IssuanceBalance, 4000)]
                               ,P.extendPeriods = Nothing}))])
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [Collect Nothing W.CollectedInterest "General"
             ,Collect Nothing W.CollectedPrincipal "General"]
 ,D.liqProvider = Nothing
 ,D.rateCap = Nothing
 ,D.triggers = Nothing
 ,D.ledgers = Nothing
 ,D.stats = (Map.empty,Map.empty,Map.empty,Map.empty)
}

baseTests = 
  let 
    nonRunAssump = (AP.NonPerfAssumption Nothing Nothing Nothing Nothing Nothing (Just [AP.InspectPt MonthEnd (FutureCurrentPoolBalance Nothing)]) Nothing Nothing Nothing Nothing Nothing Nothing)
    (dealAfterRun,poolCf,rcs,_,_) = case DR.runDeal baseCase S.empty Nothing nonRunAssump of
                                         Left e -> error $ "Deal run failed"++ show e
                                         Right x -> x
    inspects = [ rc | rc@(InspectBal {}) <- rcs ] 
  in 
   testGroup "Base Deal Test" 
   [ testCase "empty pool flow" $
     assertEqual "empty pool flow"
     0
     -- (P.futureCf (D.pool baseCase))
     0
     -- https://docs.google.com/spreadsheets/d/1gmz8LOB01qqfPldquyDn43PJJ1MI016tS-JS5KW3SvM/edit?gid=1325808922#gid=1325808922
     ,testCase "pool current balance (run time)" $
     assertEqual "pool current balance (run time)"
      (InspectBal (toDate "20220101") (FutureCurrentPoolBalance Nothing) 4000)
      (inspects!!0)
     ,testCase "pool current balance (run time 1)" $
     assertEqual "pool current balance (run time 1)"
      (InspectBal (toDate "20220131") (FutureCurrentPoolBalance Nothing) 4000)
      (inspects!!1)
     ,testCase "pool current balance (run time 2)" $
     assertEqual "pool current balance (run time 2)"
      (InspectBal (toDate "20220228") (FutureCurrentPoolBalance Nothing) 3946.27)
      (inspects!!2)
     ,testCase "pool current balance (run time 60)" $
     assertEqual "pool current balance (run time 60)"
      (InspectBal (toDate "20270131") (FutureCurrentPoolBalance Nothing) 0.0)
      (inspects!!61)
   ]

