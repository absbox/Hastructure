module UT.DealTest2 (td,queryTests)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal

import Deal.DealQuery (queryCompound)
import Deal.DealCollection (CollectionRule(..))
import qualified Accounts as A
import qualified Stmt as S
import qualified Pool as P
import qualified Asset as Ast
import qualified AssetClass.Mortgage as ACM
import qualified AssetClass.AssetBase as AB
import qualified Expense as F
import qualified Deal.DealBase as D
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

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Types (PoolId(PoolConsol))

dummySt = (0,toDate "19000101",Nothing)

td = D.TestDeal {
  D.name = "test deal"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
  ,D.accounts = Map.fromList
                [("General", A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing })]
  ,D.fees = (Map.fromList [("Service-Fee"
                         ,F.Fee{F.feeName="service-fee"
                                ,F.feeType = F.FixFee 10
                                ,F.feeStart = (T.fromGregorian 2022 1 1)
                                ,F.feeDue = 100
                                ,F.feeDueDate = Nothing
                                ,F.feeArrears = 0
                                ,F.feeLastPaidDay = Nothing
                                ,F.feeStmt = Nothing})])
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
                             ,L.bndDueIntOverInt=0.0
                             ,L.bndDueIntDate=Nothing
                             ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                             ,L.bndStmt=Nothing})
                             ,("B"
                               ,L.Bond{
                                L.bndName="B"
                               ,L.bndType=L.Equity
                               ,L.bndOriginInfo= L.OriginalInfo{
                                                  L.originBalance=3000
                                                  ,L.originDate= (T.fromGregorian 2022 1 1)
                                                  ,L.originRate= 0.08
                                                  ,L.maturityDate = Nothing}
                               ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                               ,L.bndBalance=500
                               ,L.bndRate=0.08
                               ,L.bndDuePrin=0.0
                               ,L.bndDueInt=0.0
                               ,L.bndDueIntDate=Nothing
                               ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = D.MultiPool $ 
              Map.fromList [(PoolConsol,
                      P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix DC_ACT_365F 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=(T.fromGregorian 2022 1 1)
                                           ,AB.prinType= AB.Level
                                           ,AB.prepaymentPenalty = Nothing}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         AB.Current
                                ,AB.Mortgage
                                   AB.MortgageOriginalInfo{
                                     AB.originBalance=4000
                                     ,AB.originRate=Fix DC_ACT_365F 0.085
                                     ,AB.originTerm=60
                                     ,AB.period=Monthly
                                     ,AB.startDate=(T.fromGregorian 2022 1 1)
                                     ,AB.prinType= AB.Level
                                     ,AB.prepaymentPenalty = Nothing}
                                   200
                                   0.085
                                   60
                                   Nothing
                                   (AB.Defaulted Nothing)
                                 ]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1
                 ,P.issuanceStat = Nothing}
                )]
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                  (W.PayFee Nothing "General" ["Service-Fee"] Nothing)
                                 ,(W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [Collect Nothing W.CollectedCash "General"]
 ,D.custom = Nothing
 ,D.liqProvider = Nothing
 ,D.triggers = Nothing
 ,D.ledgers = Nothing
}

bondGroups = Map.fromList [("A"
                             ,L.BondGroup (Map.fromList 
                               [
                                ("A-1",L.Bond{
                                        L.bndName="A-1"
                                        ,L.bndType=L.Sequential
                                        ,L.bndOriginInfo= L.OriginalInfo{
                                                            L.originBalance=3000
                                                            ,L.originDate= (T.fromGregorian 2022 1 1)
                                                            ,L.originRate= 0.08
                                                            ,L.maturityDate = Nothing}
                                        ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                                        ,L.bndBalance=1500
                                        ,L.bndRate=0.08
			                ,L.bndStepUp = Nothing
                                        ,L.bndDuePrin=0.0
                                        ,L.bndDueInt=0.0
                                        ,L.bndDueIntOverInt=0.0
                                        ,L.bndDueIntDate=Nothing
                                        ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                                        ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                                        ,L.bndStmt=Nothing}),
                                ("A-2",L.Bond{
                                        L.bndName="A-2"
                                        ,L.bndType=L.Sequential
                                        ,L.bndOriginInfo= L.OriginalInfo{
                                                            L.originBalance=2000
                                                            ,L.originDate= (T.fromGregorian 2022 1 1)
                                                            ,L.originRate= 0.08
                                                            ,L.maturityDate = Nothing}
                                        ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                                        ,L.bndBalance=1000
                                        ,L.bndRate=0.08
                                        ,L.bndDuePrin=0.0
                                        ,L.bndDueInt=0.0
			                ,L.bndStepUp = Nothing
                                        ,L.bndDueIntOverInt=0.0
                                        ,L.bndDueIntDate=Nothing
                                        ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                                        ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                                        ,L.bndStmt=Nothing})
                                 ]            
                                ) Nothing)
                             ,("B"
                               ,L.Bond{
                                L.bndName="B"
                               ,L.bndType=L.Equity
			       ,L.bndStepUp = Nothing
                               ,L.bndOriginInfo= L.OriginalInfo{
                                                  L.originBalance=3000
                                                  ,L.originDate= (T.fromGregorian 2022 1 1)
                                                  ,L.originRate= 0.08
                                                  ,L.maturityDate = Nothing}
                               ,L.bndInterestInfo= L.Fix 0.08 DC_ACT_365F
                               ,L.bndBalance=500
                               ,L.bndRate=0.08
                               ,L.bndDuePrin=0.0
                               ,L.bndDueInt=0.0
                               ,L.bndDueIntDate=Nothing
                               ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndStmt=Nothing})
                            ]

tdBondGroup = td { D.bonds = bondGroups,
                   D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                  W.PayFee Nothing "General" ["Service-Fee"] Nothing
                                 ,W.AccrueAndPayInt Nothing "General" ["A"] Nothing
                                 ,W.PayPrinGroup Nothing "General" "A" W.ByProRataCurBal Nothing
                                 ,W.PayPrin Nothing "General" ["B"] Nothing
                ])]
   }

queryTests =  testGroup "Deal Group Test"
  [
    let
     currBndGrpBal = queryCompound tdBondGroup epocDate (CurrentBondBalanceOf ["A"])
    in
     testCase "group bond balance" $
     assertEqual "should be 2500" (Right 2500) currBndGrpBal
    ,let 
        bndsFound = D.viewDealAllBonds tdBondGroup
     in 
        testCase "view viewDealAllBonds " $
        assertEqual "should be 3" 3 (length bndsFound)
    ,let 
        totalBndBal = queryCompound tdBondGroup epocDate CurrentBondBalance 
    in 
        testCase "total bond balance" $
        assertEqual "should be 3000" (Right 3000) totalBndBal
    ,let
        originBndbal = queryCompound tdBondGroup epocDate (OriginalBondBalanceOf ["A"])
    in
        testCase "original bond balance" $
        assertEqual "should be 5000" (Right 5000) originBndbal
  ]
