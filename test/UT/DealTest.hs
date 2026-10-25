module UT.DealTest(td2,queryTests,triggerTests,dateTests,liqProviderTest,poolFlowTest,priceBondsIrrTest
                  ,mapLookupSafetyTest,validationNameTest,aggRuleValidationTest,nonPerfAssumptionTest)

where

import Test.Tasty
import Test.Tasty.HUnit
import Deal
import Deal.DealQuery (queryCompound, calcBondTargetBalance)
import Deal.DealValidation (validatePreRun)

import qualified Accounts as A
import qualified Stmt as Stmt
import Deal.DealCollection (CollectionRule(..))
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
import qualified Ledger as LD
import Lib
import Types

import Control.Lens hiding (Index,Empty)
import Control.Lens.TH
import Data.Maybe
import Data.Either
import Data.List (isInfixOf)

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import qualified Data.DList as DL

import Debug.Trace
debug = flip Debug.Trace.trace

dummySt = (0,toDate "19000101",Nothing)





td2 = D.TestDeal {
  D.name = "test deal1"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
  ,D.accounts = (Map.fromList
  [("General", (A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing
  })),
   ("Reserve", (A.Account { A.accName="Reserve" ,A.accBalance=0.0 ,A.accType=Just (A.FixReserve 500), A.accInterest=Nothing ,A.accStmt=Nothing
  }))
  ])
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
                             ,L.bndStmt=Nothing, L.bndStepUp = Nothing })
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
                               ,L.bndStmt=Nothing, L.bndStepUp = Nothing, L.bndDueIntOverInt = 0.0 })
                         ]
           )
  ,D.pool = D.MultiPool $
               Map.fromList $ 
                   [( PoolConsol,
                      P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix DC_ACT_365F 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=(T.fromGregorian 2022 1 1)
                                           ,AB.prinType= AB.Level
                                           ,AB.prepaymentPenalty = Nothing, AB.obligor = Nothing }
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
                                     ,AB.prepaymentPenalty = Nothing, AB.obligor = Nothing }
                                   200
                                   0.085
                                   60
                                   Nothing
                                   (AB.Defaulted Nothing)
                                 ]
                 ,P.futureCf=Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1
                 ,P.issuanceStat = Just $ Map.fromList [(RuntimeCurrentPoolBalance, 70)], P.futureScheduleCf = Nothing, P.extendPeriods = Nothing }
                )]
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                  (W.PayFee Nothing "General" ["Service-Fee"] Nothing)
                                 ,(W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
   ])]
 ,D.collects = [Collect Nothing W.CollectedInterest "General"
             ,Collect Nothing W.CollectedPrincipal "General"]
 ,D.custom = Nothing
 ,D.liqProvider = Just $ Map.fromList $
                    [("Liq1",CE.LiqFacility 
                                "" 
                                (CE.FixSupport 100)
                                50
                                (ByAvailAmount 100)
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing 
                                0
                                0
                                (toDate "20220201")
                                Nothing
                                (Just (Stmt.Statement (DL.fromList [SupportTxn (toDate "20220215") (ByAvailAmount 110) 10 40 0 0 Empty 
                                                    ,SupportTxn (toDate "20220315") (ByAvailAmount 100) 10 50 0 0 Empty]))))]
 ,D.triggers = Just $
                Map.fromList $
                  [(BeginDistributionWF,
                    Map.fromList [ ("revolving trigger",Trg.Trigger{Trg.trgCondition = IfDate G (toDate "20220501")
                                                                    ,Trg.trgEffects = Trg.DealStatusTo Revolving
                                                                    ,Trg.trgStatus = False 
                                                                    ,Trg.trgCurable = False , Trg.trgStmt = Nothing })]
                                                                    )]
 ,D.ledgers = Nothing
 ,D.stats = (Map.empty,Map.empty,Map.empty,Map.empty)
, D.rateCap = Nothing }

baseDeal = D.TestDeal {
  D.name = "base deal"
  ,D.status = Amortizing
  ,D.rateSwap = Nothing
  ,D.stats = (Map.empty,Map.empty,Map.empty,Map.empty)
  ,D.currencySwap = Nothing
  ,D.dates = CurrentDates (toDate "20220101",toDate "20220101") Nothing (toDate "20300101")
                (toDate "20220201" , MonthFirst) (toDate "20220225" , MonthFirst)
  ,D.accounts = Map.fromList [("General", A.Account { A.accName="General" ,A.accBalance=1000.0 ,A.accType=Nothing, A.accInterest=Nothing ,A.accStmt=Nothing})]
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
                             ,L.bndStepUp=Nothing
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
                               ,L.bndStepUp=Nothing
                               ,L.bndDuePrin=0.0
                               ,L.bndDueInt=0.0
                               ,L.bndDueIntOverInt=0.0
                               ,L.bndDueIntDate=Nothing
                               ,L.bndLastIntPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndLastPrinPay = Just (T.fromGregorian 2022 1 1)
                               ,L.bndStmt=Nothing})
                         ]
           )
  ,D.pool = D.MultiPool $
               Map.fromList $ 
                   [( PoolConsol,
                      P.Pool {P.assets=[AB.Mortgage
                                         AB.MortgageOriginalInfo{
                                           AB.originBalance=4000
                                           ,AB.originRate=Fix DC_ACT_365F 0.085
                                           ,AB.originTerm=60
                                           ,AB.period=Monthly
                                           ,AB.startDate=(T.fromGregorian 2022 1 1)
                                           ,AB.prinType= AB.Level
                                           ,AB.obligor = Nothing
                                           ,AB.prepaymentPenalty = Nothing}
                                         4000
                                         0.085
                                         60
                                         Nothing
                                         AB.Current]
                 ,P.futureCf= Nothing
                 ,P.extendPeriods = Nothing
                 ,P.asOfDate = T.fromGregorian 2022 1 1
                 ,P.issuanceStat = Just $ Map.fromList [(RuntimeCurrentPoolBalance, 70),(IssuanceBalance, 4000)], P.futureScheduleCf = Nothing })]
   ,D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [
                                 (W.PayInt Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["A"] Nothing)
                                 ,(W.PayPrin Nothing "General" ["B"] Nothing)
   ])]
 ,D.collects = [Collect Nothing W.CollectedCash "General"]
 ,D.custom = Nothing
 ,D.liqProvider = Nothing 
 ,D.triggers = Nothing 
 ,D.ledgers = Nothing
 ,D.rateCap = Nothing
}

poolFlowTest = 
   let 
     (deal,mPoolCf,mResultComp,mPricing,oustandingFlow) = case (runDeal baseDeal S.empty Nothing AP.defaultNonPerfAssumption) of
                                              (Left er) -> error $ "Deal run failed"++ show er
                                              (Right (a,b,c,d,e)) -> (a,b,c,d,e) 
     bndMap = D.viewBondsInMap deal
   in 
   testGroup "pool cashflow test" 
    [
      testCase "pool begin flow" $
      assertEqual "pool size should be 60" 
      (Map.fromList [(PoolConsol ,60)])
      (Map.map CF.sizeCashFlowFrame  mPoolCf )  -- `debug` ("pool from test "++ show (mPoolCf))
      
      ,testCase "total principal bal" $
      assertEqual "pool bal should equal to total collect"
      (Map.fromList [(PoolConsol ,4000)])
      (Map.map CF.totalPrincipal mPoolCf) -- `debug` ("pool "++ show (viewBond))
      
      -- ,testCase "last bond A payment date" $
      --  assertEqual "pool bal should equal to total collect"
      --  (Just (BondTxn (toDate "20240201") 0.00 0.00 30.56 0.080000 30.56 0.00 0.00 (Just 0.0) (PayPrin ["A"])))
      --  $ (\s -> last (DL.toList (view Stmt.statementTxns s))) <$> (L.bndStmt $ (bndMap Map.! "A")) `debug` ("bond A stmt " ++ show (L.bndStmt $ (bndMap Map.! "A")))
    ]


queryTests =  testGroup "deal stat query Tests"
  [
    let
      ctx = D.defaultRunContext
      currentDefBal = queryCompound td2 ctx epocDate CurrentPoolDefaultedBalance
    in
      testCase "query current assets in defaulted status" $
      assertEqual "should be 200" (Right 200) currentDefBal
  ,let
      bA = fromJust (Map.lookup "A" (D.bonds td2))
      badBond = bA { L.bndType = L.PAC (IRateCurve [TsPoint (toDate "20220101") 0.05]) }
      badDeal = td2 { D.bonds = Map.insert "A" badBond (D.bonds td2) }
    in
      testCase "PAC target with a non-balance curve returns Left" $
      assertBool "error mentions BalanceCurve" $
        case calcBondTargetBalance badDeal D.defaultRunContext (toDate "20220101") badBond of
          Left e -> "BalanceCurve" `isInfixOf` e
          Right _ -> False
  ]

triggerTests = testGroup "Trigger Tests"
  [ let 
      setup = 0 
      poolflows = (CF.CashFlowFrame dummySt $
                     [CF.MortgageDelinqFlow (toDate "20220201") 800 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing 
                     ,CF.MortgageDelinqFlow (toDate "20220301") 700 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                     ,CF.MortgageDelinqFlow (toDate "20220401") 600 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing 
                     ,CF.MortgageDelinqFlow (toDate "20220501") 500 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                     ,CF.MortgageDelinqFlow (toDate "20220601") 400 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                     ,CF.MortgageDelinqFlow (toDate "20220701") 300 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                     ]
		   ,Nothing)
      poolflowM = Map.fromList [(PoolConsol, poolflows)]
      ctx = D.RunContext poolflowM Nothing Nothing
      ads = [PoolCollection (toDate "20220201") "" 
             ,RunWaterfall  (toDate "20220225") ""
             ,PoolCollection (toDate "20220301")""
             ,RunWaterfall  (toDate "20220325") ""
             ,PoolCollection (toDate "20220401")""
             ,RunWaterfall  (toDate "20220425") ""
             ,PoolCollection (toDate "20220501")""
             ,RunWaterfall  (toDate "20220525") ""
             ,PoolCollection (toDate "20220601")""
             ,RunWaterfall  (toDate "20220625") ""
             ,PoolCollection (toDate "20220701")""
             ,RunWaterfall  (toDate "20220725") ""  ]
      (fdeal,_,_) = case run td2 ctx (Just ads) Nothing DL.empty of 
                    Left _ -> error ""
                    Right x -> x
    in 
      testCase "deal becomes revolving" $
      assertEqual "revoving" 
        Revolving 
        (D.status fdeal)
  ]

dateTests = 
  let 
   a = PreClosingDates
        (toDate "20220601") 
        (toDate "20220610") 
        Nothing
        (toDate "20220901") 
        (toDate "20220630",MonthEnd)
        (toDate "20220715",DayOfMonth 10)
  in 
   testGroup "Deal Tests" 
   [ testCase "Dates pattern" $
     assertEqual  ""
    (Right $ 
      ((toDate "20220601"), (toDate "20220610"),(toDate "20220715")
        ,[PoolCollection (toDate "20220630") "",PoolCollection (toDate "20220731") "",PoolCollection (toDate "20220831") ""]
        ,[RunWaterfall (toDate "20220715") "",RunWaterfall (toDate "20220810") ""]
        ,(toDate "20220901")
        ,[]))
      (populateDealDates a Amortizing)
    ,testCase "populateDealDates with no future pay date returns Left instead of crashing" $
     assertBool "should be Left"
       (isLeft (populateDealDates (CurrentDates (toDate "20211201",toDate "20220101") Nothing (toDate "20220101") (toDate "20220101", MonthFirst) (toDate "20220101", MonthFirst)) Amortizing))
    ]
  
liqProviderTest = 
  let 
    liq1 = CE.LiqFacility "" 
                       (CE.FixSupport 100)
                       90
                       (ByAvailAmount 100)
                       (Just CE.IncludeDueInt)
                       Nothing -- rate type
                       Nothing -- premium rate type
                       
                       Nothing -- rate
                       Nothing -- premium reate
                       (Just (toDate "20220201"))
                       0
                       0
                       (toDate "20220301")
                       Nothing
                       (Just (Stmt.Statement 
                               (DL.fromList ([SupportTxn (toDate "20220215") (ByAvailAmount 110) 40 40 0 0 Empty
                               ,SupportTxn (toDate "20220315") (ByAvailAmount 100) 50 90 0 0 Empty
                               ]))))
  in 
    testGroup "Liq provider test" 
      [testCase "Liq Provider Int test" $
          assertEqual ""
           (ByAvailAmount 100)
           (CE.liqCredit $ CE.accrueLiqProvider (toDate "20221101") liq1)
      ]

priceBondsIrrTest = 
  let 
    historyCash = [(toDate "20220101", -1000.0),(toDate "20230101", 1100.0)]
    holding = AP.HoldingBond historyCash 0 Nothing
  in 
    testGroup "priceBonds IrrInput Tests"
      [ testCase "unknown bond name returns Left instead of crashing" $
          case priceBonds baseDeal (AP.IrrInput (Map.fromList [("NONEXISTENT", holding)])) of
            Left er -> assertBool ("error message should mention the missing bond, got: " ++ er)
                                    ("NONEXISTENT" `isInfixOf` er)
            Right _ -> assertFailure "expected Left when pricing an unknown bond name"
      , testCase "known bond name still prices successfully" $
          case priceBonds baseDeal (AP.IrrInput (Map.fromList [("A", holding)])) of
            Left er -> assertFailure ("expected Right for known bond \"A\" but got: " ++ er)
            Right m  -> assertEqual "should return a result for bond A" ["A"] (Map.keys m)
      ]

mapLookupSafetyTest = 
  let 
    ctx = D.RunContext Map.empty Nothing Nothing
    d0 = toDate "20220101"
    dealWithLedger = baseDeal { D.ledgers = Just (Map.fromList [("L1", LD.Ledger "L1" (Debit, 0) Nothing)]) }
    badTrg = Trg.Trigger (IfDate G (toDate "20220501")) (Trg.DoAccrueFee ["NONEXISTENTFEE"]) False False Nothing
    tBadTrg = td2 { D.triggers = Just $ Map.fromList [(BeginDistributionWF, Map.fromList [("bad fee trigger", badTrg)])] }
    poolflows = (CF.CashFlowFrame dummySt $
                   [CF.MortgageDelinqFlow (toDate "20220201") 800 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing 
                   ,CF.MortgageDelinqFlow (toDate "20220301") 700 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                   ,CF.MortgageDelinqFlow (toDate "20220401") 600 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing 
                   ,CF.MortgageDelinqFlow (toDate "20220501") 500 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                   ,CF.MortgageDelinqFlow (toDate "20220601") 400 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                   ,CF.MortgageDelinqFlow (toDate "20220701") 300 100 20 0 0 0 0 0 0.08 Nothing Nothing Nothing
                   ]
                 ,Nothing)
    poolflowM = Map.fromList [(PoolConsol, poolflows)]
    ctxPool = D.RunContext poolflowM Nothing Nothing
    ads = [PoolCollection (toDate "20220201") "" 
           ,RunWaterfall  (toDate "20220225") ""
           ,PoolCollection (toDate "20220301")""
           ,RunWaterfall  (toDate "20220325") ""
           ,PoolCollection (toDate "20220401")""
           ,RunWaterfall  (toDate "20220425") ""
           ,PoolCollection (toDate "20220501")""
           ,RunWaterfall  (toDate "20220525") ""  ]
  in 
    testGroup "map lookup safety (missing names return Left)"
    [
      testCase "ResetBondRate with missing bond returns Left" $
        assertBool "Left" (isLeft (run baseDeal ctx (Just [ResetBondRate d0 "NONEXISTENT"]) Nothing DL.empty))
    , testCase "StepUpBondRate with missing bond returns Left" $
        assertBool "Left" (isLeft (run baseDeal ctx (Just [StepUpBondRate d0 "NONEXISTENT"]) Nothing DL.empty))
    , testCase "FundBond with missing bond returns Left" $
        assertBool "Left" (isLeft (run baseDeal ctx (Just [FundBond d0 Nothing "NONEXISTENT" "General" 100]) Nothing DL.empty))
    , testCase "RefiBondRate with missing bond returns Left" $
        assertBool "Left" (isLeft (run baseDeal ctx (Just [RefiBondRate d0 "General" "NONEXISTENT" (L.Fix 0.08 DC_ACT_365F)]) Nothing DL.empty))
    , testCase "RefiBondRate with missing account returns Left" $
        assertBool "Left" (isLeft (run baseDeal ctx (Just [RefiBondRate d0 "NONEXISTENTACC" "A" (L.Fix 0.08 DC_ACT_365F)]) Nothing DL.empty))
    , testCase "DoAccrueFee trigger effect with missing fee returns Left" $
        assertBool "Left" (isLeft (run tBadTrg ctxPool (Just ads) Nothing DL.empty))
    , testCase "FeePaidAmt query with missing fee returns Left" $
        assertBool "Left" (isLeft (queryCompound baseDeal ctx d0 (FeePaidAmt ["NONEXISTENT"])))
    , testCase "FeeTxnAmtBy query with missing fee returns Left" $
        assertBool "Left" (isLeft (queryCompound baseDeal ctx d0 (FeeTxnAmtBy d0 ["NONEXISTENT"] Nothing)))
    , testCase "AccTxnAmtBy query with missing account returns Left" $
        assertBool "Left" (isLeft (queryCompound baseDeal ctx d0 (AccTxnAmtBy d0 ["NONEXISTENT"] Nothing)))
    , testCase "LedgerTxnAmt query with missing ledger returns Left" $
        assertBool "Left" (isLeft (queryCompound dealWithLedger ctx d0 (LedgerTxnAmt ["NONEXISTENT"] Nothing)))
    , testCase "FeePaidAmt query with existing fee still works" $
        assertEqual "0" (Right 0 :: Either String Rational) (queryCompound td2 ctx d0 (FeePaidAmt ["Service-Fee"]))
    , testCase "AccTxnAmtBy query with existing account still works" $
        assertEqual "0" (Right 0 :: Either String Rational) (queryCompound td2 ctx d0 (AccTxnAmtBy d0 ["General"] Nothing))
    , testCase "LedgerTxnAmt query with existing ledger still works" $
        assertEqual "0" (Right 0 :: Either String Rational) (queryCompound dealWithLedger ctx d0 (LedgerTxnAmt ["L1"] Nothing))
    ]

validationNameTest = 
  let 
    errMsgs deal = let (_,errs) = validatePreRun deal in [ m | ErrorMsg m <- errs ]
    nameErrs deal = [ m | m <- errMsgs deal, "not found in deal" `isInfixOf` m ]
    badWaterfall = td2 { D.waterfall = Map.fromList [(W.DistributionDay Amortizing, [ W.PayInt Nothing "General" ["NONEXISTENTBOND"] Nothing ])] }
    badTrigger = td2 { D.triggers = Just (Map.fromList [(BeginDistributionWF, Map.fromList [("t", Trg.Trigger (IfDate G (toDate "20220501")) (Trg.DoAccrueFee ["NONEXISTENTFEE"]) False False Nothing)])]) }
    badCollect = td2 { D.collects = [ Collect Nothing W.CollectedInterest "NONEXISTENTACC" ] }
  in 
    testGroup "pre-run name validation"
    [
      testCase "waterfall referencing a missing bond is flagged" $
        assertBool ("expected a bond error, got: " ++ show (nameErrs badWaterfall))
          ("Bond/BondGroup not found in deal: NONEXISTENTBOND" `elem` nameErrs badWaterfall)
    , testCase "trigger effect referencing a missing fee is flagged" $
        assertBool ("expected a fee error, got: " ++ show (nameErrs badTrigger))
          ("Fee not found in deal: NONEXISTENTFEE" `elem` nameErrs badTrigger)
    , testCase "collect rule referencing a missing account is flagged" $
        assertBool ("expected an account error, got: " ++ show (nameErrs badCollect))
          ("Account not found in deal: NONEXISTENTACC" `elem` nameErrs badCollect)
    , testCase "a valid deal has no name reference errors" $
        assertBool ("unexpected name errors: " ++ show (nameErrs td2)) (null (nameErrs td2))
    ]

aggRuleValidationTest = 
  let 
    errMsgs deal = let (_,errs) = validatePreRun deal in [ m | ErrorMsg m <- errs ]
    aggErrs deal = [ m | m <- errMsgs deal, "Pool source" `isInfixOf` m || "Pool Id not found" `isInfixOf` m ]
  in 
    testGroup "collection rule validation"
    [
      testCase "normal collect rules validate with no errors" $
        assertBool ("unexpected errors: " ++ show (aggErrs baseDeal)) (null (aggErrs baseDeal))
    , testCase "a deal with empty collects does not crash and reports no errors" $
        let deal = baseDeal { D.collects = [] }
        in assertBool ("unexpected errors: " ++ show (aggErrs deal)) (null (aggErrs deal))
    , testCase "over-weighted collect source is flagged" $
        let deal = baseDeal { D.collects = [ Collect Nothing W.CollectedInterest "General"
                                           , Collect Nothing W.CollectedInterest "General" ] }
        in assertBool ("expected a weight error, got: " ++ show (aggErrs deal))
                      (any ("has a weight of" `isInfixOf`) (aggErrs deal))
    , testCase "unknown pool id in a collect rule is flagged" $
        let deal = baseDeal { D.collects = [ Collect (Just [PoolName "NOPE"]) W.CollectedInterest "General" ] }
        in assertBool ("expected a pool id error, got: " ++ show (aggErrs deal))
                      (any ("Pool Id not found" `isInfixOf`) (aggErrs deal))
    ]

nonPerfAssumptionTest = 
  testGroup "NonPerfAssumption / RunContext defaults"
  [
    testCase "default NonPerfAssumption is fully defined and shows without throwing" $
      assertBool "show forces every field" (not (null (show AP.defaultNonPerfAssumption)))
  , testCase "default NonPerfAssumption has no interest assumption" $
      assertBool "interest is Nothing"
        (case AP.interest AP.defaultNonPerfAssumption of Nothing -> True; _ -> False)
  , testCase "nonPerfWithRates sets interest and leaves other fields untouched" $
      let a = AP.nonPerfWithRates (Just [])
      in do
        assertBool "interest set" (case AP.interest a of Just [] -> True; _ -> False)
        assertBool "show forces every field" (not (null (show a)))
        assertBool "revolving stays Nothing" (case AP.revolving a of Nothing -> True; _ -> False)
        assertBool "pricing stays Nothing"   (case AP.pricing a of Nothing -> True; _ -> False)
  , testCase "default RunContext is fully defined" $
      assertBool "show forces every field" (not (null (show D.defaultRunContext)))
  , testCase "default RunContext has an empty pool flow" $
      assertEqual "empty pool flow" Map.empty (D.runPoolFlow D.defaultRunContext)
  ]
