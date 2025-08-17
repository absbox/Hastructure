{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Loan 
  (projectLoanFlow,updateOriginDate)
  where

import qualified Data.Time as T
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import InterestRate
import Asset
import Lib
import Util
import DateUtil
import Types
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

import AssetClass.AssetBase
import AssetClass.AssetCashflow

import Debug.Trace
import Assumptions (AssetDefaultAssumption(DefaultCDR))
import qualified Asset as A
import Control.Lens hiding (element)
import Control.Lens.TH
import qualified Data.DList as DL
debug = flip trace


-- instance Asset Loan where
projectLoanFlow :: ((Balance,Int,IRate), Balance, Date, AmortPlan, DayCount, IRate, Rational) -> (Dates, [DefaultRate],[PrepaymentRate],[IRate],[Int]) -> (DL.DList CF.TsRow, Balance, Date ,Rational)
projectLoanFlow ((originBal,ot,or), startBal, lastPayDate, pt, dc,startRate, begFactor) (cfDates,defRates,ppyRates,rateVector,remainTerms) = 
  let 
    initRow = CF.LoanFlow lastPayDate startBal 0.0 0.0 0.0 0.0 0.0 0.0 startRate Nothing
  in 
    foldl
      (\(acc,begBal,lastPaidDate,factor) (pDate, ppyRate, defRate, intRate, rt)
        -> let 
             -- begBal = view CF.tsRowBalance (last acc)
             -- lastPaidDate = getDate (last acc)
             newDefault = mulBR begBal defRate
             newPrepay = mulBR (begBal - newDefault) ppyRate
             intBal = begBal - newDefault - newPrepay
             newFactor = factor * (1-defRate) * (1- ppyRate )
             newInt = case (rt,pt) of
                        (0,F_P) -> 0
                        (_,F_P) -> mulBR (mulBIR originBal or) newFactor
                        _ -> calcInt intBal lastPaidDate pDate intRate dc 
             newPrin = case (rt,pt) of
                         (1,I_P) -> intBal
                         (1,F_P) -> intBal
                         (0,F_P) -> 0
                         (_,F_P) -> mulBR (divideBI intBal rt)  newFactor
                         (_,I_P) -> 0
                         (0,ScheduleRepayment cf _) -> intBal
                         (_,ScheduleRepayment cf _) -> 
                          let 
                            projAmt = fromRational $ getValByDate cf Inc pDate 
                          in 
                            if rt == 1 then
                              intBal
                            else
                              mulBR projAmt newFactor
                         _ -> error $ "failed to match Loan Project newPrin"++ show (rt,pt)
             endBal = intBal - newPrin
           in
             (DL.snoc acc (CF.LoanFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 intRate Nothing)
             ,endBal
             ,pDate
             ,newFactor))
      (DL.singleton initRow, startBal, lastPayDate, begFactor)
      (zip5 cfDates ppyRates defRates rateVector remainTerms)  

instance Asset Loan where
  calcCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype _) bal rate term _ ) asOfDay mRates 
    = fst <$>
      projCashflow pl
                   asOfDay
                   (A.LoanAssump Nothing Nothing Nothing Nothing
                    ,A.DummyDelinqAssump
                    ,A.DummyDefaultAssump)
                   mRates

  getCurrentBal pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype _) _bal _rate _term _ )
    = _bal

  getOriginRate pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype _) _bal _rate _term _ )
    = case or of
        Fix _ _r -> _r
        Floater _ _ _ _r _ _ _ _ -> _r 

  getCurrentRate pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd ptype _) _bal _rate _term _ )
    = _rate

  getOriginBal pl@(PersonalLoan (LoanOriginalInfo ob _ _ _ _ _ _) _ _ _ _ ) = ob

  isDefaulted pl@(PersonalLoan _ _ _ _ (Defaulted _)) = True
  isDefaulted PersonalLoan {} = False
 
  getOriginInfo (PersonalLoan oi cb cr rt st) = oi
  getOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P _) cb cr rt st ) = sd
  
  resetToOrig m@(PersonalLoan (LoanOriginalInfo ob or ot p sd I_P obr) cb cr rt st ) 
    = PersonalLoan (LoanOriginalInfo ob or ot p sd I_P obr) ob (getOriginRate m) ot st
  
  getRemainTerms (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P _) cb cr rt st ) = rt

  updateOriginDate (PersonalLoan (LoanOriginalInfo ob or ot p sd I_P obr) cb cr rt st ) nd
    = PersonalLoan (LoanOriginalInfo ob or ot p nd I_P obr) cb cr rt st 

  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd (ScheduleRepayment ts mDp) _) _bal _rate _term _ ) extra
    = let 
        pdays = getTsDates ts 
        extraDates = genSerialDates (fromMaybe MonthEnd mDp) Inc (last pdays) extra
      in 
        pdays ++ extraDates
  
  getPaymentDates pl@(PersonalLoan (LoanOriginalInfo ob _ ot p sd _ _) _bal _rate _term _ )  extra
    = genDates sd p (ot+extra)
  
  -- ^ Project cashflow for loans with prepayment/default/loss and interest rate assumptions
  projCashflow pl@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType _) cb cr rt Current) 
               asOfDay 
               (A.LoanAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRate 
    = let
        recoveryLag = maybe 0 getRecoveryLag recoveryAssump
        lastPayDate:cfDates = lastN (rt + recoveryLag + 1) $ sd:getPaymentDates pl recoveryLag
      in
        do
          rateVector <- A.projRates cr or mRate cfDates
          ppyRates <- A.buildPrepayRates pl (lastPayDate:cfDates) prepayAssump 
          defRates <- A.buildDefaultRates pl (lastPayDate:cfDates) defaultAssump
          let dc = getDayCount or          
          let remainTerms = reverse $ replicate recoveryLag 0 ++ [0..rt] -- `debug` ("rateVector"++show rateVector)
          let initFactor = case prinPayType of 
                         ScheduleRepayment ts _ -> 
                          let 
                            scheduleBals = scanl (-) ob $ fromRational <$> getTsVals ts
                          in 
                            divideBB cb (scheduleBals!!(ot - rt))
                         _ -> 1.0  
          let (txns,_,_,_) = projectLoanFlow ((ob,ot,getOriginRate pl), cb,lastPayDate,prinPayType,dc,cr,initFactor) (cfDates,defRates,ppyRates,rateVector,remainTerms)  -- `debug` (" rateVector"++show rateVector)
          txns' <- patchLossRecovery (DL.toList txns) recoveryAssump
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns'
          let begBal = CF.buildBegBal futureTxns
          return $ (applyHaircut ams (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns), historyM)
  -- ^ Project cashflow for defautled loans 
  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType _) cb cr rt (Defaulted (Just defaultedDate))) 
               asOfDay 
               (_,_,A.DefaultedRecovery rr lag timing)
               _
    = let 
        (cf_dates1,cf_dates2) = splitAt (pred lag) $ genDates defaultedDate p (lag+ length timing)
        beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr Nothing| d <- cf_dates1 ]
        recoveries = calcRecoveriesFromDefault cb rr timing
        _txns = [  CF.LoanFlow d 0 0 0 0 0 r 0 cr Nothing | (d,r) <- zip cf_dates2 recoveries ]
        (_, txns) = splitByDate (beforeRecoveryTxn++_txns) asOfDay EqToRight -- `debug` ("AS OF Date"++show asOfDay)
        (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
        begBal = CF.buildBegBal futureTxns
      in 
        Right $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns, historyM)

  projCashflow m@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType _) cb cr rt (Defaulted Nothing)) asOfDay assumps _
    = Right $ (CF.CashFlowFrame (cb,asOfDay,Nothing) [CF.LoanFlow asOfDay 0 0 0 0 0 0 0 cr Nothing],Map.empty)
  
  projCashflow a b c d = Left $ "failed to match projCashflow for Loan "++show a++show b++show c++show d
  
  splitWith l@(PersonalLoan (LoanOriginalInfo ob or ot p sd prinPayType obr) cb cr rt st) rs
    = [ PersonalLoan (LoanOriginalInfo (mulBR ob ratio) or ot p sd prinPayType obr) (mulBR cb ratio) cr rt st | ratio <- rs ]
