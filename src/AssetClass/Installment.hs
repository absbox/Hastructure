{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Installment 
  (projectInstallmentFlow, updateOriginDate)
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Aeson.Types
import GHC.Generics

import Asset
import InterestRate
import qualified Assumptions as A
import Types 
import Lib
import Util
import DateUtil
import qualified Cashflow as CF

import AssetClass.AssetBase

import Debug.Trace
import AssetClass.AssetCashflow
import qualified Asset as Ast
debug = flip trace


projectInstallmentFlow :: (Balance,Date,(Balance,Balance),IRate,Rational) -> (Dates, [DefaultRate], [PrepaymentRate], [Int]) -> ([CF.TsRow],Rational)
projectInstallmentFlow (startBal, lastPaidDate, (originRepay,originInt), startRate,begFactor) (cfDates, defRates, ppyRates, remainTerms)
  = let 
      initRow = CF.LoanFlow lastPaidDate startBal 0.0 0.0 0.0 0.0 0.0 0.0 startRate Nothing
      calcPrin _rt _bal _opmt _factor = case _rt of
                                          1 -> _bal
                                          0 -> 0
                                          _ -> mulBR _opmt _factor
    in
      foldl
        (\(acc,factor) (pDate, ppyRate, defRate, rt) -> 
          let 
            begBal = CF.mflowBalance (last acc)
            newDefault = mulBR begBal defRate
            newPrepay = mulBR (begBal - newDefault) ppyRate
            intBal = begBal - newDefault - newPrepay
            newFactor = factor * (1-defRate) * (1- ppyRate)
            newInt = if rt > 0 then 
                       mulBR originInt newFactor
                     else
                       0 
            newPrin = calcPrin rt intBal originRepay newFactor
            endBal = intBal - newPrin
          in 
            (acc ++ [CF.LoanFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 startRate Nothing]
            ,newFactor))
        ([initRow], begFactor)
        (zip4 cfDates ppyRates defRates remainTerms)


instance Asset Installment where
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt st) asOfDay _
    = CF.CashFlowFrame flows 
     where 
        last_pay_date:cf_dates = lastN (rt+1) $ sd:getPaymentDates inst 0
        opmt = divideBI ob ot  
        schedule_balances = scanl (-) ob (replicate ot opmt) -- `debug` (show ot++">>"++show rt)
        current_schedule_bal =  schedule_balances !! (ot - rt)   
        ofee = mulBIR ob (getOriginRate inst)

        factor =  cb / current_schedule_bal 
        cpmt = opmt * factor -- `debug` ("Current B"++show cb++">> schedule bal"++ show current_schedule_bal)
        cfee = ofee * factor 
        orate = getOriginRate inst

        stressed_bal_flow = map (* factor)  $ lastN rt schedule_balances
        prin_flow = replicate rt cpmt 
        int_flow =  replicate rt cfee
        _flows = zipWith10 CF.LoanFlow cf_dates stressed_bal_flow prin_flow int_flow (replicate rt 0.0) (replicate rt 0.0) (replicate rt 0.0) (replicate rt 0.0) (replicate rt orate) (replicate rt Nothing)
        -- (_,flows) = splitByDate _flows asOfDay EqToRight -- `debug` ("5"++show bals++">>"++show pmts)
        flows = cutBy Inc Future asOfDay _flows


  getCurrentBal (Installment _ b _ _ ) = b
  
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _) _ _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _) _ _ _) 
    = case or of
       Fix _ _r -> _r
       Floater _ _ _ _r _ _ _ _ -> _r

  isDefaulted (Installment _ _ _ (Defaulted _)) = True
  isDefaulted (Installment {}) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) extra 
    = genDates sd p (ot+extra)

  getOriginDate (Installment (LoanOriginalInfo _ _ ot p sd _) _ _ _) = sd
  
  getRemainTerms (Installment (LoanOriginalInfo _ _ ot p sd _) _ rt _) = rt

  updateOriginDate (Installment (LoanOriginalInfo ob or ot p sd _type) cb rt st) nd
    = Installment (LoanOriginalInfo ob or ot p nd _type) cb rt st

  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd _) cb rt Current) 
               asOfDay 
               pAssump@(A.InstallmentAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRates
      = (applyHaircut ams (CF.CashFlowFrame futureTxns), historyM)
        where 
          recoveryLag = maybe 0 getRecoveryLag recoveryAssump
          lastPayDate:cfDates = lastN (rt + recoveryLag +1) $ sd:getPaymentDates inst recoveryLag
          
          opmt = divideBI ob ot
          orate = getOriginRate inst
          ofee = mulBIR ob orate
          
          remainTerms = reverse $ replicate recoveryLag 0 ++ [0..rt]  

          scheduleBalances = scanl (-) ob (replicate ot opmt)
          currentScheduleBal = scheduleBalances !! (ot - rt) -- `debug` ("RT->"++show rt)
          currentFactor = divideBB cb currentScheduleBal
          
          ppyRates = Ast.buildPrepayRates (lastPayDate:cfDates) prepayAssump
          defRates = Ast.buildDefaultRates (lastPayDate:cfDates) defaultAssump
          (txns,_) = projectInstallmentFlow (cb,lastPayDate,(opmt,ofee),orate,currentFactor) (cfDates,defRates,ppyRates,remainTerms) 
          (futureTxns,historyM) = CF.cutoffTrs asOfDay (patchLossRecovery txns recoveryAssump)


  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype) cb rt (Defaulted (Just defaultedDate))) 
               asOfDay 
               (_,_,(A.DefaultedRecovery rr lag timing))
               mRates
    = let 
         (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+length timing)
         beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr Nothing | d <- cf_dates1 ]
         recoveries = calcRecoveriesFromDefault cb rr timing
         bals = scanl (-) cb recoveries
         _txns = [  CF.LoanFlow d b 0 0 0 0 r 0 cr Nothing | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
      in 
         (CF.CashFlowFrame $ cutBy Inc Future asOfDay (beforeRecoveryTxn++_txns),Map.empty)
      where 
        cr = getOriginRate inst
  
  projCashflow inst@(Installment _ cb rt (Defaulted Nothing)) asOfDay assumps _
    = (CF.CashFlowFrame $ [CF.LoanFlow asOfDay cb 0 0 0 0 0 0 (getOriginRate inst) Nothing],Map.empty)
        
  splitWith (Installment (LoanOriginalInfo ob or ot p sd _type) cb rt st) rs
    = [ Installment (LoanOriginalInfo (mulBR ob ratio) or ot p sd _type) (mulBR cb ratio) rt st | ratio <- rs ]


