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
import qualified Data.DList as DL
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
import Control.Lens hiding (element)
import Control.Lens.TH
debug = flip trace


projectInstallmentFlow :: (Balance,Date,(Balance,Balance),IRate,Rational,AmortPlan,Int) -> (Dates, [DefaultRate], [PrepaymentRate], [Int]) -> (DL.DList CF.TsRow, Balance ,Rational)
projectInstallmentFlow (startBal, lastPaidDate, (originRepay,originInt), startRate,begFactor,pt,ot) (cfDates, defRates, ppyRates, remainTerms)
  = let 
      initRow = CF.LoanFlow lastPaidDate startBal 0.0 0.0 0.0 0.0 0.0 0.0 startRate Nothing
      calcPrin _rt _bal _opmt _factor = case _rt of
                                          1 -> _bal
                                          0 -> 0
                                          _ -> mulBR _opmt _factor
    in
      foldl
        (\(acc,begBal,factor) (pDate, ppyRate, defRate, rt) -> 
          let 
            -- begBal = view CF.tsRowBalance (last acc)
            newDefault = mulBR begBal defRate
            newPrepay = mulBR (begBal - newDefault) ppyRate
            intBal = begBal - newDefault - newPrepay
            newFactor = factor * (1-defRate) * (1- ppyRate)
            newInt = case pt of 
                      F_P -> if rt > 0 then 
                               mulBR originInt newFactor
                             else
                               0
                      PO_FirstN n -> if (ot-rt) >= n then
                                          mulBR originInt newFactor
                                        else
                                          0 
            newPrin = calcPrin rt intBal originRepay newFactor
            endBal = intBal - newPrin
          in 
            (DL.snoc acc (CF.LoanFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 startRate Nothing)
            ,endBal
            ,newFactor))
        (DL.singleton initRow, startBal, begFactor)
        (zip4 cfDates ppyRates defRates remainTerms)


instance Asset Installment where
  calcCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype _) cb rt st) asOfDay _
    = return $ CF.CashFlowFrame (begBal,asOfDay,Nothing) flows 
     where 
        lastPayDate:cf_dates = lastN (rt+1) $ sd:getPaymentDates inst 0
        opmt = divideBI ob ot  
        schedule_balances = scanl (-) ob (replicate ot opmt) 
        current_schedule_bal =  schedule_balances !! (ot - rt)   
        ofee = mulBIR ob (getOriginRate inst)

        factor =  cb / current_schedule_bal 
        cpmt = opmt * factor 
        cfee = ofee * factor 
        orate = getOriginRate inst

        stressed_bal_flow = map (* factor)  $ lastN rt schedule_balances
        prin_flow = replicate rt cpmt 
        int_flow =  case ptype of 
                      F_P -> replicate rt cfee
                      PO_FirstN n -> lastN rt $ replicate n 0.0 ++ replicate (ot-n) cfee 
        _flows = let 
                  _rt = succ rt 
                 in 
                  zipWith10 CF.LoanFlow (lastPayDate:cf_dates) (cb:stressed_bal_flow) (0:prin_flow) (0:int_flow) 
                                        (replicate _rt 0.0) (replicate _rt 0.0) (replicate _rt 0.0) (replicate _rt 0.0) 
                                        (replicate _rt orate) (replicate _rt Nothing)
                                
        flows = cutBy Inc Future asOfDay _flows
        begBal = CF.buildBegBal flows


  getCurrentBal (Installment _ b _ _ ) = b
  getOriginInfo (Installment oi _ _ _) = oi
  getOriginBal (Installment (LoanOriginalInfo ob _ _ _ _ _ _) _ _ _) = ob

  getOriginRate (Installment (LoanOriginalInfo _ or _ _ _ _ _) _ _ _) 
    = case or of
       Fix _ _r -> _r
       Floater _ _ _ _r _ _ _ _ -> _r

  isDefaulted (Installment _ _ _ (Defaulted _)) = True
  isDefaulted (Installment {}) = False

  getPaymentDates (Installment (LoanOriginalInfo _ _ ot p sd _ _) _ _ _) extra 
    = genDates sd p (ot+extra)

  getOriginDate (Installment (LoanOriginalInfo _ _ ot p sd _ _) _ _ _) = sd
  
  getRemainTerms (Installment (LoanOriginalInfo _ _ ot p sd _ _) _ rt _) = rt

  updateOriginDate (Installment (LoanOriginalInfo ob or ot p sd _type _obligor) cb rt st) nd
    = Installment (LoanOriginalInfo ob or ot p nd _type _obligor) cb rt st

  resetToOrig (Installment (LoanOriginalInfo ob or ot p sd _type _obligor) cb rt st)
    = Installment (LoanOriginalInfo ob or ot p sd _type _obligor) ob ot st

  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd pt _) cb rt Current) 
               asOfDay 
               pAssump@(A.InstallmentAssump defaultAssump prepayAssump recoveryAssump ams,_,_)
               mRates
      = let
          recoveryLag = maybe 0 getRecoveryLag recoveryAssump
          lastPayDate:cfDates = lastN (rt + recoveryLag +1) $ sd:getPaymentDates inst recoveryLag
          
          opmt = divideBI ob ot
          orate = getOriginRate inst
          ofee = mulBIR ob orate
          
          remainTerms = reverse $ replicate recoveryLag 0 ++ [0..rt]

          scheduleBalances = scanl (-) ob (replicate ot opmt)
          currentScheduleBal = scheduleBalances !! (ot - rt) -- `debug` ("RT->"++show rt)
          currentFactor = divideBB cb currentScheduleBal
        in  
          do 
            ppyRates <- Ast.buildPrepayRates inst (lastPayDate:cfDates) prepayAssump
            defRates <- Ast.buildDefaultRates inst (lastPayDate:cfDates) defaultAssump
            let (txns,_,_) = projectInstallmentFlow (cb,lastPayDate,(opmt,ofee),orate,currentFactor,pt,ot) (cfDates,defRates,ppyRates,remainTerms) 
            txns' <- patchLossRecovery (DL.toList txns) recoveryAssump
            let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns'
            let begBal = CF.buildBegBal futureTxns
            return (applyHaircut ams (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns), historyM)

  -- ^ project with defaulted at a date
  projCashflow inst@(Installment (LoanOriginalInfo ob or ot p sd ptype _) cb rt (Defaulted (Just defaultedDate))) 
               asOfDay 
               (_,_,A.DefaultedRecovery rr lag timing)
               mRates
    = let 
         (cf_dates1,cf_dates2) = splitAt lag $ genDates defaultedDate p (lag+length timing)
         beforeRecoveryTxn = [  CF.LoanFlow d cb 0 0 0 0 0 0 cr Nothing | d <- cf_dates1 ]
         recoveries = calcRecoveriesFromDefault cb rr timing
         bals = scanl (-) cb recoveries
         _txns = [  CF.LoanFlow d b 0 0 0 0 r 0 cr Nothing | (b,d,r) <- zip3 bals cf_dates2 recoveries ]
         futureTxns = cutBy Inc Future asOfDay $ beforeRecoveryTxn++_txns
         begBal = CF.buildBegBal futureTxns
      in 
         return (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns ,Map.empty)
      where 
        cr = getOriginRate inst
  
  -- ^ project cashflow with defaulted status
  projCashflow inst@(Installment _ cb rt (Defaulted Nothing)) asOfDay assumps _
    = return (CF.CashFlowFrame (cb, asOfDay, Nothing) $ [CF.LoanFlow asOfDay cb 0 0 0 0 0 0 (getOriginRate inst) Nothing],Map.empty)

  projCashflow a b c d = Left $ "Failed to match when proj mortgage with assumption >>" ++ show a ++ show b ++ show c ++ show d
  
  splitWith (Installment (LoanOriginalInfo ob or ot p sd _type _obligor) cb rt st) rs
    = [ Installment (LoanOriginalInfo (mulBR ob ratio) or ot p sd _type _obligor) (mulBR cb ratio) rt st | ratio <- rs ]


