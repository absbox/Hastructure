{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Expense (Fee(..),FeeType(..) ,buildFeeAccrueAction
               ,feeNameLens,feeDueLens,feeTypeLens,feeStmtLens,reAccruableFeeType)
  where

import Lib(Period,paySeqLiabilities,Dates
           ,Amount,Balance,Date,Rate,Ts(..))
import Stmt(appendStmt,Statement,TxnComment(..))
import Data.Traversable
import Control.Monad
import Language.Haskell.TH

import qualified Data.Text
import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.DList as DL
import GHC.Generics

import Data.Fixed
import Types
import Util
import DateUtil
import qualified Stmt as S
import qualified InterestRate as IR

import Control.Lens
import Debug.Trace
debug = flip trace

type FormulaRate = DealStats

data FeeType = AnnualRateFee DealStats FormulaRate                       -- ^ annulized fee with a referece
             | PctFee DealStats FormulaRate                              -- ^ fee base on percentage 
             | FixFee Balance                                            -- ^ one-off fee
             | RecurFee DatePattern Balance                              -- ^ fee occur every date pattern
             | NumFee DatePattern DealStats Amount                       -- ^ fee based on an integer number
             | AmtByTbl DatePattern DealStats (Table Balance Balance)    -- ^ lookup query value in a table
             | TargetBalanceFee DealStats DealStats                      -- ^ fee due amount = max( 0, (ds1 - ds2))
             | FeeFlow Ts                                                -- ^ a time series based fee 
             | FeeFlowByPoolPeriod (PerCurve Balance)                    -- ^ a pool index series based fee
             | FeeFlowByBondPeriod (PerCurve Balance)                    -- ^ a bond index series based fee
             | ByCollectPeriod Amount                                    -- ^ fix amount per collection period
             deriving (Show, Eq, Generic, Ord)

data Fee = Fee {
  feeName :: String              -- ^ fee name
  ,feeType :: FeeType            -- ^ fee type
  ,feeStart :: Date              -- ^ when fee become effective
  ,feeDue :: Balance             -- ^ outstanding due amount fee
  ,feeDueDate :: Maybe Date      -- ^ the date when due amount was calculated
  ,feeArrears :: Balance         -- ^ not paid oustanding amout
  ,feeLastPaidDay :: Maybe Date  -- ^ last paid date
  ,feeStmt :: Maybe Statement    -- ^ transaction history
} deriving (Show, Ord, Eq, Generic)

-- ^ a predicate identify a fee whether can be accrued multiple times regardless of model definition
reAccruableFeeType :: FeeType -> Bool
reAccruableFeeType (RecurFee _ _) = False
reAccruableFeeType (NumFee _ _ _) = False
reAccruableFeeType (AmtByTbl _ _ _) = False
reAccruableFeeType (PctFee _ _) = False
reAccruableFeeType _ = True


instance Payable Fee where

  pay d DueFee amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    | amt > fd = Left $ "Payment amount is greater than due amount of fee:" ++ fn
    | otherwise = 
        let 
          dueRemain = fd - amt
          newStmt = appendStmt (ExpTxn d dueRemain amt fa (PayFee fn)) fstmt
        in 
          return $ f {feeLastPaidDay = Just d ,feeDue = dueRemain ,feeStmt = newStmt}

  pay d DueArrears amt f@(Fee fn ft fs fd fdDay fa flpd fstmt)
    | amt > fa = Left $ "Payment amount is greater than arrears amount of fee:" ++ fn
    | otherwise = 
        let 
          arrearRemain = fa - amt
          newStmt = appendStmt (ExpTxn d fd amt arrearRemain (PayFee fn)) fstmt
        in 
          return $ f {feeLastPaidDay = Just d ,feeArrears = arrearRemain ,feeStmt = newStmt}


  pay d (DueTotalOf []) amt f = return f 
  pay d (DueTotalOf (dt:dts)) amt f 
    = pay d (DueTotalOf dts) remainAmt =<< pay d dt amtToPay f
      where 
        dueBal = getDueBal d (Just dt) f
        (amtToPay, remainAmt) = (min dueBal amt, amt - min dueBal amt)
  
  pay d DueResidual amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
    let 
      newStmt = appendStmt (ExpTxn d fd amt fa (PayFee fn)) fstmt  
    in 
      return $ f {feeLastPaidDay = Just d ,feeStmt = newStmt}

  pay d dt _ f = Left $ "Unsupported due type for pay fee"++ feeName f ++ show dt
  
  
  getDueBal d t fee@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    = case t of 
        Nothing -> fa + fd
        Just DueFee -> fd
        Just DueArrears -> fa
        Just (DueTotalOf []) -> 0 
        Just (DueTotalOf (dt:dts)) -> getDueBal d (Just dt) fee + getDueBal d (Just (DueTotalOf dts)) fee 

  writeOff d DueFee amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    | amt > fd = Left $ "Write off amount is greater than due amount of fee:" ++ fn 
    | otherwise = 
      let 
        newStmt = appendStmt (ExpTxn d (fd - amt) 0 fa (WriteOff fn amt)) fstmt
      in 
        return (Fee fn ft fs (fd - amt) fdDay fa flpd newStmt)

  writeOff d DueArrears amt f@(Fee fn ft fs fd fdDay fa flpd fstmt)
    | amt > fa = Left $ "Write off amount is greater than arrears amount of fee:" ++ fn
    | otherwise = 
        let 
          newStmt = appendStmt (ExpTxn d fd 0 (fa - amt) (WriteOff fn amt)) fstmt
        in 
          return (Fee fn ft fs fd fdDay (fa - amt) flpd newStmt)

  writeOff d (DueTotalOf []) amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    | amt /= 0 = Left $ "Write off amount is not zero for empty due total of fee:" ++ fn
    | otherwise = return f
  
  writeOff d (DueTotalOf (dt:dts)) amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    = let 
        dueBal = getDueBal d (Just dt) f
        (amtToWriteOff, remainAmt) = (min dueBal amt, amt - min dueBal amt)
      in
        writeOff d (DueTotalOf dts) remainAmt =<< writeOff d dt amtToWriteOff f

  writeOff d dt _ _ = Left $ "Unsupported due type for write off fee"++ show dt

-- | build accure dates for a fee
buildFeeAccrueAction :: [Fee] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildFeeAccrueAction [] ed r = r
buildFeeAccrueAction (fee@Fee{feeName = fn }:fees) ed r
  = buildFeeAccrueAction fees ed ((fn, getAccrualDates ed fee):r)

instance S.QueryByComment Fee where 
    queryStmt Fee{feeStmt = Nothing} tc = []
    queryStmt Fee{feeStmt = Just (S.Statement txns)} tc
      = filter (\x -> S.getTxnComment x == tc) (DL.toList txns)

instance Liable Fee where 
  isPaidOff f@Fee{feeDue=bal,feeArrears=fa}
    | bal==0 && fa==0 = True 
    | otherwise = False
    
  getOutstandingAmount Fee{feeDue=bal,feeArrears=fa} = bal + fa

instance Accruable Fee where
  getAccrualDates ed Fee{feeStart=fs,feeDueDate=fdDay,feeType=RecurFee dp _}
    = projDatesByPattern dp fs ed
  getAccrualDates ed Fee{feeStart=fs,feeDueDate=fdDay,feeType=FixFee _}
    = [fs]
  getAccrualDates ed Fee{feeStart=fs,feeDueDate=fdDay,feeType=FeeFlow ts}
    = getTsDates ts
  getAccrualDates ed Fee{feeStart=fs,feeDueDate=fdDay,feeType=NumFee dp _ _}
    = projDatesByPattern dp fs ed
  getAccrualDates ed Fee{feeStart=fs,feeDueDate=fdDay,feeType=AmtByTbl dp _ _}
    = projDatesByPattern dp fs ed
  getAccrualDates ed Fee{}
    = []

instance IR.UseRate Fee where
  isAdjustableRate x = False
  getIndex x = Nothing 

makeLensesFor [("feeName","feeNameLens"),("feeType","feeTypeLens") ,("feeDue","feeDueLens") 
              ,("feeDueDate","feeDueDateLens") ,("feeStmt","feeStmtLens")] ''Fee

$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
