{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Expense (Fee(..),FeeType(..) ,buildFeeAccrueAction
               ,feeNameLens,feeDueLens,feeTypeLens,feeStmtLens)
  where

import Lib(Period,paySeqLiabilities,Dates
           ,Amount,Balance,Date,Rate,Ts(..))
import Stmt(appendStmt,Statement,TxnComment(..))
import Data.Traversable
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


instance Payable Fee where
  pay d (DueTotalOf [DueArrears,DueFee]) amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) = 
    let 
      [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd]
      paid = fa + fd - arrearRemain - dueRemain 
      newStmt = appendStmt (ExpTxn d dueRemain paid arrearRemain (PayFee fn)) fstmt
    in 
      f {feeLastPaidDay = Just d ,feeDue = dueRemain ,feeArrears = arrearRemain ,feeStmt = newStmt}
  
  pay d DueResidual amt f@(Fee fn ft fs fd fdDay fa flpd fstmt) =
    let 
      [(r0,arrearRemain),(r1,dueRemain)] = paySeqLiabilities amt [fa,fd] 
      newStmt = appendStmt (ExpTxn d dueRemain amt arrearRemain (PayFee fn)) fstmt  
    in 
      f {feeLastPaidDay = Just d ,feeDue = dueRemain ,feeArrears = arrearRemain ,feeStmt = newStmt}

  
  getDueBal d t fee@(Fee fn ft fs fd fdDay fa flpd fstmt) 
    = case t of 
	Nothing -> fa + fd
	Just DueFee -> fd
	Just DueArrears -> fa

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

makeLensesFor [("feeName","feeNameLens"),("feeType","feeTypeLens") ,("feeDue","feeDueLens") ,("feeDueDate","feeDueDateLens") ,("feeStmt","feeStmtLens")] ''Fee

$(deriveJSON defaultOptions ''FeeType)
$(deriveJSON defaultOptions ''Fee)
