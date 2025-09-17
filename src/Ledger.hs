{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ledger (Ledger(..),LedgerName,queryGap,clearLedgersBySeq
              ,entryLogByDr,bookToTarget,bookToClear)
    where
import qualified Data.Time as T
import Stmt 
import Types
import Lib
import Util
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.DList as DL
import GHC.Generics

import Control.Lens hiding (element)

import Control.Lens.TH
import Debug.Trace
debug = flip trace


type LedgerName = String
type EntryAmount = (BookDirection, Amount)
type LedgerBalance = (BookDirection, Amount)

rev :: BookDirection -> BookDirection
rev Credit = Debit
rev Debit  = Credit


data Ledger = Ledger {
    ledgName :: String                              -- ^ ledger account name
    ,ledgBalance :: LedgerBalance                   -- ^ current balance of ledger
    ,ledgStmt :: Maybe Statement                    -- ^ ledger transaction history
} deriving (Show, Generic,Ord, Eq)


entryLogByDr :: EntryAmount -> Date -> Maybe TxnComment -> Ledger -> Ledger
entryLogByDr (dr, amt) d mCmt ledg@Ledger{ledgStmt = mStmt, ledgBalance = (curDr, curBal)} 
  = let 
      cmt = case mCmt of 
              Nothing -> TxnDirection dr
              Just c -> if hasTxnDirection c then c else TxnComments [TxnDirection dr,c]
      (newBalAmt, newDr) = case (curDr, dr, amt > curBal ) of 
                            (Debit, Debit, _ ) -> (curBal + amt, Debit)
                            (Credit, Credit, _) -> (curBal + amt, Credit)
                            (Debit, Credit, True ) -> (amt - curBal, Credit)
                            (Debit, Credit, False ) -> (curBal - amt, Debit)
                            (Credit, Debit, True ) -> (amt - curBal, Debit )
                            (Credit, Debit, False ) -> (curBal - amt, Credit)

      txn = EntryTxn d (newDr, newBalAmt) (dr, amt) cmt
    in 
      ledg { ledgStmt = appendStmt txn mStmt ,ledgBalance = (newDr, newBalAmt) }

hasTxnDirection :: TxnComment -> Bool
hasTxnDirection (TxnDirection _) = True
hasTxnDirection (TxnComments txns) = any hasTxnDirection txns
hasTxnDirection _ = False


-- ^ backout book txn from a target amount
bookToTarget :: Ledger -> (BookDirection, Amount) -> (BookDirection, Amount)
bookToTarget Ledger{ledgBalance = (curDr,curBal) } (targetDr, targetBal) 
  = let 
      a = 1 
    in 
      case (curDr == targetDr , targetBal >= curBal) of 
        (True, True) ->
          (curDr, targetBal - curBal)
        (True, False) ->
          (rev curDr, curBal - targetBal)
        (False, _) -> 
          (targetDr, targetBal + curBal)

bookToClear :: EntryAmount -> Date -> Ledger -> (EntryAmount, Ledger)
bookToClear (_,0) d ledg = ((Credit,0),ledg )
bookToClear (dr,amt) d ledg@Ledger{ledgBalance = (curDr, curBal)} 
  | curDr == dr = ((dr, amt), ledg)
  | otherwise 
    = let 
        bookAmt 
          | amt > curBal = curBal
          | otherwise = amt
        remainAmt = amt - bookAmt
        newLedger = entryLogByDr (dr, bookAmt) d (Just (TxnDirection dr)) ledg
      in 
        ((dr,remainAmt), newLedger)

-- ^ return ledger's bookable amount (for netting off to zero ) with direction input
queryGap :: Ledger -> LedgerBalance
queryGap Ledger{ledgBalance = (Credit, bal)} = (Debit, bal)      -- credit balance can be booked by debit
queryGap Ledger{ledgBalance = (Debit, bal)} = (Credit, bal)      -- debit balance can be booked by credit

-- ^ book an amount to a list of ledgers by sequence
clearLedgersBySeq :: EntryAmount -> Date -> [Ledger] -> [Ledger] -> ([Ledger],EntryAmount)
clearLedgersBySeq (dr,0) d rs unAllocLedgers = ( (reverse rs)++unAllocLedgers,(dr,0))
clearLedgersBySeq (dr,amtToAlloc) d  rs [] = (reverse rs,(dr,amtToAlloc))
clearLedgersBySeq (dr,amtToAlloc) d  rs (ledger:ledgers)  
  = let 
      ((newDr, remainAmt), newLedger) = bookToClear (dr,amtToAlloc) d ledger 
    in 
      clearLedgersBySeq (newDr,remainAmt) d (newLedger:rs) ledgers

instance QueryByComment Ledger where 
    queryStmt (Ledger _ _ Nothing) tc = []
    queryStmt (Ledger _ _ (Just (Statement txns))) tc
      = filter (\x -> getTxnComment x == tc) (DL.toList txns)

    queryTxnAmt a tc = sum $ map getTxnAmt $ queryStmt a tc

makeLensesFor [("ledgName","ledgNameLens"),("ledgBalance","ledgBalLens"),("ledgStmt","ledgStmtLens")] ''Ledger


$(deriveJSON defaultOptions ''Ledger)