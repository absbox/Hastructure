module UT.LedgerTest(bookTest)
where

import Test.Tasty
import Test.Tasty.HUnit
import Accounts 
import Lib
import Stmt
import Util
import DateUtil
import Types
import Deal
import Deal.DealQuery (queryCompound)
import Deal.DealBase
import Control.Lens hiding (element,Empty)
import Control.Lens.TH
import Data.Map.Lens
import qualified Ledger as LD


import qualified Data.Time as T
import qualified Data.DList as DL 
import qualified Data.Map as Map

bookTest = 
    let 
        leg1 = LD.Ledger "L1" (Debit, 200) Nothing
        leg2 = LD.Ledger "L2" (Debit, 100) Nothing

        btoClear = LD.bookToClear (Credit, 50) (toDate "20220101") leg1
        btoClear' = LD.bookToClear (Credit, 250) (toDate "20220101") leg1
        bySeq = LD.clearLedgersBySeq (Credit, 240) (toDate "20220101") [] [leg1,leg2]
        bySeq' = LD.clearLedgersBySeq (Credit, 350) (toDate "20220101") [] [leg1,leg2]
    in 
        testGroup "Booking Ledger Test"
        [
        testCase "Booking Ledger Test:01" $
            assertEqual "01"
            (Debit, 250)
            (LD.ledgBalance (LD.entryLogByDr (Debit,50) (toDate "20220101") Nothing leg1))
        ,testCase "Booking Ledger Test:02" $
            assertEqual "02"
            (Credit, 50)
            (LD.ledgBalance (LD.entryLogByDr (Credit,250) (toDate "20220101") Nothing leg1)) 
        ,testCase "Booking Ledger Test:03" $
            assertEqual "03"
            (Debit, 150)
            (LD.ledgBalance (LD.entryLogByDr (Credit,50) (toDate "20220101") Nothing leg1)) 
        ,testCase "Booking Ledger Test:04" $
            assertEqual "04"
            (Credit, 250)
            (LD.bookToTarget leg1 (Credit,50))
        ,testCase "Booking Ledger Test:05" $
            assertEqual "05"
            (Debit, 150)
            (LD.bookToTarget leg1 (Debit,350))
        ,testCase "Booking Ledger Test:06" $
            assertEqual "06"
            (Credit, 50)
            (LD.bookToTarget leg1 (Debit,150))
        ,testCase "Booking Ledger Test:07" $
            assertEqual "07"
            ((Debit, 150), 0)
            ((LD.ledgBalance . snd) btoClear, (snd . fst) btoClear)
        ,testCase "Booking Ledger Test:08" $
            assertEqual "08"
            ((Debit, 0), (Credit, 50))
            ((LD.ledgBalance . snd) btoClear',  fst btoClear')
        ,testCase "Booking Ledger Test:09" $
            assertEqual "09"
            [(Debit, 60),(Debit, 0), (Credit, 0)]
            [LD.ledgBalance ((fst bySeq) !! 1), LD.ledgBalance ((fst bySeq) !! 0), snd bySeq]
        ,testCase "Booking Ledger Test:10" $
            assertEqual "10"
            [(Debit, 0),(Debit, 0), (Credit, 50)]
            [LD.ledgBalance ((fst bySeq') !! 1), LD.ledgBalance ((fst bySeq') !! 0), snd bySeq']
        ]
