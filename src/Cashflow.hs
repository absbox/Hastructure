{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DataKinds       #-}

module Cashflow (CashFlowFrame(..),Principals,Interests,Amount
                ,combine,mergePoolCf,sumTsCF,tsSetLoss,tsSetRecovery
                ,sizeCashFlowFrame,aggTsByDates,emptyCashFlowFrame
                ,mflowInterest,mflowPrincipal,mflowRecovery,mflowPrepayment
                ,mflowRental,mflowRate,sumPoolFlow,splitTrs,aggregateTsByDate
                ,mflowDefault,mflowLoss
                ,getDatesCashFlowFrame
                ,lookupSource,lookupSourceM,combineTss
                ,mflowBegBalance,tsDefaultBal
                ,mflowBorrowerNum,mflowPrepaymentPenalty,tsRowBalance
                ,emptyTsRow,mflowAmortAmount
                ,tsTotalCash, setPrepaymentPenalty, setPrepaymentPenaltyFlow
                ,getDate,getTxnLatestAsOf,totalPrincipal
                ,mflowWeightAverageBalance,tsDate
                ,totalLoss,totalDefault,totalRecovery,firstDate
                ,shiftCfToStartDate,cfInsertHead,buildBegTsRow,insertBegTsRow
                ,tsCumDefaultBal,tsCumDelinqBal,tsCumLossBal,tsCumRecoveriesBal
                ,TsRow(..),cfAt,cutoffTrs,patchCumulative,extendTxns,dropTailEmptyTxns
                ,cashflowTxn,clawbackInt,scaleTsRow,mflowFeePaid, currentCumulativeStat, patchCumulativeAtInit
                ,mergeCf,buildStartTsRow
                ,txnCumulativeStats,consolidateCashFlow, cfBeginStatus, getBegBalCashFlowFrame
                ,splitCashFlowFrameByDate, mergePoolCf2, buildBegBal, extendCashFlow, patchBalance
                ,splitPoolCashflowByDate
                ,getAllDatesCashFlowFrame,splitCf, cutoffCashflow
                ,AssetCashflow,PoolCashflow
                ,emptyCashflow,isEmptyRow2,appendMCashFlow
                ) where

import Data.Time (Day)
import Data.Fixed
import Lib (weightedBy,toDate,getIntervalFactors,daysBetween,paySeqLiabilitiesAmt)
import Util (mulBR,mulBInt,mulIR,lastOf)
import DateUtil ( splitByDate )
import Types
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.List as L
import Data.Maybe 

import Data.Aeson hiding (json)
import Language.Haskell.TH
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Text.Printf

import Debug.Trace
import qualified Control.Lens as Map
import Control.Applicative (liftA2)
import Data.OpenApi (HasPatch(patch), HasXml (xml))
import Control.DeepSeq (NFData,rnf)
import Data.Text.Internal.Encoding.Fusion (streamUtf16BE)

import qualified Text.Tabular as TT
import qualified Text.Tabular.AsciiArt as A
import Control.Lens hiding (element)
import Control.Lens.TH

debug = flip trace

type Delinquent = Balance
type Amounts = [Float]
type Principals = [Principal]
type Interests = [Interest]
type Prepayments = [Prepayment]
type Recoveries = [Recovery]
type Rates = [Rate]

type CumulativeStat = (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)
type AssetCashflow = CashFlowFrame
type PoolCashflow = (AssetCashflow, Maybe [AssetCashflow])
emptyCashflow = CashFlowFrame (0,epocDate,Nothing) []


instance Monoid CashFlowFrame where
  mempty = emptyCashflow

instance Semigroup CashFlowFrame where
  CashFlowFrame (begBal1, begDate1, mAccInt1) ts1 <> CashFlowFrame (begBal2, begDate2, mAccInt2) ts2 
    = CashFlowFrame (begBal1,begDate1,mAccInt1) (ts1 <> ts2)

opStats :: (Balance -> Balance -> Balance) -> Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
opStats op (Just (a1,b1,c1,d1,e1,f1)) (Just (a2,b2,c3,d2,e2,f2)) = Just (op a1 a2,op b1 b2,op c1 c3,op d1 d2,op e1 e2,op f1 f2)
opStats op Nothing Nothing = Nothing
opStats op (Just a) Nothing = Just a
opStats op Nothing (Just a) = Just a

sumStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- sumStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (a1+a2,b1+b2,c1+c3,d1+d2,e1+e2,f1+f2)
sumStats s1 s2 = opStats (+) s1 s2

subStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- subStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (a1-a2,b1-b2,c1-c3,d1-d2,e1-e2,f1-f2)
subStats s1 s2 = opStats (-) s1 s2

maxStats :: Maybe CumulativeStat -> Maybe CumulativeStat -> Maybe CumulativeStat
-- maxStats (a1,b1,c1,d1,e1,f1) (a2,b2,c3,d2,e2,f2) = (max a1 a2,max b1 b2,max c1 c3,max d1 d2,max e1 e2,max f1 f2)
maxStats s1 s2 = opStats max s1 s2

splitStats :: Rational -> CumulativeStat -> CumulativeStat
splitStats r st1@(a1,b1,c1,d1,e1,f1) = ((`mulBR` r) a1,(`mulBR` r) b1,(`mulBR` r) c1,(`mulBR` r) d1,(`mulBR` r) e1,(`mulBR` r) f1)

type Depreciation = Balance
type NewDepreciation = Balance 
type AccuredFee = Balance
type FeePaid = Balance


data TsRow = CashFlow Date Amount
           | BondFlow Date Balance Principal Interest
           | MortgageFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe BorrowerNum) (Maybe PrepaymentPenalty) (Maybe CumulativeStat)
           | MortgageDelinqFlow Date Balance Principal Interest Prepayment Delinquent Default Recovery Loss IRate (Maybe BorrowerNum) (Maybe PrepaymentPenalty) (Maybe CumulativeStat)
           | LoanFlow Date Balance Principal Interest Prepayment Default Recovery Loss IRate (Maybe CumulativeStat)
           | LeaseFlow Date Balance Rental Default
           | FixedFlow Date Balance NewDepreciation Depreciation Balance Balance -- unit cash 
           | ReceivableFlow Date Balance AccuredFee Principal FeePaid Default Recovery Loss (Maybe CumulativeStat) 
           deriving(Show,Eq,Ord,Generic,NFData)

instance Semigroup TsRow where 
  CashFlow d1 a1 <> (CashFlow d2 a2) = CashFlow (max d1 d2) (a1 + a2)
  BondFlow d1 b1 p1 i1 <> (BondFlow d2 b2 p2 i2) = BondFlow (max d1 d2) (b1 + b2) (p1 + p2) (i1 + i2)
  MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1 <> MortgageFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2
    = MortgageFlow (max d1 d2) (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1 + los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2])))  (liftA2 (+) mbn1 mbn2)   (liftA2 (+) pn1 pn2)  (sumStats st1 st2)
  MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1 <> MortgageDelinqFlow d2 b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2
    = MortgageDelinqFlow (max d1 d2) (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (delinq1 + delinq2) (def1 + def2) (rec1 + rec2) (los1 + los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) (liftA2 (+) mbn1 mbn2) (liftA2 (+) pn1 pn2) (sumStats st1 st2)
  LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1 <> LoanFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 st2
    = LoanFlow (max d1 d2) (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1 + los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) (sumStats st1 st2)
  LeaseFlow d1 b1 r1 def1 <> LeaseFlow d2 b2 r2 def2
    = LeaseFlow (max d1 d2) (b1 + b2) (r1 + r2) (def1 + def2)
  FixedFlow d1 b1 ndep1 dep1 c1 a1 <> FixedFlow d2 b2 ndep2 dep2 c2 a2 
    = FixedFlow (max d1 d2) (b1 + b2) (ndep1 + ndep2) (dep1 + dep2) (c1 + c2) (a1 + a2)
  ReceivableFlow d1 b1 af1 p1 fp1 def1 rec1 los1 st1 <> ReceivableFlow d2 b2 af2 p2 fp2 def2 rec2 los2 st2
    = ReceivableFlow (max d1 d2) (b1 + b2) (af1 + af2) (p1 + p2) (fp1 + fp2) (def1 + def2) (rec1 + rec2) (los1 + los2) (sumStats st1 st2)
  a <> b = error $ "TsRow Semigroup not supported "++show a++" "++show b


instance TimeSeries TsRow where 
    getDate (CashFlow x _) = x
    getDate (BondFlow x  _ _ _) = x
    getDate (MortgageFlow x _ _ _ _ _ _ _ _ _ _ _) = x
    getDate (MortgageDelinqFlow x _ _ _ _ _ _ _ _ _ _ _ _) = x
    getDate (LoanFlow x _ _ _ _ _ _ _ _ _) = x
    getDate (LeaseFlow x _ _ _) = x
    getDate (FixedFlow x _ _ _ _ _ ) = x
    getDate (ReceivableFlow x _ _ _ _ _ _ _ _) = x


scaleTsRow :: Rational -> TsRow -> TsRow
scaleTsRow r (CashFlow d a) = CashFlow d (fromRational r * a)
scaleTsRow r (BondFlow d b p i) = BondFlow d (fromRational r * b) (fromRational r * p) (fromRational r * i)
scaleTsRow r (MortgageFlow d b p i prep def rec los rat mbn pp st) 
  = MortgageFlow d 
     (fromRational r * b) 
     (fromRational r * p) 
     (fromRational r * i) 
     (fromRational r * prep) 
     (fromRational r * def) 
     (fromRational r * rec) 
     (fromRational r * los) 
     rat 
     mbn 
     pp 
     (splitStats r <$> st)
scaleTsRow r (MortgageDelinqFlow d b p i prep delinq def rec los rat mbn pp st) 
  = MortgageDelinqFlow d 
      (fromRational r * b)
      (fromRational r * p)
      (fromRational r * i)
      (fromRational r * prep)
      (fromRational r * delinq)
      (fromRational r * def) 
      (fromRational r * rec) 
      (fromRational r * los) 
      rat 
      mbn 
      pp
      (splitStats r <$> st)
scaleTsRow r (LoanFlow d b p i prep def rec los rat st) 
  = LoanFlow d (fromRational r * b) (fromRational r * p) (fromRational r * i) (fromRational r * prep) (fromRational r * def) (fromRational r * rec) (fromRational r * los) rat ((splitStats r) <$> st)
scaleTsRow r (LeaseFlow d b rental def) = LeaseFlow d (fromRational r * b) (fromRational r * rental) (fromRational r * def)
scaleTsRow r (FixedFlow d b ndep dep c a) = FixedFlow d (fromRational r * b) (fromRational r * ndep) (fromRational r * dep) (fromRational r * c) (fromRational r * a)
scaleTsRow r (ReceivableFlow d b af p fp def rec los st) = ReceivableFlow d (fromRational r * b) (fromRational r * af) (fromRational r * p) (fromRational r * fp) (fromRational r * def) (fromRational r * rec) (fromRational r * los) ((splitStats r) <$> st)


type BeginBalance = Balance
type AccuredInterest = Maybe Balance
type BeginDate = Date
type BeginStatus = (BeginBalance, BeginDate, AccuredInterest)

data CashFlowFrame = CashFlowFrame BeginStatus [TsRow]
                   | DummyCF
                   deriving (Eq,Generic,Ord)

cfBeginStatus :: Lens' CashFlowFrame BeginStatus
cfBeginStatus = lens getter setter
  where 
    getter (CashFlowFrame st _) = st
    setter (CashFlowFrame _ tsRows) st = CashFlowFrame st tsRows


instance Show CashFlowFrame where
  show (CashFlowFrame st []) = "Empty CashflowFrame"++ show st
  -- show (CashFlowFrame st txns) = concat $ L.intersperse "\n" [ show txn | txn <- txns ]
  show (CashFlowFrame st txns) = 
    let 
        ds = [ show d | d <- getDates txns]
        rowHeader = [TT.Header h | h <- ds ]
        getCs (CashFlow {}) = ["Amount"]
        getCs (BondFlow {}) = ["Balance", "Principal", "Interest"]
        getCs (MortgageFlow {}) = ["Balance", "Principal", "Interest", "Prepayment", "Default", "Recovery", "Loss", "IRate", "BorrowerNum", "PrepaymentPenalty", "CumulativeStat"]
        getCs (MortgageDelinqFlow {}) = [ "Balance", "Principal", "Interest", "Prepayment", "Delinquent", "Default", "Recovery", "Loss", "IRate", "BorrowerNum", "PrepaymentPenalty", "CumulativeStat"]
        getCs (LoanFlow {}) = ["Balance", "Principal", "Interest", "Prepayment", "Default", "Recovery", "Loss", "IRate", "CumulativeStat"]
        getCs (LeaseFlow {}) = [ "Balance", "Rental", "Default"]
        getCs (FixedFlow {}) = [ "Balance", "NewDepreciation", "Depreciation", "Balance", "Amount"]
        getCs (ReceivableFlow {}) = [ "Balance", "AccuredFee", "Principal", "FeePaid", "Default", "Recovery", "Loss", "CumulativeStat"]
        colHeader = [TT.Header c | c <- getCs (head txns) ]
        getRs (CashFlow d a) = [show a]
        getRs (BondFlow d b p i) = [ show b, show p, show i]
        getRs (MortgageFlow d b p i prep def rec los rat mbn pp st) = [ show b, show p, show i, show prep, show def, show rec, show los, show rat, show mbn, show pp, show st]
        getRs (MortgageDelinqFlow d b p i prep delinq def rec los rat mbn pp st) = [ show b, show p, show i, show prep, show delinq, show def, show rec, show los, show rat, show mbn, show pp, show st]
        getRs (LoanFlow d b p i prep def rec los rat st) = [ show b, show p, show i, show prep, show def, show rec, show los, show rat, show st]
        getRs (LeaseFlow d b r def) = [ show b, show r, show def]
        getRs (FixedFlow d b ndep dep c a) = [ show b, show ndep, show dep, show c, show a]
        getRs (ReceivableFlow d b af p fp def rec los st) = [ show b, show af, show p, show fp, show def, show rec, show los, show st]
        values = [ getRs txn  | txn <- txns ]
        tbl = TT.Table (TT.Group TT.SingleLine rowHeader) (TT.Group TT.SingleLine colHeader) values
    in 
        show st <> "\n" <> A.render id id id tbl

instance NFData CashFlowFrame where 
  rnf (CashFlowFrame st txns) = rnf st `seq` rnf txns

sizeCashFlowFrame :: CashFlowFrame -> Int
sizeCashFlowFrame (CashFlowFrame _ ts) = length ts

emptyCashFlowFrame :: CashFlowFrame -> Bool 
emptyCashFlowFrame (CashFlowFrame _ []) = True
emptyCashFlowFrame (CashFlowFrame _ _) = False

getDatesCashFlowFrame :: CashFlowFrame -> [Date]
getDatesCashFlowFrame (CashFlowFrame _ ts) = getDates ts

getAllDatesCashFlowFrame :: CashFlowFrame -> [Date]
getAllDatesCashFlowFrame (CashFlowFrame (_,d,_) ts) = d : getDates ts

getBegBalCashFlowFrame :: CashFlowFrame -> Balance
getBegBalCashFlowFrame (CashFlowFrame _ []) = 0
getBegBalCashFlowFrame (CashFlowFrame _ (cf:cfs)) = mflowBegBalance cf

cfAt :: CashFlowFrame -> Int -> Maybe TsRow
cfAt (CashFlowFrame _ trs) idx 
  | (idx < 0) || (idx >= length trs) = Nothing
  | otherwise = Just (trs!!idx)

cfInsertHead :: TsRow -> CashFlowFrame -> CashFlowFrame
cfInsertHead tr (CashFlowFrame st trs) = CashFlowFrame st $ tr:trs


splitCashFlowFrameByDate :: CashFlowFrame -> Date -> SplitType  -> (CashFlowFrame,CashFlowFrame)
splitCashFlowFrameByDate (CashFlowFrame status txns) d st
  = let 
      (ls,rs) = splitByDate txns d st
      newStatus = case rs of 
                    [] -> (0, d, Nothing)
                    (r:_) -> (buildBegBal rs, d, Nothing)
    in 
      (CashFlowFrame status ls,CashFlowFrame newStatus rs)


splitPoolCashflowByDate :: PoolCashflow -> Date -> SplitType -> (PoolCashflow,PoolCashflow)
splitPoolCashflowByDate (poolCF, mAssetCfs) d st
  = let 
      (lPoolCF,rPoolCF) = splitCashFlowFrameByDate poolCF d st
      mAssetSplited = (\xs -> [ splitCashFlowFrameByDate x d st | x <- xs ]) <$> mAssetCfs
      assetCfs = (\xs -> [ (lCf, rCf) | (lCf,rCf) <- xs ]) <$> mAssetSplited 
      lAssetCfs = (fst <$>) <$> assetCfs
      rAssetCfs = (snd <$>) <$> assetCfs
    in 
      ((lPoolCF, lAssetCfs) , (rPoolCF, rAssetCfs))


getTxnLatestAsOf :: CashFlowFrame -> Date -> Maybe TsRow
getTxnLatestAsOf (CashFlowFrame _ txn) d = L.find (\x -> getDate x <= d) $ reverse txn

addTs :: TsRow -> TsRow -> TsRow     
-- ^ left cashflow is ealier ,right one is later,combine both and yield cashflow with earlier date
addTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
addTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2)
addTs (MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = sumStats st1 st2
    in 
      MortgageFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) bn p st
addTs (MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr@(MortgageDelinqFlow _ b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = sumStats st1 st2
    in 
      MortgageDelinqFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) bn p st

addTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) tr@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (b1 - mflowAmortAmount tr) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) (sumStats st1 st2)

addTs (LeaseFlow d1 b1 r1 def1) tr@(LeaseFlow d2 b2 r2 def2) 
  = LeaseFlow d1 (b1 - mflowAmortAmount tr) (r1 + r2) (def1 + def2)

addTs (ReceivableFlow d1 b1 af1 p1 fp1 def1 rec1 los1 st1) tr@(ReceivableFlow _ b2 af2 p2 fp2 def2 rec2 los2 st2)
  = ReceivableFlow d1 (b1 - mflowAmortAmount tr) (af1 + af2) (p1 + p2) (fp1 + fp2) (def1 + def2) (rec1 + rec2) (los1 + los2) (sumStats st1 st2)

combineTs :: TsRow -> TsRow -> TsRow     

-- ^ combine two cashflow records from two entities, return cashflow with earlier date
combineTs (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)

combineTs (BondFlow d1 b1 p1 i1 ) tr@(BondFlow _ b2 p2 i2 ) = BondFlow d1 (b1 + b2) (p1 + p2) (i1 + i2)

combineTs tr1@(MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr2@(MortgageDelinqFlow _ b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = sumStats st1 st2
    in 
      MortgageDelinqFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [mflowBegBalance tr1,mflowBegBalance tr2]) (toRational <$> [rat1,rat2]))) bn p st 

combineTs tr1@(MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) tr2@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = (+) <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = sumStats st1 st2
    in 
      MortgageFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [mflowBegBalance tr1, mflowBegBalance tr2]) (toRational <$> [rat1,rat2]))) bn p st 

combineTs tr1@(LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) tr2@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (b1 + b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [mflowBegBalance tr1, mflowBegBalance tr2]) (toRational <$> [rat1,rat2]))) (sumStats st1 st2)

combineTs (LeaseFlow d1 b1 r1 def1) tr@(LeaseFlow d2 b2 r2 def2) 
  = LeaseFlow d1 (b1 + b2) (r1 + r2) (def1 + def2)

combineTs (FixedFlow d1 b1 de1 cde1 p1 c1 ) (FixedFlow d2 b2 de2 cde2 p2 c2)
  = FixedFlow d1 (b1+b2) (de1+de2) (cde1+cde2) (p1+p2) (c1+c2)

combineTs (ReceivableFlow d1 b1 af1 p1 fp1 def1 rec1 los1 st1) tr@(ReceivableFlow _ b2 af2 p2 fp2 def2 rec2 los2 st2)
  = ReceivableFlow d1 (b1 + b2) (af1 + af2) (p1 + p2) (fp1 + fp2) (def1 + def2) (rec1 + rec2) (los1 + los2) (sumStats st1 st2)

-- ^ combine two cashflows from two entities,(auto patch a beg balance)
-- ^ left cashflow is ealier ,right one is later,combine both and yield cashflow with earlier date
combineTss :: [TsRow] -> [TsRow] -> [TsRow] -> [TsRow]
combineTss [] [] r = r
combineTss [] r [] = r
combineTss [] (r1:r1s) (r2:r2s)
  | getDate r1 > getDate r2 = combineTss [] (r2:r2s) (r1:r1s)
  | getDate r1 == getDate r2 = combineTss [combineTs r1 r2] r1s r2s -- `debug` ("combineTss after same"++show r1s++" "++show r2s)
  | otherwise = combineTss [set tsRowBalance (mflowBegBalance r2+(view tsRowBalance r1)) r1]
                           r1s
                           (r2:r2s)
                           
combineTss consols [] [] = reverse consols
combineTss (consol:consols) (r:rs) [] = combineTss (appendTs consol r:consol:consols) rs []
combineTss (consol:consols) [] (tr:trs) = combineTss (appendTs consol tr:consol:consols) [] trs
combineTss (consol:consols) (r:rs) (tr:trs)
  | getDate r == getDate tr = combineTss (appendTs consol (combineTs r tr):consol:consols) rs trs
  | getDate r < getDate tr = combineTss (appendTs consol r:consol:consols) rs (tr:trs)
  | getDate r > getDate tr = combineTss (appendTs consol tr:consol:consols) (r:rs) trs 
combineTss a b c = error $ "combineTss not supported "++show a++" "++show b++" "++show c



appendTs :: TsRow -> TsRow -> TsRow 
-- ^ combine two cashflow records from two entities ,(early row on left, later row on right)
appendTs bn1@(BondFlow d1 b1 _ _ ) bn2@(BondFlow d2 b2 p2 i2 ) 
  = set tsRowBalance (b1 - mflowAmortAmount bn2) bn2 -- `debug` ("b1 >> "++show b1++">>"++show (mflowAmortAmount bn2))
appendTs (MortgageDelinqFlow d1 b1 p1 i1 prep1 _ def1 rec1 los1 rat1 mbn1 _ mstat1) bn2@(MortgageDelinqFlow _ b2 p2 i2 prep2 _ def2 rec2 los2 rat2 mbn2 _ mstat2)
  = set tsRowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs bn1@(MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 _ mstat1) bn2@(MortgageFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 _ mstat2)
  =  set tsRowBalance (b1 - mflowAmortAmount bn2) bn2 -- `debug` ("Summing stats"++ show bn1 ++ show mstat1++">>"++ show bn2 ++ show mstat2)
appendTs (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mstat1) bn2@(LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 mstat2)
  =  set tsRowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (LeaseFlow d1 b1 r1 def1) bn2@(LeaseFlow d2 b2 r2 def2) 
  = set tsRowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (FixedFlow d1 b1 de1 cde1 p1 c1 ) bn2@(FixedFlow d2 b2 de2 cde2 p2 c2)
  = set tsRowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs (ReceivableFlow d1 b1 af1 p1 fp1 def1 rec1 los1 mstat1) bn2@(ReceivableFlow _ b2 af2 p2 fp2 def2 rec2 los2 mstat2)
  =  set tsRowBalance (b1 - mflowAmortAmount bn2) bn2
appendTs _1 _2 = error $ "appendTs failed with "++ show _1 ++ ">>" ++ show _2

-- ^ add up TsRow from same entity
addTsCF :: TsRow -> TsRow -> TsRow
addTsCF (CashFlow d1 a1 ) (CashFlow _ a2 ) = CashFlow d1 (a1 + a2)
addTsCF (BondFlow d1 b1 p1 i1 ) (BondFlow _ b2 p2 i2 ) = BondFlow d1 (min b1 b2) (p1 + p2) (i1 + i2)
addTsCF m1@(MortgageFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 mbn1 pn1 st1) m2@(MortgageFlow d2 b2 p2 i2 prep2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = min <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      st = maxStats st1 st2
    in 
      MortgageFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) bn p st 
addTsCF (MortgageDelinqFlow d1 b1 p1 i1 prep1 delinq1 def1 rec1 los1 rat1 mbn1 pn1 st1) (MortgageDelinqFlow d2 b2 p2 i2 prep2 delinq2 def2 rec2 los2 rat2 mbn2 pn2 st2)
  = let 
      bn = min <$> mbn1 <*> mbn2
      p =  (+) <$> pn1 <*> pn2
      delinq = (+) delinq1 delinq2
      st = maxStats st1 st2
    in 
      MortgageDelinqFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) delinq (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) bn p st
addTsCF (LoanFlow d1 b1 p1 i1 prep1 def1 rec1 los1 rat1 st1) (LoanFlow _ b2 p2 i2 prep2 def2 rec2 los2 rat2 st2)
  = LoanFlow d1 (min b1 b2) (p1 + p2) (i1 + i2) (prep1 + prep2) (def1 + def2) (rec1 + rec2) (los1+los2) (fromRational (weightedBy (toRational <$> [b1,b2]) (toRational <$> [rat1,rat2]))) (maxStats st1 st2)
addTsCF (LeaseFlow d1 b1 r1 def1) (LeaseFlow d2 b2 r2 def2) = LeaseFlow d1 (min b1 b2) (r1 + r2) (def1 + def2)
addTsCF (FixedFlow d1 b1 dep1 cd1 u1 c1) (FixedFlow d2 b2 dep2 cd2 u2 c2) 
  = FixedFlow d1 (min b1 b2) (dep1 + dep2) (cd1 + cd2) u2 (c1 + c2)
addTsCF (ReceivableFlow d1 b1 af1 p1 fp1 def1 rec1 los1 st1) (ReceivableFlow d2 b2 af2 p2 fp2 def2 rec2 los2 st2)
  = ReceivableFlow d1 (min b1 b2) (af1 + af2) (p1 + p2) (fp1 + fp2) (def1 + def2) (rec1 + rec2) (los1 + los2) (maxStats st1 st2)


buildBegBal :: [TsRow] -> Balance
buildBegBal [] = 0
buildBegBal (x:xs) = mflowBegBalance x


sumTs :: [TsRow] -> Date -> TsRow
sumTs trs d = set tsDate d (foldr1 addTs trs)

-- ^ group cashflow from same entity by a single date
sumTsCF :: [TsRow] -> Date -> TsRow
-- sumTsCF [] = tsSetDate (foldl1 addTsCF trs) -- `debug` ("Summing"++show trs++">>"++ show (tsSetDate (foldr1 addTsCF trs) d))
sumTsCF [] _ = error "sumTsCF failed with empty list"
sumTsCF trs d = set tsDate d (foldl1 addTsCF trs) --  `debug` ("Summing"++show trs++">>"++ show (tsSetDate (foldr1 addTsCF trs) d))

tsTotalCash :: TsRow -> Balance
tsTotalCash (CashFlow _ x) = x
tsTotalCash (BondFlow _ _ a b) = a + b
tsTotalCash (MortgageDelinqFlow x _ a b c _ _ e _ _ _ mPn _ ) = a + b + c + e + fromMaybe 0 mPn
tsTotalCash (MortgageFlow x _ a b c _ e _ _ _ mPn _) = a + b + c + e + fromMaybe 0 mPn
tsTotalCash (LoanFlow _ _ a b c _ e _ _ _) =  a + b + c + e
tsTotalCash (LeaseFlow _ _ a _) =  a
tsTotalCash (FixedFlow _ _ _ _ _ x) = x
tsTotalCash (ReceivableFlow _ _ _ a b _ c _ _ ) = a + b + c

tsDefaultBal :: TsRow -> Either String Balance
tsDefaultBal CashFlow {} = Left "no default amount for bond flow"
tsDefaultBal BondFlow {} = Left "no default amount for bond flow"
tsDefaultBal (MortgageDelinqFlow _ _ _ _ _ _ x _ _ _ _ _ _) = Right x
tsDefaultBal (MortgageFlow _ _ _ _ _ x _ _ _ _ _ _) = Right x
tsDefaultBal (LoanFlow _ _ _ _ _ x _ _ _ _) = Right x
tsDefaultBal (LeaseFlow _ _ _ x) = Right x
tsDefaultBal (FixedFlow _ _ x _ _ _) =  Right x
tsDefaultBal (ReceivableFlow _ _ _ _ _ x _ _ _ ) = Right x

tsCumulative :: Lens' TsRow (Maybe CumulativeStat)
tsCumulative = lens getter setter
  where
    getter (MortgageDelinqFlow  _ _ _ _ _ _ _ _ _ _ _ _ mStat) = mStat
    getter (MortgageFlow  _ _ _ _ _ _ _ _ _ _ _ mStat) = mStat
    getter (LoanFlow  _ _ _ _ _ _ _ _ _ mStat) = mStat
    getter (ReceivableFlow _ _ _ _ _ _ _ _ mStat) = mStat
    getter _ = Nothing

    setter (MortgageDelinqFlow  a b c d e f g h i j k l _) mStat = MortgageDelinqFlow a b c d e f g h i j k l mStat
    setter (MortgageFlow  a b c d e f g h i j k _) mStat = MortgageFlow a b c d e f g h i j k mStat
    setter (LoanFlow  a b c d e f g h i _) mStat = LoanFlow a b c d e f g h i mStat
    setter (ReceivableFlow a b c d e f g h _) mStat = ReceivableFlow a b c d e f g h mStat
    setter x _ = x

tsCumDefaultBal :: TsRow -> Maybe Balance
tsCumDefaultBal tr = preview (tsCumulative . _Just . _4) tr

tsCumDelinqBal :: TsRow -> Maybe Balance
tsCumDelinqBal tr = preview (tsCumulative . _Just . _3) tr

tsCumLossBal :: TsRow -> Maybe Balance
tsCumLossBal tr = preview (tsCumulative . _Just . _6) tr

tsCumRecoveriesBal :: TsRow -> Maybe Balance
tsCumRecoveriesBal tr = preview (tsCumulative . _Just . _5) tr

tsDate :: Lens' TsRow Date 
tsDate = lens getter setter 
  where 
    getter (CashFlow x _) = x
    getter (BondFlow x _ _ _) = x
    getter (MortgageDelinqFlow x _ _ _ _ _ _ _ _ _ _ _ _) = x 
    getter (MortgageFlow x _ _ _ _ _ _ _ _ _ _ _) = x
    getter (LoanFlow x _ _ _ _ _ _ _ _ _) = x
    getter (LeaseFlow x _ _ _ ) = x
    getter (FixedFlow x _ _ _ _ _) = x
    getter (ReceivableFlow x _ _ _ _ _ _ _ _) = x
    setter (CashFlow _ a) x = CashFlow x a
    setter (BondFlow _ a b c) x = BondFlow x a b c
    setter (MortgageDelinqFlow _ a b c d e f g h i j k l) x = MortgageDelinqFlow x a b c d e f g h i j k l
    setter (MortgageFlow _ a b c d e f g h i j k) x = MortgageFlow x a b c d e f g h i j k
    setter (LoanFlow _ a b c d e f g h i) x = LoanFlow x a b c d e f g h i
    setter (LeaseFlow _ a b c) x = LeaseFlow x a b c
    setter (FixedFlow _ a b c d e) x = FixedFlow x a b c d e
    setter (ReceivableFlow _ a b c d e f g h) x = ReceivableFlow x a b c d e f g h

tsSetLoss :: Balance -> TsRow -> TsRow
tsSetLoss x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d a b c d e f g x i j k l
tsSetLoss x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d a b c d e f x h i j k 
tsSetLoss x (LoanFlow _d a b c d e f g h i) = LoanFlow _d a b c d e f x h i
tsSetLoss x (ReceivableFlow _d a b c d e f g h) = ReceivableFlow _d a b c d e f x h
tsSetLoss x _ = error $ "Failed to set Loss for "++show x

tsSetRecovery :: Balance -> TsRow -> TsRow
tsSetRecovery x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d a b c d e f x h i j k l
tsSetRecovery x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d a b c d e x g h i j k 
tsSetRecovery x (LoanFlow _d a b c d e f g h i) = LoanFlow _d a b c d e x g h i
tsSetRecovery x (ReceivableFlow _d a b c d e f g h) = ReceivableFlow _d a b c d e x g h
tsSetRecovery x _ = error $ "Failed to set Recovery for "++show x

tsOffsetDate :: Integer -> TsRow -> TsRow
tsOffsetDate x (CashFlow _d a) = CashFlow (T.addDays x _d) a
tsOffsetDate x (BondFlow _d a b c) = BondFlow (T.addDays x _d) a b c
tsOffsetDate x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow (T.addDays x _d) a b c d e f g h i j k l
tsOffsetDate x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow (T.addDays x _d) a b c d e f g h i j k
tsOffsetDate x (LoanFlow _d a b c d e f g h i) = LoanFlow (T.addDays x _d) a b c d e f g h i
tsOffsetDate x (LeaseFlow _d a b c) = LeaseFlow (T.addDays x _d) a b c
tsOffsetDate x (ReceivableFlow _d a b c d e f g h) = ReceivableFlow (T.addDays x _d) a b c d e f g h

tsReduceInt :: Balance -> TsRow -> TsRow
tsReduceInt x (BondFlow _d a b c) = BondFlow _d a b (c-x)
tsReduceInt x (MortgageDelinqFlow _d a b c d e f g h i j k l) = MortgageDelinqFlow _d a b (c-x) d e f g h i j k l
tsReduceInt x (MortgageFlow _d a b c d e f g h i j k) = MortgageFlow _d a b (c-x) d e f g h i j k 
tsReduceInt x (LoanFlow _d a b c d e f g h i) = LoanFlow _d a b (c-x) d e f g h i
tsReduceInt _ x = error $ "Failed to reduce interest on asset "++ show x

-- ^ claw back interest from cashflow records
clawbackInt :: Balance -> [TsRow] -> [TsRow]
clawbackInt bal txns
  = let
      intFlow = mflowInterest <$> txns
      intDowns = paySeqLiabilitiesAmt bal intFlow
    in 
      [ tsReduceInt intDown txn | (txn,intDown) <- zip txns intDowns]

aggregateTsByDate :: [TsRow] -> [TsRow] -> [TsRow]
aggregateTsByDate rs [] = reverse rs
aggregateTsByDate [] (tr:trs) = aggregateTsByDate [tr] trs
aggregateTsByDate (r:rs) (tr:trs) 
  | sameDate r tr = aggregateTsByDate (combineTs r tr:rs) trs
  | otherwise = aggregateTsByDate (tr:r:rs) trs


firstDate :: CashFlowFrame -> Date 
firstDate (CashFlowFrame _ []) = error "empty cashflow frame to get first date"
firstDate (CashFlowFrame _ [r]) = getDate r
firstDate (CashFlowFrame _ (r:rs)) = getDate r


-- ! combine two cashflow frames from two entities
-- ! cashflow earlier on the left ,later cashflow on the right
combine :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame 
combine (CashFlowFrame st1 []) (CashFlowFrame st2 []) = CashFlowFrame st1 []
combine (CashFlowFrame _ []) cf2 = cf2
combine cf1 (CashFlowFrame _ []) = cf1
combine cf1@(CashFlowFrame st1@(begBal1,begDate1,acc1) txn1) cf2@(CashFlowFrame st2@(begBal2,begDate2,acc2) txn2) 
  | begDate1 > begDate2 = combine cf2 cf1
  | otherwise =
    let 
      txns = combineTss [] txn1 txn2
    in 
      CashFlowFrame (begBal1,begDate1,acc1) txns 

buildCollectedCF :: [[TsRow]] -> [Date] -> [TsRow] -> [[TsRow]]
buildCollectedCF [] [] [] = []
buildCollectedCF trs [] _trs = trs
buildCollectedCF trs ds [] = trs ++ [ [viewTsRow _d ((last . last) trs)] | _d <- ds ]
buildCollectedCF trs (d:ds) _trs =
  case newFlow of
    [] -> case Util.lastOf trs (not . null) of
            Nothing -> buildCollectedCF (trs++[[]]) ds _trs  -- `debug` ("empty trs"++ show d)
            Just lastTr ->  buildCollectedCF (trs++[[viewTsRow d (last lastTr)]]) ds _trs -- `debug` ("non empty last tr "++ show lastTr ++ "for date"++ show d++"insert with "++show (viewTsRow d (last lastTr)))
    newFlow -> buildCollectedCF (trs++[newFlow]) ds remains
  where 
    (newFlow, remains) = splitBy d Inc _trs

buildCollectedCF a b c = error $ "buildCollectedCF failed"++ show a++">>"++ show b++">>"++ show c


aggTsByDates :: [TsRow] -> [Date] -> [TsRow]
aggTsByDates [] ds = []
aggTsByDates trs ds = uncurry sumTsCF <$> filter (\(cfs,_d) -> (not . null) cfs) (zip (buildCollectedCF [] ds trs) ds) -- `debug` (">>> to sumTsCF "++ show (zip (buildCollectedCF [] ds trs) ds ))

mflowPrincipal :: TsRow -> Balance
mflowPrincipal (BondFlow _ _ p _) = p
mflowPrincipal (MortgageFlow _ _ x _ _ _ _ _ _ _ _ _) = x
mflowPrincipal (MortgageDelinqFlow _ _ x _ _ _ _ _ _ _ _ _ _) = x
mflowPrincipal (LoanFlow _ _ x _ _ _ _ _ _ _) = x
mflowPrincipal (ReceivableFlow _ _ _ x _ _ _ _ _) = x
mflowPrincipal _  = error "not supported"

mflowInterest :: TsRow -> Balance
mflowInterest (BondFlow _ _ _ i) = i
mflowInterest (MortgageDelinqFlow _ _ _ x _ _ _ _ _ _ _ _ _) = x
mflowInterest (MortgageFlow _ _ _ x _ _ _ _ _ _ _ _) = x
mflowInterest (LoanFlow _ _ _ x _ _ _ _ _ _) = x
mflowInterest x  = error $ "not supported: getting interest from row" ++ show x

mflowPrepayment :: TsRow -> Balance
mflowPrepayment (MortgageFlow _ _ _ _ x _ _ _ _ _ _ _) = x
mflowPrepayment (MortgageDelinqFlow _ _ _ _ x _ _ _ _ _ _ _ _) = x
mflowPrepayment (LoanFlow _ _ _ _ x _ _ _ _ _) = x
mflowPrepayment _  = error "not supported"

mflowDefault :: TsRow -> Balance
mflowDefault (MortgageFlow _ _ _ _ _ x _ _ _ _ _ _) = x
mflowDefault (MortgageDelinqFlow _ _ _ _ _ _ x _ _ _ _ _ _) = x
mflowDefault (LoanFlow _ _ _ _ _ x _ _ _ _) = x
mflowDefault (FixedFlow _ _ x _ _ _) = x
mflowDefault (ReceivableFlow _ _ _ _ _ x _ _ _ ) = x
mflowDefault _  = 0

mflowRecovery :: TsRow -> Balance
mflowRecovery (MortgageFlow _ _ _ _ _ _ x _ _ _ _ _) = x
mflowRecovery (MortgageDelinqFlow _ _ _ _ _ _ _ x _ _ _ _ _) = x
mflowRecovery (LoanFlow _ _ _ _ _ _ x _ _ _) = x
mflowRecovery FixedFlow {} = 0
mflowRecovery (ReceivableFlow _ _ _ _ _ _ x _ _ ) = x
mflowRecovery (LeaseFlow _ _ _ _) = 0
mflowRecovery _  = error "not supported"

tsRowBalance :: Lens' TsRow Balance
tsRowBalance = lens getter setter 
  where 
    getter (BondFlow _ x _ _) = x
    getter (MortgageFlow _ x _ _ _ _ _ _ _ _ _ _) = x
    getter (MortgageDelinqFlow _ x _ _ _ _ _ _ _ _ _ _ _) = x
    getter (LoanFlow _ x _ _ _ _ _ _ _ _) = x
    getter (LeaseFlow _ x _ _) = x
    getter (FixedFlow _ x _ _ _ _) = x
    getter (ReceivableFlow _ x _ _ _ _ _ _ _ ) = x

    setter (BondFlow a _ p i) x = BondFlow a x p i
    setter (MortgageFlow a _ p i prep def rec los rat mbn pn st) x = MortgageFlow a x p i prep def rec los rat mbn pn st
    setter (MortgageDelinqFlow a _ p i prep delinq def rec los rat mbn pn st) x = MortgageDelinqFlow a x p i prep delinq def rec los rat mbn pn st
    setter (LoanFlow a _ p i prep def rec los rat st) x = LoanFlow a x p i prep def rec los rat st
    setter (LeaseFlow a _ r def) x = LeaseFlow a x r def
    setter (FixedFlow a _ b c d e) x = FixedFlow a x b c d e
    setter (ReceivableFlow a _ b c d e f g h) x = ReceivableFlow a x b c d e f g h


mflowBegBalance :: TsRow -> Balance
mflowBegBalance (BondFlow _ x p _) = x + p
mflowBegBalance (MortgageDelinqFlow _ x p _ ppy delinq def _ _ _ _ _ _) = x + p + ppy + delinq
mflowBegBalance (MortgageFlow _ x p _ ppy def _ _ _ _ _ _) = x + p + ppy + def
mflowBegBalance (LoanFlow _ x p _ ppy def _ _ _ _) = x + p + ppy + def
mflowBegBalance (LeaseFlow _ b r def ) = b + r + def 
mflowBegBalance (FixedFlow a b c d e f ) = b + c
mflowBegBalance (ReceivableFlow _ x _ b f def _ _ _) = x + b + def + f

mflowLoss :: TsRow -> Balance
mflowLoss (MortgageFlow _ _ _ _ _ _ _ x _ _ _ _) = x
mflowLoss (MortgageDelinqFlow _ _ _ _ _ _ _ _ x _ _ _ _) = x
mflowLoss (LoanFlow _ _ _ _ _ _ _ x _ _) = x
mflowLoss (ReceivableFlow _ _ _ _ _ _ _ x _ ) = x
mflowLoss _ = 0

mflowDelinq :: TsRow -> Balance
mflowDelinq (MortgageDelinqFlow _ _ _ _ _ x _ _ _ _ _ _ _) = x
mflowDelinq _ = 0

mflowRate :: TsRow -> IRate
-- ^ get rate(weigthed avg rate) for a cashflow record
mflowRate (MortgageFlow _ _ _ _ _ _ _ _ x _ _ _) = x
mflowRate (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ x _ _ _) = x
mflowRate (LoanFlow _ _ _ _ _ _ _ _ x _) = x
mflowRate (BondFlow _ _ _ _) = 0
mflowRate _ = 0

mflowRental :: TsRow -> Amount
mflowRental (LeaseFlow _ _ x _) = x
mflowRental x = error ("not support get rental from row"++show x)

mflowFeePaid :: TsRow -> Amount
mflowFeePaid (ReceivableFlow _ _ _ _ x _ _ _ _ ) = x
mflowFeePaid _ = 0

mflowAmortAmount :: TsRow -> Balance
-- ^ calculate amortized amount for cashflow (for defaults only)
mflowAmortAmount (MortgageFlow _ _ p _ ppy def _ _ _ _ _ _) = p + ppy + def
mflowAmortAmount (MortgageDelinqFlow _ _ p _ ppy delinq _ _ _ _ _ _ _) = p + ppy + delinq
mflowAmortAmount (LoanFlow _ _ x _ y z _ _ _ _) = x + y + z
mflowAmortAmount (LeaseFlow _ _ x def) = x + def
mflowAmortAmount (FixedFlow _ _ x _ _ _) = x
mflowAmortAmount (BondFlow _ _ p i) = p
mflowAmortAmount (ReceivableFlow _ _ _ x f def _ _ _ ) = x + def + f

mflowBorrowerNum :: TsRow -> Maybe BorrowerNum
-- ^ get borrower numfer for Mortgage Flow
mflowBorrowerNum (MortgageFlow _ _ _ _ _ _ _ _ _ x _ _) = x
mflowBorrowerNum (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ x _ _) = x
mflowBorrowerNum _ = undefined

mflowPrepaymentPenalty :: TsRow -> Balance
-- ^ get prepayment penalty for a cashflow record
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ (Just x) _) = x
mflowPrepaymentPenalty (MortgageFlow _ _ _ _ _ _ _ _ _ _ Nothing _) = 0
mflowPrepaymentPenalty (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ (Just x) _) = x
mflowPrepaymentPenalty (MortgageDelinqFlow _ _ _ _ _ _ _ _ _ _ _ Nothing _) = 0
mflowPrepaymentPenalty _ = undefined

-- tobe factor out alongside with similar funciton in bond cashflow
mflowWeightAverageBalance :: Date -> Date -> [TsRow] -> Balance
mflowWeightAverageBalance sd ed trs
  = sum $ zipWith mulBR _bals _dfs  -- `debug` ("CalcingAvgBal=>"++show sd++show ed++show txns  )
    where
     txns = filter (\x -> (view tsDate x >=sd)&& (view tsDate x)<=ed) trs
     _ds = view tsDate <$> txns -- `debug` ("fee base txns"++show txns)
     _bals = map mflowBegBalance txns
     _dfs =  getIntervalFactors $ sd:_ds

emptyTsRow :: Date -> TsRow -> TsRow 
-- ^ reset all cashflow fields to zero and init with a date
emptyTsRow _d (MortgageDelinqFlow a x c d e f g h i j k l m) = MortgageDelinqFlow _d 0 0 0 0 0 0 0 0 0 Nothing Nothing Nothing
emptyTsRow _d (MortgageFlow a x c d e f g h i j k l) = MortgageFlow _d 0 0 0 0 0 0 0 0 Nothing Nothing Nothing
emptyTsRow _d (LoanFlow a x c d e f g i j k) = LoanFlow _d 0 0 0 0 0 0 0 0 Nothing
emptyTsRow _d (LeaseFlow a x c d) = LeaseFlow _d 0 0 0
emptyTsRow _d (FixedFlow a x c d e f ) = FixedFlow _d 0 0 0 0 0
emptyTsRow _d (BondFlow a x c d) = BondFlow _d 0 0 0
emptyTsRow _d (ReceivableFlow a x c d e f g h i) = ReceivableFlow _d 0 0 0 0 0 0 0 Nothing

extendCashFlow :: Date -> CashFlowFrame -> CashFlowFrame
extendCashFlow d (CashFlowFrame st []) = CashFlowFrame st []
extendCashFlow d (CashFlowFrame st txns) 
    = let 
        lastRow = last txns
        newTxn = emptyTsRow d lastRow
      in 
        CashFlowFrame st (txns++[newTxn])


viewTsRow :: Date -> TsRow -> TsRow 
-- ^ take a snapshot of a record from record balance/stats and a new date
viewTsRow _d (MortgageDelinqFlow a b c d e f g h i j k l m) = MortgageDelinqFlow _d b 0 0 0 0 0 0 0 j k l m
viewTsRow _d (MortgageFlow a b c d e f g h i j k l) = MortgageFlow _d b 0 0 0 0 0 0 i j k l
viewTsRow _d (LoanFlow a b c d e f g i j k) = LoanFlow _d b 0 0 0 0 0 0 j k
viewTsRow _d (LeaseFlow a b c d) = LeaseFlow _d b 0 0
viewTsRow _d (FixedFlow a b c d e f ) = FixedFlow _d b 0 0 0 0
viewTsRow _d (BondFlow a b c d) = BondFlow _d b 0 0
viewTsRow _d (ReceivableFlow a b c d e f g h i) = ReceivableFlow _d b 0 0 0 0 0 0 i

-- ^ given a cashflow,build a new cf row with begin balance
buildBegTsRow :: Date -> TsRow -> TsRow
buildBegTsRow d flow@FixedFlow{} = flow
buildBegTsRow d tr = 
  let 
    r = set tsRowBalance ((view tsRowBalance tr) + mflowAmortAmount tr) (emptyTsRow d tr)
    rate = mflowRate tr
  in
    tsSetRate rate r

buildStartTsRow :: CashFlowFrame -> Maybe TsRow
buildStartTsRow (CashFlowFrame (begBal,begDate,accInt) []) = Nothing
buildStartTsRow (CashFlowFrame (begBal,begDate,accInt) (txn:txns)) = 
  let 
    rEmpty = emptyTsRow begDate txn 
    r = set tsRowBalance begBal rEmpty
    rate = mflowRate txn
  in
    Just $ tsSetRate rate r

tsSetRate :: IRate -> TsRow -> TsRow
tsSetRate _r (MortgageDelinqFlow a b c d e f g h i j k l m) = MortgageDelinqFlow a b c d e f g h i _r k l m
tsSetRate _r (MortgageFlow a b c d e f g h i j k l) = MortgageFlow a b c d e f g h _r j k l
tsSetRate _r (LoanFlow a b c d e f g i j k) = LoanFlow a b c d e f g i _r k
tsSetRate _r (BondFlow a b c d) = BondFlow a b c d
tsSetRate _r (ReceivableFlow a b c d e f g h i) = ReceivableFlow a b c d e f g h i
tsSetRate _r (LeaseFlow a b c d) = LeaseFlow a b c d
tsSetRate _r (FixedFlow {} ) = error "Not implement set rate for FixedFlow"
tsSetRate _ _ = error "Not implement set rate for this type"


insertBegTsRow :: Date -> CashFlowFrame -> CashFlowFrame
insertBegTsRow d (CashFlowFrame st []) = CashFlowFrame st []
insertBegTsRow d (CashFlowFrame st (txn:txns))
  = let
      begRow = buildBegTsRow d txn
    in 
      CashFlowFrame st (begRow:txn:txns)


totalLoss :: CashFlowFrame -> Balance
totalLoss (CashFlowFrame _ rs) = sum $ mflowLoss <$> rs

totalDefault :: CashFlowFrame -> Balance
totalDefault (CashFlowFrame _ rs) = sum $ mflowDefault <$> rs

totalRecovery :: CashFlowFrame -> Balance
totalRecovery (CashFlowFrame _ rs) = sum $ mflowRecovery <$> rs

totalPrincipal :: CashFlowFrame -> Balance
totalPrincipal (CashFlowFrame _ rs) = sum $ mflowPrincipal <$> rs

-- ^ merge two cashflow frame but no patching beg balance
mergePoolCf :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
mergePoolCf cf (CashFlowFrame _ []) = cf
mergePoolCf (CashFlowFrame _ []) cf = cf
-- first day of left is earlier than right one
mergePoolCf cf1@(CashFlowFrame st1 txns1) cf2@(CashFlowFrame st2 txns2) 
  | startDate1 > startDate2 = mergePoolCf cf2 cf1 
  | otherwise 
      = let 
          splitDate = firstDate cf2  -- (ls,rs) = splitByDate txns d st
          (txn0,txnToMerged) = splitByDate txns1 splitDate EqToRight
          txn1 = combineTss [] txnToMerged txns2 -- `debug` ("left"++show cfToBeMerged++">> right"++ show cf2)
        in 
          CashFlowFrame st1 (txn0++txn1) -- `debug` ("Txn1"++show txn1)
  where 
    [startDate1,startDate2] = firstDate <$> [cf1,cf2]


-- ^ agg cashflow (but not updating the cumulative stats)
aggTs :: [TsRow] -> [TsRow] -> [TsRow]
-- ^ short circuit
aggTs [] [] = []
-- ^ return result update the cumulative stats
aggTs rs [] = rs 
-- ^ init with the first row
aggTs [] (r:rs) = aggTs [r] rs
aggTs (r:rs) (tr:trs) 
  | sameDate r tr = aggTs (addTs r tr:rs) trs
  | otherwise = aggTs (tr:r:rs) trs 


patchBalance :: (Balance,Maybe CumulativeStat) -> [TsRow] -> [TsRow] -> [TsRow]
patchBalance (bal,stat) [] [] = []
patchBalance (bal,mStat) r [] = case mStat of 
                                  Just stat -> patchCumulative stat [] $ reverse r
                                  Nothing -> patchCumulative (0,0,0,0,0,0) [] $ reverse r
patchBalance (bal,stat) r (tr:trs) = 
  let 
    amortAmt = mflowAmortAmount tr
    newBal = bal - amortAmt
    rWithUpdatedBal = set tsRowBalance newBal tr
  in 
    patchBalance (newBal,stat) (rWithUpdatedBal:r) trs

-- type CumulativeStat = (CumPrincipal,CumPrepay,CumDelinq,CumDefault,CumRecovery,CumLoss)
-- 
calcBeginStats :: Maybe CumulativeStat -> TsRow -> CumulativeStat
calcBeginStats Nothing tr = (0,0,0,0,0,0)
calcBeginStats (Just (cumPrin,cumPrepay,cumDlinq,cumDef,cumRec,cumLoss)) tr
  = case tr of 
      (MortgageFlow _ _ p _ ppy def rec los _ _ _ _) -> 
        (cumPrin - p,cumPrepay - ppy, 0 , cumDef - def, cumRec - rec , cumLoss - los)
      (MortgageDelinqFlow _ _ p _ ppy delinq def rec los _ _ _ _) -> 
        (cumPrin - p,cumPrepay - ppy, cumDlinq - delinq , cumDef - def, cumRec - rec , cumLoss - los)
      (LoanFlow _ _ p _ ppy def rec los _ _) -> 
        (cumPrin - p,cumPrepay - ppy, 0 , cumDef - def, cumRec - rec , cumLoss - los)
      (ReceivableFlow _ _ _ p f def rec los _) -> 
        (cumPrin - p, 0 , 0 , cumDef - def, cumRec - rec , cumLoss - los)
      (BondFlow _ _ p i) -> 
        (cumPrin - p,0 , 0 , 0, 0, 0)
      (LeaseFlow _ b r def ) -> 
        (cumPrin - r,0 , 0, cumDef - def, 0, 0)
      (FixedFlow _ b c d e _ ) -> (0, 0 ,0 , 0, 0, 0)
      (CashFlow _ amt) -> (0,0,0,0,0,0)


getCfBegStats :: CashFlowFrame -> CumulativeStat
getCfBegStats (CashFlowFrame _ []) = (0,0,0,0,0,0)
getCfBegStats (CashFlowFrame _ (tr:trs)) = calcBeginStats (view tsCumulative tr) tr


mergePoolCf2 :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
mergePoolCf2 cf (CashFlowFrame _ []) = cf
mergePoolCf2 (CashFlowFrame _ []) cf = cf
mergePoolCf2 cf1@(CashFlowFrame st1@(bBal1,bDate1,a1) txns1) cf2@(CashFlowFrame (bBal2,bDate2,a2) txns2) 
  | null txns2 = over cashflowTxn (patchBalance (bBal1,head txns1 ^. tsCumulative) []) cf1
  | bDate1 > bDate2 = mergePoolCf2 cf2 cf1
  -- both cashflow frame start on the same day OR left one starts earlier than right one
  -- 20241021:why? | bDate1 == bDate2 && bBal2 == 0 = over cashflowTxn (patchBalance bBal1 []) cf1
  | bDate1 == bDate2 && bBal2 == 0 = cf1
  | bDate1 == bDate2 = 
    let 
      begBal = bBal1 + bBal2
      
      begStat = sumStats (Just (getCfBegStats cf1)) (Just (getCfBegStats cf2))
      txnsSorted = reverse $ L.sortOn getDate (txns1 ++ txns2)
      txnAggregated = aggTs [] txnsSorted
      txnPatchedBalance = patchBalance (begBal,begStat) [] txnAggregated -- `debug` ("\n Pathcing with stat"++ show begStat)
    in 
      CashFlowFrame (begBal, bDate1, a1) txnPatchedBalance
  | otherwise 
      = let 
          (resultCf1, cfToCombine) = splitCashFlowFrameByDate cf1 bDate2 EqToRight 
          (CashFlowFrame _ txnCombined) = mergePoolCf2 cfToCombine cf2
        in 
          over cashflowTxn (++ txnCombined) resultCf1 


mergeCf :: CashFlowFrame -> CashFlowFrame -> CashFlowFrame
mergeCf cf (CashFlowFrame _ []) = cf
mergeCf (CashFlowFrame _ []) cf = cf
mergeCf cf1@(CashFlowFrame (begBal1,begDate1,mAccInt1) txns1) cf2@(CashFlowFrame (begBal2,begDate2,mAccInt2)txns2) -- first day of left is earlier than right one
  = let 
      mSrow1 = buildStartTsRow cf1
      mSrow2 = buildStartTsRow cf2
      txns1' = case mSrow1 of
                  Nothing -> txns1
                  Just srow1 -> srow1:txns1
      txns2' = case mSrow2 of
                  Nothing -> txns2
                  Just srow2 -> srow2:txns2
      txns = combineTss [] txns1' txns2'
      newSt = if begDate1 < begDate2 then (begBal1,begDate1,mAccInt1) else (begBal2,begDate2,mAccInt2)
    in 
      CashFlowFrame newSt txns


consolidateCashFlow :: CashFlowFrame -> CashFlowFrame
consolidateCashFlow (CashFlowFrame st []) = CashFlowFrame st []
consolidateCashFlow (CashFlowFrame st (txn:txns))
  = let 
      totalBals = sum $ mflowAmortAmount <$> (txn:txns)
    in 
      CashFlowFrame st (set tsRowBalance totalBals txn:txns)
    

shiftCfToStartDate :: Date -> CashFlowFrame -> CashFlowFrame
shiftCfToStartDate d cf@(CashFlowFrame st (txn:txns))
  = let 
      fstDate = firstDate cf 
      diffDays = daysBetween fstDate d
    in 
      CashFlowFrame st $ tsOffsetDate diffDays <$> (txn:txns)

-- ^ sum a single pool source from a cashflow frame
sumPoolFlow :: CashFlowFrame -> PoolSource -> Balance
sumPoolFlow (CashFlowFrame _ trs) ps 
  = sum $ (`lookupSource` ps) <$> trs

-- ^ lookup a pool source from a row
lookupSource :: TsRow -> PoolSource -> Balance 
lookupSource tr CollectedPrepayment  = mflowPrepayment tr
lookupSource tr CollectedPrincipal = mflowPrincipal tr
lookupSource tr CollectedRecoveries = mflowRecovery tr
lookupSource tr CollectedRental = mflowRental tr
lookupSource tr CollectedInterest = mflowInterest tr
lookupSource tr CollectedPrepaymentPenalty = mflowPrepaymentPenalty tr
lookupSource tr CollectedFeePaid = mflowFeePaid tr
lookupSource tr CollectedCash = tsTotalCash tr
lookupSource tr NewDelinquencies = mflowDelinq tr
lookupSource tr NewDefaults = mflowDefault tr
lookupSource tr NewLosses = mflowLoss tr
lookupSource tr CurBalance = view tsRowBalance tr
lookupSource tr CurBegBalance = mflowBegBalance tr
lookupSource tr x = error ("Failed to lookup source"++ show x)

lookupSourceM :: Balance -> Maybe TsRow -> PoolSource -> Balance
lookupSourceM bal Nothing CurBegBalance = bal
lookupSourceM bal Nothing CurBalance = bal
lookupSourceM _ Nothing _ = 0
lookupSourceM _ (Just tr) ps = lookupSource tr ps


setPrepaymentPenalty :: Balance -> TsRow -> TsRow
setPrepaymentPenalty bal (MortgageDelinqFlow a b c d e f g h i j k l m) = MortgageDelinqFlow a b c d e f g h i j k (Just bal) m
setPrepaymentPenalty bal (MortgageFlow b c d e f g h i j k l m) = MortgageFlow b c d e f g h i j k (Just bal) m
setPrepaymentPenalty _ _ = error "prepay pental only applies to MortgageFlow"

setPrepaymentPenaltyFlow :: [Balance] -> [TsRow] -> [TsRow]
setPrepaymentPenaltyFlow bals trs = [ setPrepaymentPenalty bal tr | (bal,tr) <- zip bals trs]


-- ^ split single cashflow record by a rate
splitTs :: Rate -> TsRow -> TsRow 
splitTs r (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN mStat)
  = MortgageDelinqFlow d (mulBR bal r) (mulBR p r) (mulBR i r) (mulBR ppy r)
                       (mulBR delinq r) (mulBR def r) (mulBR recovery r) (mulBR loss r)
                       rate ((\x -> round (toRational x * r)) <$> mB) ((`mulBR` r) <$> mPPN)
                       (splitStats r <$> mStat)
splitTs r (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN mStat)
  = MortgageFlow d (mulBR bal r) (mulBR p r) (mulBR i r) (mulBR ppy r)
                       (mulBR def r) (mulBR recovery r) (mulBR loss r)
                       rate ((\x -> round (toRational x * r)) <$> mB) ((`mulBR` r) <$> mPPN)
                       (splitStats r <$> mStat)
splitTs r (LeaseFlow d bal p def)
  = LeaseFlow d (mulBR bal r) (mulBR p r) (mulBR def r)
splitTs _ tr = error $ "Not support for spliting TsRow"++show tr

splitTrs :: Rate -> [TsRow] -> [TsRow]
splitTrs r trs = splitTs r <$> trs 

splitCf :: Rate -> CashFlowFrame -> CashFlowFrame
splitCf 1 cf = cf
splitCf r (CashFlowFrame st []) = CashFlowFrame st []
splitCf r (CashFlowFrame (begBal, begDate, mAccInt) trs) 
  = CashFlowFrame (mulBR begBal r, begDate, (`mulBR` r) <$> mAccInt) $ splitTrs r trs -- `debug` ("split by rate"++ show (fromRational r))

currentCumulativeStat :: [TsRow] -> CumulativeStat
currentCumulativeStat [] = (0,0,0,0,0,0)
currentCumulativeStat trs = 
  let 
    tr = last trs
  in 
    fromMaybe (0,0,0,0,0,0) $ view txnCumulativeStats tr


cashFlowInitCumulativeStats ::  Lens' CashFlowFrame (Maybe CumulativeStat)
cashFlowInitCumulativeStats = lens getter setter 
  where
    getter (CashFlowFrame _ []) = Nothing
    getter (CashFlowFrame _ (tr:trs)) = view txnCumulativeStats tr
    
    setter (CashFlowFrame st []) mStat = CashFlowFrame st []
    setter (CashFlowFrame st (tr:trs)) mStat = CashFlowFrame st $ (set txnCumulativeStats mStat tr):trs


patchCumulativeAtInit :: Maybe CumulativeStat -> [TsRow] -> [TsRow]
patchCumulativeAtInit _ [] = []
patchCumulativeAtInit mStatsInit (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN mStat:trs)
  = MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN (sumStats mStat mStatsInit):trs
patchCumulativeAtInit mStatsInit (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN mStat:trs)
  = MortgageFlow d bal p i ppy def recovery loss rate mB mPPN (sumStats mStat mStatsInit):trs
patchCumulativeAtInit mStatsInit (LoanFlow d bal p i ppy def recovery loss rate mStat:trs)
  = LoanFlow d bal p i ppy def recovery loss rate (sumStats mStat mStatsInit):trs
patchCumulativeAtInit mStatsInit (ReceivableFlow d bal p i ppy def recovery loss mStat:trs)
  = ReceivableFlow d bal p i ppy def recovery loss (sumStats mStat mStatsInit):trs
patchCumulativeAtInit _ trs = trs


patchCumulative :: CumulativeStat -> [TsRow] -> [TsRow] -> [TsRow]
patchCumulative _ rs [] = reverse rs
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
                rs
                (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN _:trs)
  = patchCumulative newSt
                    (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN (Just newSt):rs)
                    trs
                 where 
                   newSt = (cPrin+p,cPrepay+ppy,cDelinq+delinq,cDefault+def,cRecovery+recovery,cLoss+loss)
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
               rs
               ((MortgageFlow d bal p i ppy def recovery loss rate mB mPPN _):trs)
  = patchCumulative newSt
                   (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN (Just newSt):rs)
                   trs
                where 
                  newSt = (cPrin+p,cPrepay+ppy,cDelinq,cDefault+def,cRecovery+recovery,cLoss+loss)
patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
              rs
              ((LoanFlow d bal p i ppy def recovery loss rate _):trs)
  = patchCumulative newSt
                  (LoanFlow d bal p i ppy def recovery loss rate (Just newSt):rs)
                  trs
               where 
                 newSt = (cPrin+p,cPrepay+ppy,cDelinq,cDefault+def,cRecovery+recovery,cLoss+loss)

patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
              rs
              ((FixedFlow a b c d e f):trs)
  = patchCumulative newSt
                  (FixedFlow a b c d e f:rs)
                  trs
               where 
                 newSt = (0,0,0,0,0,0)

patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
              rs
              ((ReceivableFlow a b c d e f g h i):trs)
  = patchCumulative newSt
                  (ReceivableFlow a b c d e f g h (Just newSt):rs)
                  trs
               where
                 newSt = (cPrin+c,0,0,cDefault+f,cRecovery+g,cLoss+h)

patchCumulative (cPrin,cPrepay,cDelinq,cDefault,cRecovery,cLoss)
              rs
              ((LeaseFlow a b c d) :trs)
  = patchCumulative newSt
                  (LeaseFlow a b c d:rs)
                  trs
               where
                 newSt = (0,0,0,0,0,0)

patchCumulative a b c = error ("failed to patch cumulative stats for "++show a ++">>"++show b++">>"++show c)



-- ^ split cashflow by rate while build missing defaults/losses stats
cutoffTrs :: Date -> [TsRow] -> ([TsRow],Map.Map CutoffFields Balance)
cutoffTrs d [] = ([],Map.empty)
cutoffTrs d trs 
  = let 
      beforeTrs = cutBy Exc Past d trs
      cumuDefaults = sum $ mflowDefault <$> beforeTrs 
      cumuDelinquency = sum $ mflowDelinq <$> beforeTrs  
      cumuLoss = sum $ mflowLoss <$> beforeTrs 
      m = Map.fromList [(HistoryDefaults,cumuDefaults),(HistoryDelinquency,cumuDelinquency),(HistoryLoss,cumuLoss)]
      
      afterTrs  = cutBy Inc Future d trs
    in
      (patchCumulative (0.0,0.0,0.0,0.0,0.0,0.0) [] afterTrs, m)

-- TODO need to fix accrue interest & cutoff stat
cutoffCashflow :: Date -> Dates -> CashFlowFrame -> CashFlowFrame
cutoffCashflow sd ds (CashFlowFrame st []) = CashFlowFrame st []
cutoffCashflow sd ds (CashFlowFrame st txns) 
  = let 
      futureTxns = cutBy Inc Future sd txns
      withBegTs [] =  []
      withBegTs (tr:trs) = buildBegTsRow sd tr: tr :trs 
      aggTxns = aggTsByDates (withBegTs futureTxns) ds
    in 
      CashFlowFrame (buildBegBal aggTxns, sd, Nothing) aggTxns 


extendTxns :: TsRow -> [Date] -> [TsRow]      
extendTxns tr ds = [ emptyTsRow d tr | d <- ds ]

-- test emtpy cashflow row
isEmptyRow :: TsRow -> Bool 
isEmptyRow (MortgageDelinqFlow _ 0 0 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow (MortgageFlow _ 0 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow (LoanFlow _ 0 0 0 0 0 0 0 i j ) = True
isEmptyRow (LeaseFlow _ 0 0 0) = True
isEmptyRow (FixedFlow _ 0 0 0 0 0) = True
isEmptyRow (BondFlow _ 0 0 0) = True
isEmptyRow (CashFlow _ 0) = True
isEmptyRow (ReceivableFlow _ 0 0 0 0 0 0 0 _ ) = True
isEmptyRow _ = False

-- test emtpy cashflow row (ignore balance)
isEmptyRow2 :: TsRow -> Bool 
isEmptyRow2 (MortgageDelinqFlow _ _ 0 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow2 (MortgageFlow _ _ 0 0 0 0 0 0 _ _ _ _) = True
isEmptyRow2 (LoanFlow _ _ 0 0 0 0 0 0 i j ) = True
isEmptyRow2 (LeaseFlow _ _ 0 _) = True
isEmptyRow2 (FixedFlow _ _ 0 0 0 0) = True
isEmptyRow2 (BondFlow _ _ 0 0) = True
isEmptyRow2 (CashFlow _ 0) = True
isEmptyRow2 (ReceivableFlow _ _ 0 0 0 0 0 0 _ ) = True
isEmptyRow2 _ = False

-- ^ Remove empty cashflow from the tail
dropTailEmptyTxns :: [TsRow] -> [TsRow]
dropTailEmptyTxns trs 
  = reverse $ dropWhile isEmptyRow (reverse trs)


cashflowTxn :: Lens' CashFlowFrame [TsRow]
cashflowTxn = lens getter setter
  where 
    getter (CashFlowFrame _ txns) = txns
    setter (CashFlowFrame st txns) newTxns = CashFlowFrame st newTxns

appendMCashFlow :: Maybe CashFlowFrame -> [TsRow] -> Maybe CashFlowFrame
appendMCashFlow Nothing [] = Nothing
appendMCashFlow (Just cf) [] = Just cf
appendMCashFlow Nothing txns 
  = Just $ CashFlowFrame (0, epocDate, Nothing) txns
appendMCashFlow (Just (CashFlowFrame st txns)) newTxns 
  = Just $ CashFlowFrame st (txns ++ newTxns)

txnCumulativeStats :: Lens' TsRow (Maybe CumulativeStat)
txnCumulativeStats = lens getter setter
  where 
    getter (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN mStat) = mStat
    getter (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN mStat) = mStat
    getter (LoanFlow d bal p i ppy def recovery loss rate mStat) = mStat
    getter (ReceivableFlow d bal p i ppy def recovery loss mStat) = mStat
    getter _ = Nothing
    
    setter (MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN _) mStat 
      = MortgageDelinqFlow d bal p i ppy delinq def recovery loss rate mB mPPN mStat
    setter (MortgageFlow d bal p i ppy def recovery loss rate mB mPPN _) mStat
      = MortgageFlow d bal p i ppy def recovery loss rate mB mPPN mStat
    setter (LoanFlow d bal p i ppy def recovery loss rate _) mStat
      = LoanFlow d bal p i ppy def recovery loss rate mStat
    setter (ReceivableFlow d bal p i ppy def recovery loss _) mStat
      = ReceivableFlow d bal p i ppy def recovery loss mStat
    setter x _ = x

$(deriveJSON defaultOptions ''TsRow)
$(deriveJSON defaultOptions ''CashFlowFrame)
