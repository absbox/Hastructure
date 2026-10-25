{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hedge
  (RateSwap(..),RateCap(..)
  ,RateSwapType(..),RateSwapBase(..)
  ,accrueIRS,payoutIRS,receiveIRS,receiveRC
  ,CurrencySwap(..),rsRefBalLens,SRT(..),SrtType(..)
  )
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Map as Map
import GHC.Generics
import Language.Haskell.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import qualified Data.DList as DL
import Types
import Util
import Stmt
import DateUtil
import Interface

import qualified Assumptions as A
import qualified InterestRate as IR
import Control.Lens


type SettleDates = DatePattern       -- ^ dates when rates/ex-rates are reseted

data RateSwapBase = Fixed Balance    -- ^ a fixed balance as notional base 
                  | Base DealStats   -- ^ a referece as notional base
                  | Schedule Ts      -- ^ a predfiend schedule of notional balance
                  deriving(Show,Generic,Eq,Ord)

data RateSwapType = FloatingToFloating Floater Floater    -- ^ Paying Floating rate and receiving Floating Rate
                  | FloatingToFixed  Floater IRate        -- ^ Paying Floating Rate and receiving Fixed Rate
                  | FixedToFloating  IRate Floater        -- ^ Paying Fixed Rate and receiving Floating rate
                  | FormulaToFloating   DealStats Floater    -- ^ Paying Formula Rate and receiving Floating rate
                  | FloatingToFormula   Floater DealStats    -- ^ Paying Floating Rate and receiving Formula rate
                  deriving(Show,Generic,Eq,Ord)

data RateSwap = RateSwap {rsType :: RateSwapType         -- ^ swap type
                          ,rsDayCount :: DayCount        -- ^ day count convention
                          ,rsSettleDates :: Maybe (SettleDates,String)         -- ^ define settle dates
                          ,rsUpdateDates :: DatePattern   -- ^ define observe dates

                          ,rsNotional :: RateSwapBase     -- ^ define notional balance
                          ,rsRefBalance :: Balance        -- ^ notional balance in use
                          
                          ,rsPayingRate :: IRate          -- ^ collect rate
                          ,rsReceivingRate :: IRate       -- ^ paying rate
                          
                          ,rsNetCash :: Balance           -- ^ amount to pay/collect
                          
                          ,rsStartDate :: StartDate       -- ^ swap start date
                          ,rsLastStlDate :: Maybe Date    -- ^ last settle date
                          ,rsStmt :: Maybe Statement      -- ^ transaction history
                          }
                          deriving(Show,Generic,Eq,Ord)

-- | The `accrueIRS` will calculate the `Net` amount 
-- ( payble with negative, positve with receivable) of Rate Swap      
accrueIRS :: Date -> RateSwap -> RateSwap
accrueIRS d rs@RateSwap{rsRefBalance = face               
                      , rsPayingRate = payRate            
                      , rsReceivingRate = receiveRate     
                      , rsNetCash = netCash     
                      , rsDayCount = dc          
                      , rsStmt = stmt}                    
  = rs {rsNetCash = newNet , rsLastStlDate = Just d, rsStmt = appendStmt newTxn stmt}
      where 
          accureStartDate = case rsLastStlDate rs of 
                              Nothing ->  rsStartDate rs 
                              Just lsd -> lsd
          rateDiff =  receiveRate - payRate 
          yearFactor = fromRational $ yearCountFraction dc accureStartDate d
          newNetAmount = mulBIR (face * yearFactor) rateDiff  -- `debug` ("Diff rate"++ show rateDiff)
          newNet = netCash + newNetAmount
          newTxn = IrsTxn d face newNetAmount payRate receiveRate newNet SwapAccrue

-- | set rate swap to state of receive all cash from counterparty
receiveIRS :: Date -> RateSwap -> RateSwap 
receiveIRS d rs@RateSwap{rsNetCash = receiveAmt, rsStmt = stmt} 
  | receiveAmt > 0 = rs { rsNetCash = 0 ,rsStmt = appendStmt (IrsTxn d 0 receiveAmt 0 0 0 (SwapInSettle "")) stmt}
  | otherwise = rs

-- | set rate swap to state of payout all possible cash to counterparty
payoutIRS :: Date -> Amount -> RateSwap -> RateSwap 
payoutIRS d amt rs@RateSwap{rsNetCash = payoutAmt, rsStmt = stmt} 
  | payoutAmt < 0  =  rs { rsNetCash = outstanding, rsStmt = newStmt }
  | otherwise = rs
      where 
        actualAmt = min amt (negate payoutAmt)  --TODO need to add a check here
        outstanding = payoutAmt + actualAmt
        newStmt = appendStmt (IrsTxn d 0 actualAmt 0 0 0 (SwapOutSettle "")) stmt 

instance QueryByComment RateSwap where 
    queryStmt RateSwap{rsStmt = Nothing} tc = []
    queryStmt RateSwap{rsStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) (DL.toList txns)

instance Liable RateSwap where 
  isPaidOff rs@RateSwap{rsNetCash=bal}
    | bal == 0 = True
    | otherwise = False

  getOutstandingAmount rs@RateSwap{rsNetCash=bal} 
    | bal < 0 = negate bal
    | otherwise = 0

  -- ^ a swap is a net-settlement instrument: no principal balance, no interest accrual —
  --   the only balance is the net amount owed to/from the counterparty;
  --   the accrual rate is the current paying-leg rate
  getCurBalance rs = getOutstandingAmount rs
  getCurRate rs@RateSwap{rsPayingRate = r} = r
  getOriginBalance _ = 0
  getOriginDate rs@RateSwap{rsStartDate = d} = d
  getAccrueBegDate rs = case rsLastStlDate rs of
                          Just lsd -> lsd
                          Nothing -> rsStartDate rs
  getDueInt _ = 0
  getDueIntOverInt _ = 0
  getTotalDueInt _ = 0

data RateCap = RateCap {
                rcIndex :: Types.Index
                ,rcStrikeRate :: Ts
                ,rcNotional :: RateSwapBase
                ,rcStartDate :: Date
                ,rcSettleDates :: DatePattern
                ,rcEndDate :: Date
                ,rcReceivingRate :: IRate       -- ^ receiving rate
                ,rcLastStlDate :: Maybe Date    -- ^ last settle date
                ,rcNetCash :: Balance           -- ^ amount to collect
                ,rcStmt :: Maybe Statement      -- ^ transaction history                
              }
              deriving(Show,Generic,Eq,Ord)


receiveRC :: Date -> RateCap -> RateCap
receiveRC d rc@RateCap{rcNetCash = receiveAmt, rcStmt = stmt} 
  | receiveAmt > 0 = rc { rcNetCash = 0 ,rcStmt = appendStmt (IrsTxn d 0 receiveAmt 0 0 0 (SwapInSettle "")) stmt}
  | otherwise = rc

instance IR.UseRate RateCap where 
  getIndexes rc@RateCap{rcIndex = idx} = Just [idx]

  -- ^ a rate cap is always indexed to a rate index
  isAdjustableRate _ = True
  getIndex rc@RateCap{rcIndex = idx} = Just idx
  -- ^ reset/settle dates over the cap's life
  getResetDates rc@RateCap{rcStartDate = sd,rcSettleDates = dp,rcEndDate = ed} 
    = genSerialDatesTill2 NO_IE sd dp ed 
  getSpread _ = Nothing  -- ^ the index carries no spread

instance QueryByComment RateCap where 
    queryStmt RateCap{rcStmt = Nothing} tc = []
    queryStmt RateCap{rcStmt = Just (Statement txns)} tc
      = filter (\x -> getTxnComment x == tc) (DL.toList txns)


data CurrencySwap = CurrencySwap {
                    csBalance :: Balance
                    } deriving (Show,Generic,Ord,Eq)

instance IR.UseRate RateSwap where 
  getIndexes rs@RateSwap{rsType = rstype}
    = case rstype of
        FloatingToFloating (idx1,_) (idx2,_) -> Just [idx1,idx2]
        FloatingToFixed (idx1,_) _ -> Just [idx1]
        FixedToFloating _ (idx1,_) -> Just [idx1]
        _ -> Nothing

  -- ^ each swap type has at least one floating leg
  isAdjustableRate rs = case rsType rs of
                          FloatingToFloating _ _ -> True
                          FloatingToFixed _ _ -> True
                          FixedToFloating _ _ -> True
                          FormulaToFloating _ _ -> True
                          FloatingToFormula _ _ -> True

  getIndex rs = case IR.getIndexes rs of
                  Just (idx:_) -> Just idx
                  _ -> Nothing

  -- ^ the floating leg's spread (the only leg that carries one)
  getSpread rs = case rsType rs of
                   FloatingToFloating (_,spr) _ -> Just spr
                   FloatingToFixed (_,spr) _ -> Just spr
                   FixedToFloating _ (_,spr) -> Just spr
                   FormulaToFloating _ (_,spr) -> Just spr
                   FloatingToFormula (_,spr) _ -> Just spr

  getResetDates _ = [] -- ^ settle/reset dates are computed by the deal run from `rsSettleDates` with deal dates

makeLensesFor [("rsRefBalance","rsRefBalLens")] ''RateSwap

data SrtType = SrtByEndDay DealStats DatePattern  -- ^ autu accrue by end of day
              deriving(Show,Generic,Eq,Ord)


data SRT = SRT {
    srtName :: String 
    ,srtType :: SrtType 
    ,srtPremiumType :: IR.RateType              -- ^ define how/when to update the balance
    
    ,srtRefBalance :: Balance                   -- ^ balance to calc premium
    ,srtPremiumRate :: IRate                    -- ^ current interest rated on oustanding balance

    ,srtOpenBalance :: Balance                  -- ^ total open balance
    
    ,srtDuePremiumDate :: Maybe Date            -- ^ last day of interest/premium calculated
    ,srtDuePremium :: Balance                   -- ^ oustanding due on premium
    
    ,srtStart :: Date                           -- ^ when liquidiy provider came into effective
    ,srtEnds :: Maybe Date                      -- ^ when liquidiy provider came into expired
    ,srtStmt :: Maybe Statement                 -- ^ transaction history
} deriving (Show,Generic,Eq,Ord)

instance Liable SRT where 
  isPaidOff srt@SRT{srtOpenBalance=bal,srtDuePremium=duePremium}
    | bal==0 && duePremium==0 = True
    | otherwise = False

  -- ^ the SRT facility: open balance is the drawn/outstanding amount, premium accrues on
  --   the reference balance, the current rate is the premium rate
  getCurBalance srt@SRT{srtOpenBalance = bal} = bal
  getCurRate srt@SRT{srtPremiumRate = r} = r
  getOriginBalance srt@SRT{srtOpenBalance = bal} = bal
  getOriginDate srt@SRT{srtStart = d} = d
  getAccrueBegDate srt@SRT{srtDuePremiumDate = mLastCalcDate} 
    = fromMaybe (srtStart srt) mLastCalcDate
  getDueInt srt@SRT{srtDuePremium = amt} = amt
  getDueIntOverInt _ = 0
  getTotalDueInt srt@SRT{srtDuePremium = amt} = amt
  getOutstandingAmount srt@SRT{srtOpenBalance = bal,srtDuePremium = duePremium} = bal + duePremium

instance IR.UseRate SRT where 
  getIndexes srt@SRT{srtPremiumType = rt} 
    = case rt of 
        (IR.Floater _ idx _ _ _ _ _ _ ) -> Just [idx]
        _ -> Nothing

  isAdjustableRate srt@SRT{srtPremiumType = rt} 
    = case rt of 
        (IR.Floater {}) -> True
        _ -> False

  getIndex srt = case IR.getIndexes srt of
                   Just (idx:_) -> Just idx
                   _ -> Nothing

  getSpread srt@SRT{srtPremiumType = rt} = IR._getSpread rt
  
  getResetDates srt@SRT{srtPremiumType = rt , srtStart = sd, srtEnds = Just ed} 
    = case rt of 
        (IR.Floater _ _ _ _ dp _ _ _ ) -> genSerialDatesTill2 EI sd dp ed
        _ -> []





$(concat <$> traverse (deriveJSON defaultOptions) [''RateSwap, ''RateCap, ''RateSwapType, ''RateSwapBase, ''CurrencySwap])