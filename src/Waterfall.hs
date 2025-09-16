{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Waterfall
  (PoolSource(..),Action(..),DistributionSeq(..)
  ,ActionWhen(..),BookType(..),ExtraSupport(..),PayOrderBy(..))
  where

import Language.Haskell.TH
import Data.Aeson hiding (json)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable
import Data.Fixed
import GHC.Generics

import Types
import Revolving
import Stmt (TxnComment(..))
import qualified Lib as L
import qualified Call as C
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import Ledger (LedgerName)


data BookType 
  -- | book to a list of ledgers by order
  = PDL BookDirection DealStats [(LedgerName,DealStats)] 
  -- | book to a ledger by an amount defined by a formula
  | ByDS         LedgerName BookDirection DealStats     
  -- | book to a ledger till an amount defined by a formula
  | Till         LedgerName BookDirection DealStats
  deriving (Show,Generic,Eq,Ord)

data ExtraSupport
  -- | if there is deficit, draw another account to pay the shortfall
  = SupportAccount AccountName (Maybe BookLedger)  
  -- | if there is deficit, draw facility's available credit to pay the shortfall
  | SupportLiqFacility CE.LiquidityProviderName                        
  -- | if there is deficit, draw multiple supports (by sequence in the list) to pay the shortfall
  | MultiSupport [ExtraSupport]                                     
  -- | support only available if Pre is true
  | WithCondition Pre ExtraSupport                                  
  deriving (Show,Generic,Eq,Ord)

data PayOrderBy 
  -- | pay order by name alphabetically
  = ByName 
  -- | pay order by oustanding balance
  | ByProRataCurBal
  -- | pay order by current interest rate
  | ByCurrentRate
  -- | pay order by maturity date
  | ByMaturity
  -- | pay order by start date
  | ByStartDate
  -- | pay order by Custom Names list
  | ByCustomNames [String]
  -- | pay order by in reverse
  | ReverseSeq PayOrderBy
  deriving (Show,Generic,Eq,Ord)

type BookLedger = (BookDirection, LedgerName) 
type BookLedgers = (BookDirection, [LedgerName]) 

data Action 
  -- Accounts 
  = Transfer (Maybe Limit) AccountName AccountName (Maybe TxnComment)
  | TransferAndBook (Maybe Limit) AccountName AccountName BookLedger (Maybe TxnComment)
  | TransferMultiple [(Maybe Limit, AccountName)] AccountName (Maybe TxnComment)
  -- Fee
  | CalcFee [FeeName]                                                            -- ^ calculate fee due amount in the fee names
  | PayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)              -- ^ pay fee with cash from account with optional limit or extra support
  | PayFeeBySeq (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)         -- ^ pay fee with cash from account with optional limit or extra support
  | CalcAndPayFee (Maybe Limit) AccountName [FeeName] (Maybe ExtraSupport)       -- ^ combination of CalcFee and PayFee
  | PayFeeResidual (Maybe Limit) AccountName FeeName                             -- ^ pay fee regardless fee due amount
  -- Bond - Interest
  | CalcBondInt [BondName]
  | CalcBondIntBy BondName DealStats DealStats                   -- ^ calculate interest due amount in the bond names,with optional balance and rate
  | PayIntOverInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)      -- ^ pay interest over interest only  
  | PayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)             -- ^ pay interest with cash from the account with optional limit or extra support
  | PayIntAndBook (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) BookLedger -- ^ pay interest with cash from the account with optional limit or extra support
  | PayIntBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)        -- ^ with sequence
  | PayIntOverIntBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) -- ^ pay interest over interest only with sequence
  | AccrueAndPayInt (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)    -- ^ combination of CalcInt and PayInt
  | AccrueAndPayIntBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) -- ^ with sequence
  | PayIntResidual (Maybe Limit) AccountName BondName                            -- ^ pay interest to bond regardless interest due
  | PayIntByRateIndex (Maybe Limit) AccountName [BondName] Int (Maybe ExtraSupport)      -- ^ pay interest to bond by index
  | PayIntByRateIndexBySeq (Maybe Limit) AccountName [BondName] Int (Maybe ExtraSupport)      -- ^ pay interest to bond by index
  -- Bond - Principal
  | CalcBondPrin (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)        -- ^ calculate principal due amount in the bond names
  | CalcBondPrin2 (Maybe Limit) [BondName]                                        -- ^ calculate principal due amount in the bond names
  | PayPrinWithDue AccountName [BondName] (Maybe ExtraSupport)                    -- ^ pay principal to bond till due amount
  | PayPrin (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)             -- ^ pay principal to bond via pro-rata
  | PayPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)        -- ^ pay principal to bond via sequence
  | PayPrinResidual AccountName [BondName]                                        -- ^ pay principal regardless predefined balance schedule
  | PayIntPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport)     -- ^ pay int & prin to bonds sequentially
  | AccrueAndPayIntPrinBySeq (Maybe Limit) AccountName [BondName] (Maybe ExtraSupport) 
  -- Bond Group 
  | PayPrinGroup (Maybe Limit) AccountName BondName PayOrderBy (Maybe ExtraSupport) -- ^ pay bond group with cash from account with optional limit or extra support
  | AccrueIntGroup [BondName]
  | PayIntGroup (Maybe Limit) AccountName BondName PayOrderBy (Maybe ExtraSupport)  -- ^ pay bond group with cash from account with optional limit or extra support
  | AccrueAndPayIntGroup (Maybe Limit) AccountName BondName PayOrderBy (Maybe ExtraSupport) 
  -- Bond - Balance
  | WriteOff (Maybe Limit) BondName
  | WriteOffAndBook (Maybe Limit) BondName BookLedger
  | WriteOffBySeq (Maybe Limit) [BondName]
  | WriteOffBySeqAndBook (Maybe Limit) [BondName] BookLedger
  | FundWith (Maybe Limit) AccountName BondName             -- ^ extra more funds from bond and deposit cash to account
  -- Pool/Asset change
  | BuyAsset (Maybe Limit) PricingMethod AccountName (Maybe PoolId)                       -- ^ buy asset from revolving assumptions using funds from account
  | BuyAssetFrom (Maybe Limit) PricingMethod AccountName (Maybe String) (Maybe PoolId)    -- ^ buy asset from specific pool, with revolving assumptions using funds from account
  | LiquidatePool PricingMethod AccountName  (Maybe [PoolId])                             -- ^ sell all assets and deposit proceeds to account
  -- TODO include a limit for LIquidatePool
  -- Liquidation support
  | LiqSupport (Maybe Limit) CE.LiquidityProviderName CE.LiqDrawType [String]  -- ^ draw credit and deposit to account/fee/bond interest/principal
  | LiqRepay (Maybe Limit) CE.LiqRepayType AccountName CE.LiquidityProviderName   -- ^ repay liquidity facility
  | LiqYield (Maybe Limit) AccountName CE.LiquidityProviderName                   -- ^ repay compensation to liquidity facility
  | LiqAccrue [CE.LiquidityProviderName]                                            -- ^ accure premium/due interest of liquidity facility
  -- Rate Swap
  | SwapAccrue CeName                 -- ^ calculate the net amount of swap manually
  | SwapReceive AccountName CeName    -- ^ receive amount from net amount of swap and deposit to account
  | SwapPay AccountName CeName        -- ^ pay out net amount from account 
  | SwapSettle AccountName CeName     -- ^ pay & receive net amount of swap with account
  -- RateCap 
  | CollectRateCap AccountName CeName  -- ^ collect cash from rate cap and deposit to account
  -- Record booking
  | BookBy BookType                         -- ^ book an ledger with book types
  -- Pre
  | ActionWithPre Pre [Action]            -- ^ execute actions if <pre> is true 
  | ActionWithPre2 Pre [Action] [Action]  -- ^ execute action1 if <pre> is true ,else execute action2 
  -- Trigger
  | RunTrigger DealCycle [String]        -- ^ update the trigger status during the waterfall execution
  -- Debug
  | WatchVal (Maybe String) [DealStats]  -- ^ inspect vals during the waterfall execution
  | Placeholder (Maybe String)
  | ChangeStatus (Maybe Pre) DealStatus  -- ^ change deal status
  -- Accrue Deal
  | AccrueDeal [String]
  deriving (Show,Generic,Eq,Ord)

type DistributionSeq = [Action]

$(deriveJSON defaultOptions ''BookType)
$(deriveJSON defaultOptions ''ExtraSupport)
$(deriveJSON defaultOptions ''PayOrderBy)
$(deriveJSON defaultOptions ''Action)
