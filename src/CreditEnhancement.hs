{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditEnhancement
  (LiqFacility(..),LiqSupportType(..),buildLiqResetAction,buildLiqRateResetAction
  ,LiquidityProviderName,draw,repay,accrueLiqProvider
  ,LiqDrawType(..),LiqRepayType(..),LiqCreditCalc(..)
  ,consolStmt,CreditDefaultSwap(..),CDSType(..)
  )
  where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Map as Map
import qualified Data.DList as DL
import GHC.Generics
import Language.Haskell.TH
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import Types
import Util
import DateUtil
import Stmt
import qualified InterestRate as IR

import qualified Stmt as S

import Debug.Trace
import Lib (paySeqLiabilities)
import Data.Decimal
debug = flip trace

type LiquidityProviderName = String

-- ^ describle credit support 
data LiqSupportType = ReplenishSupport DatePattern Balance    -- ^ Credit will be refresh by an interval
                    | FixSupport Balance                      -- ^ Fixed credit amount
                    | ByPct DealStats Rate                    -- ^ By a pct of formula
                    | UnLimit                                 -- ^ Unlimit credit support, like insurance company
                    deriving(Show,Generic,Eq,Ord)


data LiqDrawType = LiqToAcc        -- ^ draw credit and deposit cash to account
                 | LiqToBondInt    -- ^ draw credit and pay to bond interest if any shortfall
                 | LiqToBondPrin   -- ^ draw credit and pay to bond principal if any shortfall
                 | LiqToFee        -- ^ draw credit and pay to a fee if there is a shortfall
                 deriving (Show,Generic,Ord,Eq)


data LiqRepayType = LiqBal         -- ^ repay oustanding balance of liquidation provider
                  | LiqPremium     -- ^ repay oustanding premium fee of lp
                  | LiqInt         -- ^ repay oustanding interest of lp
                  | LiqRepayTypes [LiqRepayType]  -- ^ repay by sequence
                  | LiqResidual    
                  | LiqOD
                  deriving (Show,Generic,Ord,Eq)

data LiqCreditCalc = IncludeDueInt 
                   | IncludeDuePremium 
                   | IncludeBoth
                   deriving (Show,Generic,Ord,Eq)


data LiqFacility = LiqFacility {
    liqName :: String 
    ,liqType :: LiqSupportType 
    ,liqBalance :: Balance                   -- ^ total balance supported/drawed
    ,liqCredit :: Maybe Balance              -- ^ available balance to support. Nothing -> unlimit 
    ,liqCreditCalc :: Maybe LiqCreditCalc    -- ^ how to calculate credit
    
    ,liqRateType :: Maybe IR.RateType        -- ^ interest rate type 
    ,liqPremiumRateType :: Maybe IR.RateType -- ^ premium rate type
    
    ,liqRate :: Maybe IRate                  -- ^ current interest rated on oustanding balance
    ,liqPremiumRate :: Maybe IRate           -- ^ current premium rate used on unused credit, a.k. commitment fee
    
    ,liqDueIntDate :: Maybe Date             -- ^ last day of interest/premium calculated
    
    ,liqDueInt :: Balance                    -- ^ oustanding due on interest
    ,liqDuePremium :: Balance                -- ^ oustanding due on premium
    
    ,liqStart :: Date                        -- ^ when liquidiy provider came into effective
    ,liqEnds :: Maybe Date                   -- ^ when liquidiy provider came into expired
    ,liqStmt :: Maybe Statement              -- ^ transaction history
} deriving (Show,Generic,Eq,Ord)

consolStmt :: LiqFacility -> LiqFacility
consolStmt liq@LiqFacility{liqStmt = Nothing} = liq
consolStmt liq@LiqFacility{liqStmt = Just (S.Statement txn')} 
  | DL.empty == txn' = liq
  | otherwise = let 
                  (txn:txns) = DL.toList txn'
                  combinedBondTxns = foldl S.consolTxn [txn] txns    
                  droppedTxns = dropWhile S.isEmptyTxn combinedBondTxns 
                in 
                  liq {liqStmt = Just (S.Statement (DL.fromList (reverse droppedTxns)))}


-- | update the reset events of liquidity provider
buildLiqResetAction :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqResetAction [] ed r = r
buildLiqResetAction (liqProvider:liqProviders) ed r = 
  case liqProvider of 
    (LiqFacility lqName (ReplenishSupport dp bal) _ _ _ _ _ _ _ _ _ _ ss _ _) -- update the support credit of liquidity provider
      -> buildLiqResetAction
           liqProviders
           ed
           [(lqName, projDatesByPattern dp ss ed)]++r
    _ -> buildLiqResetAction liqProviders ed r


-- | update the rate reset events of liquidity provider
buildLiqRateResetAction  :: [LiqFacility] -> Date -> [(String, Dates)] -> [(String, Dates)]
buildLiqRateResetAction [] ed r = r
buildLiqRateResetAction (liq:liqProviders) ed r = 
  case liq of 
    liq@LiqFacility{liqRateType = rt, liqPremiumRateType = prt, liqName = ln , liqStart = sd} -> 
       buildLiqRateResetAction 
        liqProviders 
        ed 
        [(ln,IR.getRateResetDates sd ed rt ++ IR.getRateResetDates sd ed prt)]++r
    _ -> buildLiqRateResetAction liqProviders ed r


-- | draw cash from liquidity provider
draw :: Amount -> Date -> LiqFacility -> LiqFacility
draw  amt d liq@LiqFacility{ liqBalance = liqBal
                            ,liqStmt = mStmt
                            ,liqCredit = mCredit
                            ,liqDueInt = dueInt 
                            ,liqDuePremium = duePremium} 
  | isJust mCredit && (fromMaybe 0 mCredit) <= 0 = 
    liq { liqStmt = appendStmt (SupportTxn d mCredit liqBal dueInt duePremium 0 LiquidationDraw) mStmt }
  | otherwise = liq { liqBalance = newBal,liqCredit = newCredit,liqStmt = newStmt}
    where 
        newCredit = (\x -> x - amt) <$> mCredit 
        newBal = liqBal + amt 
        newStmt = appendStmt (SupportTxn d newCredit  newBal dueInt duePremium (negate amt) LiquidationDraw) mStmt


repay :: Amount -> Date -> LiqRepayType -> LiqFacility -> LiqFacility
repay amt d pt liq@LiqFacility{liqBalance = liqBal
                              ,liqStmt = mStmt 
                              ,liqCredit = mCredit
                              ,liqCreditCalc = mCreditType
                              ,liqDueInt = liqDueInt
                              ,liqDuePremium = liqDuePremium
                              ,liqType = lt} 
  = liq {liqBalance = newBal ,liqCredit = newCredit ,liqDueInt = newIntDue
         ,liqDuePremium = newDuePremium ,liqStmt = newStmt}
    where 
      (newBal, newIntDue, newDuePremium) = 
        case pt of 
          LiqBal -> ( liqBal - amt, liqDueInt, liqDuePremium )
          LiqPremium -> ( liqBal , liqDueInt,   liqDuePremium  - amt )
          LiqInt -> ( liqBal , max 0 (liqDueInt - amt), liqDuePremium )
          _ -> ( liqBal, liqDueInt, liqDuePremium )

      newCredit = case (mCreditType,pt) of
                    (_ , LiqOD) -> (+ amt) <$> mCredit
                    (Nothing, _) -> mCredit
                    (Just IncludeDueInt, LiqInt) -> (+ amt) <$> mCredit
                    (Just IncludeDuePremium, LiqPremium) -> (+ amt) <$> mCredit
                    (Just IncludeBoth, LiqInt) -> (+ amt) <$> mCredit
                    (Just IncludeBoth, LiqPremium) -> (+ amt) <$> mCredit
                    _ -> mCredit

      newStmt = appendStmt (SupportTxn d newCredit newBal newIntDue newDuePremium amt  (LiquidationRepay (show pt))) mStmt  

-- | accure fee and interest of a liquidity provider and update credit available
accrueLiqProvider ::  Date -> LiqFacility -> LiqFacility
accrueLiqProvider d liq@(LiqFacility _ _ curBal mCredit _ mRateType mPRateType rate prate dueDate dueInt duePremium sd mEd Nothing)
  = accrueLiqProvider d $ liq{liqStmt = Just defaultStmt} 
    where 
      -- insert begining record
      defaultStmt = Statement $ DL.singleton $ SupportTxn sd mCredit curBal dueInt duePremium 0 Empty

accrueLiqProvider d liq@(LiqFacility _ _ curBal mCredit mCreditType mRateType mPRateType rate prate dueDate dueInt duePremium sd mEd mStmt@(Just (Statement txns)))
  = liq { liqStmt = newStmt
         ,liqDueInt = newDueInt
         ,liqDuePremium = newDueFee
         ,liqCredit = newCredit 
         ,liqDueIntDate = Just d
         }
    where 
      lastAccDate = fromMaybe sd dueDate
      accureInt = case rate of 
                    Nothing -> 0
                    Just r -> 
                      let 
                        bals = weightAvgBalanceByDates [lastAccDate,d] (DL.toList txns)
                      in 
                        sum $ flip mulBIR r <$> bals -- `debug` ("Accure Using Rate"++show r++"avg bal"++ show bals ++"ds"++show [lastAccDate,d])
      accureFee = case prate of
                    Nothing -> 0 
                    Just r -> 
                      let 
                        (_,_unAccTxns) = splitByDate (DL.toList txns) lastAccDate EqToLeftKeepOne
                        accBals = getUnusedBal <$> _unAccTxns 
                        _ds = lastAccDate : tail (getDate <$> _unAccTxns)
                        _avgBal = calcWeightBalanceByDates DC_ACT_365F accBals (_ds++[d])
                      in 
                        mulBIR _avgBal r
                        
      getUnusedBal (SupportTxn _ b _ _ _ _ _) = fromMaybe 0 b 
      
      newDueFee = accureFee + duePremium
      newDueInt = accureInt + dueInt
      newCredit = case mCreditType of 
                    Nothing -> mCredit
                    Just IncludeDueInt -> (\x -> x - accureInt) <$> mCredit
                    Just IncludeDuePremium -> (\x -> x - accureFee) <$> mCredit
                    Just IncludeBoth -> (\x -> x - accureInt - accureFee) <$> mCredit

      newStmt = appendStmt (SupportTxn d newCredit curBal newDueInt newDueFee 0 (LiquidationSupportInt accureInt accureFee)) mStmt 


instance QueryByComment LiqFacility where 
    queryStmt liq@LiqFacility{liqStmt = Nothing} tc = []
    queryStmt liq@LiqFacility{liqStmt = (Just (Statement txns))} tc
      = filter (\x -> getTxnComment x == tc) (DL.toList txns)


instance Liable LiqFacility where 
  isPaidOff liq@LiqFacility{liqBalance=bal,liqDueInt=dueInt,liqDuePremium=duePremium}
    | bal==0 && dueInt==0 && duePremium==0 = True
    | otherwise = False

  getCurBalance LiqFacility{liqBalance = bal} = bal

  getDueInt LiqFacility{liqDueInt = dueInt} = dueInt

  getOutstandingAmount LiqFacility{liqBalance = bal,liqDueInt = dueInt,liqDuePremium = duePremium} = bal + dueInt + duePremium

  getOriginBalance LiqFacility{liqBalance = bal} = 0 

instance IR.UseRate LiqFacility where 
  getIndexes liq@LiqFacility{liqRateType = mRt,liqPremiumRateType = mPrt} 
    = case (mRt,mPrt) of 
        (Nothing, Nothing) -> Nothing
        (Just (IR.Floater _ idx _ _ _ _ _ _), Nothing ) -> Just [idx]
        (Nothing, Just (IR.Floater _ idx _ _ _ _ _ _)) -> Just [idx]
        (Just (IR.Floater _ idx1 _ _ _ _ _ _), Just (IR.Floater _ idx2 _ _ _ _ _ _)) -> Just [idx1,idx2]
        _ -> Nothing

  isAdjustbleRate liq@LiqFacility{liqRateType = mRt,liqPremiumRateType = mPrt} 
    = case (mRt,mPrt) of 
        (Just (IR.Floater {}), _ ) -> True
        (_, Just (IR.Floater {})) -> True
        _ -> False

  getIndex liq = head <$> IR.getIndexes liq


data CDSType = CoverageAttachDetach Balance Balance
              | CoverageAttach Balance
              | CoverageDetach Balance
              | AllCoverage
              deriving (Show, Generic, Eq, Ord)

data CreditDefaultSwap = CDS {
    cdsName :: String
    ,cdsType :: CDSType

    ,cdsInsure ::  DealStats     -- ^ the coverage 
    ,cdsCollectDue :: Balance    -- ^ the amount to collect from CDS

    ,cdsPremiumRefBalance :: DealStats  -- ^ how notional balance is calculated
    ,cdsPremiumNotional :: Balance      -- ^ the balance to calculate premium

    ,cdsPremiumRate :: IRate            -- ^ the rate to calculate premium
    ,cdsRateType :: Maybe IR.RateType   -- ^ interest rate type 
    
    ,cdsPremiumDue :: Balance           -- ^ the due premium to payout from SPV

    ,cdsLastCalcDate :: Maybe Date     -- ^ last calculate date on net cash 

    ,cdsSettleDate :: Maybe Date       -- ^ last setttle date on net cash 
    ,cdsNetCash :: Balance             -- ^ the net cash to settle ,negative means SPV pay to CDS, positive means CDS pay to SPV

    ,cdsStart :: Date
    ,cdsEnds :: Maybe Date
    ,cdsStmt :: Maybe Statement
}  deriving (Show, Generic, Eq, Ord)




$(deriveJSON defaultOptions ''LiqRepayType)
$(deriveJSON defaultOptions ''LiqDrawType)
$(deriveJSON defaultOptions ''LiqSupportType)
$(deriveJSON defaultOptions ''LiqCreditCalc)
$(deriveJSON defaultOptions ''LiqFacility)
