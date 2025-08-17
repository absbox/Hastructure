{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Receivable
  ()
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import DateUtil
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types

import AssetClass.AssetBase
import AssetClass.AssetCashflow
import Debug.Trace
import Assumptions (AssetPerfAssumption(ReceivableAssump))
import GHC.Float.RealFracMethods (truncateFloatInteger)
import Cashflow (extendTxns)
import qualified Asset as A

debug = flip trace

-- project recovery cashflow from recovery assumption and defaulted balance
buildRecoveryCfs :: StartDate -> Balance -> Maybe A.RecoveryAssumption -> Either String [CF.TsRow]
buildRecoveryCfs _ _ Nothing = Right []
buildRecoveryCfs sd defaultedBal (Just (A.RecoveryByDays r dists))
  = let 
      totalRecoveryAmt = mulBR defaultedBal r
      recoveryDistribution = snd <$>  dists
    in 
      case sum recoveryDistribution of
        1 -> let
               recoveryAmts =  mulBR totalRecoveryAmt <$> recoveryDistribution
               recoveryDates = (\x -> T.addDays (toInteger x)) <$> (fst <$> dists) <*> [sd]
               lossAmts = replicate (pred (length recoveryDates)) 0  ++ [defaultedBal - totalRecoveryAmt]
             in
               Right $ [ CF.ReceivableFlow d 0 0 0 0 0 amt lossAmt Nothing  | (amt,d,lossAmt) <- zip3 recoveryAmts recoveryDates lossAmts]
        _ -> Left $ "Recovery distribution does not sum up to 1, got " ++ show (sum recoveryDistribution) ++ " for " ++ show dists


calcDueFactorFee :: Receivable -> Date -> Balance
calcDueFactorFee r@(Invoice (ReceivableInfo sd ob oa dd ft obr) st) asOfDay
  = case ft of
      Nothing -> 0
      Just (FixedFee b) -> b 
      Just (FixedRateFee r) -> mulBR ob r
      Just (FactorFee r daysInPeriod rnd) -> 
        let 
          periods = case rnd of 
                      Up ->  ceiling ((fromIntegral (daysBetween sd dd)) / (fromIntegral daysInPeriod)) :: Int 
                      Down -> floor ((fromIntegral (daysBetween sd dd)) / (fromIntegral daysInPeriod)) :: Int  
        in 
          fromRational $ (toRational periods) * toRational (mulBR ob r) 
      Just (AdvanceFee r) -> mulBR oa (r  * (yearCountFraction DC_ACT_365F sd dd))
      Just (CompoundFee fs) -> 
        let 
          newReceivables = [ Invoice (ReceivableInfo sd ob oa dd (Just newFeeType) obr) st  | newFeeType <- fs] 
        in 
          sum $ (`calcDueFactorFee` asOfDay) <$> newReceivables


instance Asset Receivable where 

  getPaymentDates r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) _ = [dd]

  calcCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) asOfDay _ 
    = Right $ CF.CashFlowFrame (ob,asOfDay,Nothing) $ cutBy Inc Future asOfDay txns
    where
      payDate = dd
      feeDue = calcDueFactorFee r payDate
      initTxn = CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing

      feePaid = min ob feeDue
      principal = max 0 $ ob - feeDue

      txns = [initTxn,CF.ReceivableFlow payDate 0 0 principal feePaid 0 0 0 Nothing]

  getCurrentBal r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) = ob

  isDefaulted r@(Invoice (ReceivableInfo sd ob oa dd ft _) Current) = False
  isDefaulted r@(Invoice (ReceivableInfo sd ob oa dd ft _) _) = True

  getOriginDate r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) = sd

  resetToOrig r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) = r

  getRemainTerms r@(Invoice (ReceivableInfo sd ob oa dd ft _) st) = 1

  getOriginRate _ = 0
  getCurrentRate _ = 0

  updateOriginDate r@(Invoice (ReceivableInfo sd ob oa dd ft obr) st) newDate 
    = let 
        gaps = daysBetween sd dd
      in 
        Invoice (ReceivableInfo newDate ob oa (T.addDays gaps newDate) ft obr) st
    
  splitWith r@(Invoice (ReceivableInfo sd ob oa dd ft obr) st) rs 
    = [ Invoice (ReceivableInfo sd (mulBR ob ratio) (mulBR oa ratio) dd ft obr) st | ratio <- rs ]

  -- Defaulted Invoice
  projCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft _) (Defaulted _))
               asOfDay
               massump@(A.ReceivableAssump _ amr ams, _ , _)
               mRates
    = let
        payDate = dd
        initTxn = CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing
        txns = [initTxn, CF.ReceivableFlow asOfDay 0 0 0 0 ob 0 ob Nothing]
      in 
        do 
          txns' <- (patchLossRecovery txns amr)
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns' 
          return $ (CF.CashFlowFrame (ob,asOfDay,Nothing) futureTxns, historyM)


  -- Performing Invoice : default all balance at end of due date
  projCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft _) Current) 
               asOfDay
               massump@(A.ReceivableAssump (Just A.DefaultAtEnd) amr ams, _ , _)
               mRates
    = let 
        payDate = dd
        feeDue = calcDueFactorFee r payDate
        -- initTxn = [CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing]

        realizedLoss = case amr of
                        Nothing -> ob
                        Just _ -> 0
        txns = [CF.ReceivableFlow payDate 0 0 0 0 ob 0 realizedLoss Nothing]
      in
        do 
          recoveryFlow <- buildRecoveryCfs payDate ob amr
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay $ txns++recoveryFlow
          return $ (CF.CashFlowFrame (ob,asOfDay,Nothing) futureTxns, historyM)

  -- Performing Invoice : 
  projCashflow r@(Invoice (ReceivableInfo sd ob oa dd ft _) Current) 
               asOfDay
               massump@(A.ReceivableAssump amd amr ams, _ , _)
               mRates
    = let
        payDate = dd
        feeDue = calcDueFactorFee r payDate
        initTxn = CF.ReceivableFlow sd ob 0 0 0 0 0 0 Nothing
      in 
        do 
          defaultRates <- A.buildDefaultRates r (sd:[dd]) amd
          let defaultAmt = mulBR ob (head defaultRates)
          let afterDefaultBal =  ob - defaultAmt
          let afterDefaultFee =  mulBR feeDue (1 - head defaultRates)

          let feePaid = min afterDefaultBal afterDefaultFee
          let principal = max 0 $ afterDefaultBal - feePaid
      
          let realizedLoss = case amr of
                          Nothing -> defaultAmt
                          Just _ -> 0
      
          let txns = [initTxn, CF.ReceivableFlow payDate 0 0 principal feePaid defaultAmt 0 realizedLoss Nothing]
          recoveryFlow <- buildRecoveryCfs payDate defaultAmt amr
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay $ txns++recoveryFlow
          return $ (CF.CashFlowFrame (ob,asOfDay,Nothing) futureTxns, historyM)

  projCashflow a b c d = Left $ "Failed to match when proj receivable with assumption >>" ++ show a ++ show b ++ show c ++ show d
