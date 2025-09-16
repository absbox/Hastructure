{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.AssetCashflow
  (applyHaircut,patchPrepayPenaltyFlow,getRecoveryLag,decreaseBorrowerNum
  ,patchLossRecovery,getRecoveryLagFromAssumption)
  where

import qualified Data.Time as T
import qualified Cashflow as CF 
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

import Debug.Trace
import qualified Assumptions as A 
import GHC.Float.RealFracMethods (truncateFloatInteger)
import Cashflow (mflowDefault)
debug = flip trace

-- This module is a collection of common cashflow functions to project cashflow for different asset types.

-- ^ apply haircut to pool cashflow, reduce cash via a percentage
applyHaircut :: Maybe A.ExtraStress -> CF.CashFlowFrame -> CF.CashFlowFrame
applyHaircut Nothing cf = cf 
applyHaircut (Just A.ExtraStress{A.poolHairCut = Nothing}) cf = cf
applyHaircut (Just A.ExtraStress{A.poolHairCut = Just haircuts}) (CF.CashFlowFrame st txns)
  = CF.CashFlowFrame st $ 
      (\txn -> foldr 
                 (\fn acc -> fn acc ) 
                 txn 
                 (applyHaircutTxn <$> haircuts) ) <$> txns
    where
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin (mulBR interest (1-r)) ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal (mulBR prin (1-r)) interest ppy delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest (mulBR ppy (1-r)) delinq def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn mppn mst) 
        = CF.MortgageDelinqFlow d bal prin interest ppy delinq def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn (CollectedInterest,r) 
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin (mulBR interest (1-r)) ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrincipal,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal (mulBR prin (1-r)) interest ppy def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedRecoveries,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest ppy def (mulBR recovery (1-r)) loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepayment,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst) 
        = CF.MortgageFlow d bal prin interest (mulBR ppy (1-r)) def recovery loss irate mbn mppn mst
      applyHaircutTxn (CollectedPrepaymentPenalty,r)
                      (CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn mppn mst)
        = CF.MortgageFlow d bal prin interest ppy def recovery loss irate mbn ((\x -> mulBR x (1-r) ) <$> mppn) mst
      
      applyHaircutTxn _ _ = error "Not implemented"
   
-- ^ apply a penalty cashflow
patchPrepayPenaltyFlow :: (Int,Maybe PrepayPenaltyType) -> CF.CashFlowFrame -> CF.CashFlowFrame
patchPrepayPenaltyFlow (ot,mPpyPen) mflow@(CF.CashFlowFrame st trs) 
  = let 
      --(startDate,endDate) = CF.getDateRangeCashFlowFrame mflow
      prepaymentFlow = CF.mflowPrepayment <$> trs
      flowSize = CF.sizeCashFlowFrame mflow
    in 
      case mPpyPen of 
        Nothing -> mflow
        Just (ByTerm cutoff rate0 rate1) -> 
          let 
            rs = lastN flowSize $ replicate cutoff rate0 ++ replicate (ot-cutoff) rate1
          in 
            CF.CashFlowFrame st $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (FixAmount amt mCutoff) -> 
          let 
            projFlow = case mCutoff of 
                         Nothing -> replicate flowSize amt
                         Just cutoff -> lastN flowSize $ replicate cutoff amt ++ replicate (ot-cutoff) 0 
            actFlow = [ if ppy > 0 then 
                          f
                        else
                          0
                        | (f,ppy) <- zip projFlow prepaymentFlow]
          in 
            CF.CashFlowFrame st $ CF.setPrepaymentPenaltyFlow actFlow trs
        Just (FixPct r mCutoff) ->
          let 
            rs = case mCutoff of 
                   Nothing -> replicate flowSize r
                   Just cutoff -> lastN flowSize $ replicate cutoff r ++ replicate (ot-cutoff) 0
          in
            CF.CashFlowFrame st $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (Sliding sr changeRate) -> 
          let 
            rs = lastN flowSize $ paddingDefault 0 (0:[sr,(sr-changeRate)..0]) ot
          in
            CF.CashFlowFrame st $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs
        Just (StepDown ps) ->
          let 
            rs = lastN flowSize $ paddingDefault 0 (concat [ replicate n r | (n,r) <- ps]) ot
          in 
            CF.CashFlowFrame st $ CF.setPrepaymentPenaltyFlow (zipWith mulBR prepaymentFlow rs) trs

getRecoveryLag :: A.RecoveryAssumption -> Int
getRecoveryLag (A.Recovery (_,lag)) = lag 
getRecoveryLag (A.RecoveryTiming (_,rs)) = length rs

getRecoveryLagFromAssumption :: A.AssetPerfAssumption -> Maybe Int
getRecoveryLagFromAssumption (A.MortgageAssump _ _ (Just ra) _) = Just $ getRecoveryLag ra
getRecoveryLagFromAssumption (A.MortgageDeqAssump _ _ (Just ra) _) = Just $ getRecoveryLag ra
getRecoveryLagFromAssumption (A.LoanAssump _ _ (Just ra) _) = Just $ getRecoveryLag ra
getRecoveryLagFromAssumption (A.InstallmentAssump _ _ (Just ra) _) = Just $ getRecoveryLag ra
getRecoveryLagFromAssumption (A.ReceivableAssump _ (Just ra) _) = Just $ getRecoveryLag ra
getRecoveryLagFromAssumption _ = Nothing


decreaseBorrowerNum :: Balance -> Balance -> Maybe BorrowerNum -> Maybe Int
decreaseBorrowerNum bb 0 mBn = Nothing
decreaseBorrowerNum bb eb mBn 
  = case mBn of
      Nothing -> Nothing::(Maybe BorrowerNum)
      Just 0  -> Nothing::(Maybe BorrowerNum)
      Just bn -> Just $ round $ fromRational $ mulIR bn downRate::(Maybe BorrowerNum)
    where 
      downRate = if eb == 0 then 
                   0.0
                 else
                   divideBB eb bb

-- | given a list of future cashflows and patch recovery & loss
patchLossRecovery :: [CF.TsRow] -> Maybe A.RecoveryAssumption -> Either ErrorRep [CF.TsRow]
patchLossRecovery trs Nothing 
  = let 
      defaultVec = mflowDefault <$> trs
    in 
      return $ CF.dropTailEmptyTxns $ [ CF.tsSetRecovery 0 (CF.tsSetLoss d r) | (d,r) <- zip defaultVec trs ] -- `debug` ("Hit Nothign on recovery"++ show defaultVec)

-- ^ make sure trs has empty rows with length=lag. as it drop extended rows
patchLossRecovery trs (Just (A.Recovery (rr,lag)))
  | rr > 1.0 = Left $ "Recovery rate cannot be greater than 1.0:"++ show rr 
  | lag < 0 = Left $ "Recovery lag cannot be negative:"++ show lag
  | otherwise 
    = let 
      defaultVec = mflowDefault <$> trs
      recoveriesVec = (`mulBR` rr) <$> defaultVec -- `debug` ("Default Vec"++ show defaultVec)
      recoveryAfterLag = replicate lag 0.0 ++ recoveriesVec --  `debug` ("recovery"++ show recoveriesVec)
      lossVec = (`mulBR` (1-rr)) <$> defaultVec  --  `debug` ("Rec after lag"++ show recoveryAfterLag)
      lossVecAfterLag = replicate lag 0.0 ++ lossVec  -- drop last lag elements
    in 
      return $ CF.dropTailEmptyTxns $ [ CF.tsSetRecovery recovery (CF.tsSetLoss loss r) | (r,recovery,loss) <- zip3 trs recoveryAfterLag lossVecAfterLag]
patchLossRecovery trs (Just (A.RecoveryTiming (rr,recoveryTimingDistribution)))
  | rr > 1.0 = Left $ "Recovery rate cannot be greater than 1.0:"++ show rr
  | sum recoveryTimingDistribution /= 1.0 = Left $ "Recovery timing distribution must sum to 1.0:" ++ show recoveryTimingDistribution ++ "sum"++ show (sum recoveryTimingDistribution)
  | any (<0) recoveryTimingDistribution = Left $ "Recovery timing distribution cannot have negative values:" ++ show recoveryTimingDistribution
  | otherwise 
    = let
      cfLength = length trs -- cashflow length
      rLength = length recoveryTimingDistribution  -- recovery length
      defaultVec = mflowDefault <$> trs  -- default balance of each row

      rs = (rr *) <$> recoveryTimingDistribution 

      recoveriesVec = [ mulBR defaultVal <$> rs  | defaultVal <- defaultVec ] 
      
      offsets = [0..(length defaultVec - rLength)]
      
      paddedRecoveries = [ paddingDefault 0 (replicate prePadding 0 ++ recVal) cfLength 
                          | (prePadding,recVal) <- zip offsets recoveriesVec ]

      sumRecovery = sum <$> transpose paddedRecoveries
      lossVec = [ mulBR defaultVal (1-rr) | defaultVal <- defaultVec ]
      sumLoss = replicate (pred rLength) 0.0 ++ lossVec
    in 
      return $ CF.dropTailEmptyTxns $ [ CF.tsSetRecovery recVal (CF.tsSetLoss loss r) | (recVal,loss,r) <- zip3 sumRecovery sumLoss trs ]
