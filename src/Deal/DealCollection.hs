{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Deal.DealCollection
  ( depositInflow
  , depositPoolFlow
  , readProceeds
  , extractTxnsFromFlowFrameMap
  , CollectionRule(..)
  ) where

import GHC.Generics

import Data.Aeson.TH
import Data.Aeson.Types

import qualified Accounts as A
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Data.Map as Map hiding (mapEither)

import Data.List
import Control.Monad
import Types
import Util
import Lib
import Control.Lens hiding (element)

data CollectionRule = Collect (Maybe [PoolId]) PoolSource AccountName                   -- ^ collect a pool source from pool collection and deposit to an account
                    | CollectByPct (Maybe [PoolId]) PoolSource [(Rate,AccountName)]     -- ^ collect a pool source from pool collection and deposit to multiple accounts with percentages
                    deriving (Show,Generic,Eq,Ord)


readProceeds :: PoolSource -> CF.TsRow -> Either ErrorRep Balance
readProceeds CollectedInterest x = return $ CF.mflowInterest x
readProceeds CollectedPrincipal x = return $ CF.mflowPrincipal x
readProceeds CollectedRecoveries x = return $ CF.mflowRecovery x
readProceeds CollectedPrepayment x = return $ CF.mflowPrepayment x
readProceeds CollectedRental  x    = return $ CF.mflowRental x
readProceeds CollectedPrepaymentPenalty x = return $ CF.mflowPrepaymentPenalty x
readProceeds CollectedCash x = return $ CF.tsTotalCash x
readProceeds CollectedFeePaid x = return $ CF.mflowFeePaid x
readProceeds a _ = Left $ " Failed to find pool cashflow field from pool cashflow rule "++show a


extractTxnsFromFlowFrameMap :: Maybe [PoolId] -> Map.Map PoolId CF.PoolCashflow -> [CF.TsRow]
extractTxnsFromFlowFrameMap mPids pflowMap = 
  let 
    extractTxns :: Map.Map PoolId CF.PoolCashflow -> [CF.TsRow]
    extractTxns m = concatMap (view (_1 . CF.cashflowTxn)) $ Map.elems m
  in 
    case mPids of 
      Nothing -> extractTxns pflowMap
      Just pids -> extractTxns $ Map.filterWithKey (\k _ -> k `elem` pids) pflowMap


-- ^ deposit cash to account by collection rule
depositInflow :: Date -> CollectionRule -> Map.Map PoolId CF.PoolCashflow -> Map.Map AccountName A.Account -> Either String (Map.Map AccountName A.Account)
depositInflow d (Collect mPids s an) pFlowMap amap 
  = do 
      amts <- traverse (readProceeds s) txns
      let amt = sum amts
      return $ Map.adjust (A.deposit amt d (PoolInflow mPids s)) an amap
    where 
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap


depositInflow d (CollectByPct mPids s splitRules) pFlowMap amap    --TODO need to check 100%
  = do 
      amts <- traverse (readProceeds s) txns
      let amt = sum amts
      let amtsToAccs = [ (an, mulBR amt splitRate) | (splitRate, an) <- splitRules]
      return $ 
              foldr
                (\(accName,accAmt) accM -> 
                  Map.adjust (A.deposit accAmt d (PoolInflow mPids s)) accName accM)
                amap
                amtsToAccs
    where 
      txns =  extractTxnsFromFlowFrameMap mPids pFlowMap 


-- ^ deposit cash to account by pool map CF and rules
depositPoolFlow :: [CollectionRule] -> Date -> Map.Map PoolId CF.PoolCashflow -> Map.Map String A.Account -> Either String (Map.Map String A.Account)
depositPoolFlow rules d pFlowMap amap 
  = foldM (\acc rule -> depositInflow d rule pFlowMap acc) amap rules


$(deriveJSON defaultOptions ''CollectionRule)
