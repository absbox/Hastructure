{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Reports (patchFinancialReports,getItemBalance,buildBalanceSheet,buildCashReport
            ) where

import Data.List ( find, sort )
import qualified Data.DList as DL
import qualified Asset as P
import qualified Data.Map as Map
import qualified Cashflow as CF
import qualified Accounts as A
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Expense as F
import qualified Liability as L
import Control.Applicative (liftA3)
import Types
import Deal.DealBase
    ( TestDeal(TestDeal, pool, fees, bonds, accounts,liqProvider,rateSwap), getIssuanceStatsConsol, getAllCollectedFrame ,poolTypePool, dealPool)
import Deal.DealQuery ( queryCompound )
import Deal.DealAction ( calcDueFee, calcDueInt )
import Data.Maybe (fromMaybe)

import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Lens
import Stmt
    ( aggByTxnComment,
      getFlow,
      getTxnComment,
      getTxns,
      FlowDirection(Outflow, Inflow) )

-- ^ add financial report to the logs
patchFinancialReports :: P.Asset a => TestDeal a -> Date -> DL.DList ResultComponent -> Either String (DL.DList ResultComponent)
-- patchFinancialReports t d DL.empty = return (DL.empty)
patchFinancialReports t d logs 
  = case (find pickReportLog (reverse (DL.toList logs))) of 
      Nothing -> Right logs
      Just (FinancialReport sd ed bs cash) 
        -> let
             cashReport = buildCashReport t ed d
           in
             do 
               bsReport <- buildBalanceSheet t d
               let newlog = FinancialReport ed d bsReport cashReport
               return (DL.snoc logs newlog)
      where 
        pickReportLog FinancialReport {} = True
        pickReportLog _ = False

getItemBalance :: BookItem -> Balance
getItemBalance (Item _ bal) = bal
getItemBalance (ParentItem _ items) = sum $ getItemBalance <$> items

getPoolBalanceStats :: P.Asset a => TestDeal a -> Date -> Maybe [PoolId] -> Either String [Balance]
getPoolBalanceStats t d mPid 
  = let 
      poolStats = [queryCompound t d (FutureCurrentPoolBalance mPid)
                  ,(queryCompound t d (PoolCumCollection [NewDefaults] mPid))
                  ,negate <$> (queryCompound t d (PoolCumCollection [CollectedRecoveries] mPid))]
    in 
      do 
        poolStats2::[Rational] <- sequenceA poolStats
        return $ fromRational <$> poolStats2

type PoolBalanceSnapshot = (Balance, Balance, Balance)

buildBalanceSheet :: P.Asset a => TestDeal a -> Date -> Either String BalanceSheetReport
buildBalanceSheet t@TestDeal{ pool = pool, bonds = bndMap , fees = feeMap , liqProvider = liqMap, rateSwap = rsMap ,accounts = accMap} 
                  d 
    = let  
        --- accounts
        accM = [ ParentItem accName [Item "Balance" accBal,Item "Accrue Int" accDue] | (accName, [accBal,accDue]) <- Map.toList $ Map.map (\acc -> [A.accBalance,(A.accrueInt d)] <*> [acc]) accMap ]
        
        ---- pools
        mapPoolKey PoolConsol = Nothing 
        mapPoolKey (PoolName x) = Just [PoolName x]
        poolAstBalMap_ = Map.mapWithKey 
                           (\k _ -> getPoolBalanceStats t d (mapPoolKey k)) $
                           view (dealPool . poolTypePool) t
        
        ---- swaps
        swapToCollect = ParentItem "Swap" [ ParentItem rsName [ Item "To Receive" rsNet ] | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                            , rsNet > 0 ]
        
       -- liquidity provider 
        liqProviderAccrued = Map.map (CE.accrueLiqProvider d) (fromMaybe Map.empty liqMap)
        liqProviderOs = [ ParentItem liqName [Item "Balance" liqBal,Item "Accrue Int" liqDueInt, Item "Due Fee" liqDueFee ]  | (liqName,[liqBal,liqDueInt,liqDueFee]) <- Map.toList (Map.map (\liq -> [CE.liqBalance,CE.liqDueInt,CE.liqDuePremium]<*> [liq]) liqProviderAccrued)] 
        -- rate swap
        swapToPay = ParentItem "Swap" [ ParentItem rsName [Item "To Pay" (negate rsNet)] | (rsName,rsNet) <- Map.toList (Map.map (HE.rsNetCash . (HE.accrueIRS d)) (fromMaybe Map.empty rsMap))
                                                   , rsNet < 0 ]

      in
        do
          poolAstBalMap <- sequenceA poolAstBalMap_
          let poolAstMap = Map.mapWithKey 
                             (\k vs -> ParentItem (show k)
                                              [ Item "Performing" (vs!!0) 
                                              , Item "Defaulted"  (vs!!1) 
                                              , Item "Recovery"   (vs!!2) ])
                             poolAstBalMap
          let poolAst = ParentItem "Pool" $ Map.elems poolAstMap
          -- Asset : Account, pool, swap to collect
          let ast = ParentItem "Asset" [ParentItem "Account" accM , poolAst , swapToCollect]
          feeWithDueAmount <- (F.feeDue <$>) <$>  mapM ((calcDueFee t d)) feeMap
          let feeToPay = ParentItem "Fee" [ ParentItem feeName [Item "Due" feeDueBal] 
                                           | (feeName,feeDueBal) <- Map.toList feeWithDueAmount ]
          bndWithDueAmount <- mapM (calcDueInt t d) bndMap
          let bndToShow = Map.map (\bnd -> (L.getCurBalance bnd, L.getTotalDueInt bnd)) bndWithDueAmount 
          let bndM = [ ParentItem bndName [Item "Balance" bndBal,Item "Due Int" bndDueAmt ] 
                                        | (bndName,(bndBal,bndDueAmt)) <- Map.toList bndToShow]
          -- Liabilities: bond, fee, liquidity, swap to pay
          let liab = ParentItem "Liability" [ ParentItem "Bond" bndM , feeToPay, ParentItem "Liquidity" liqProviderOs, swapToPay]
          let totalDebtBal = getItemBalance liab
          let totalAssetBal = getItemBalance ast  
          let eqty = Item "Net Asset" (totalAssetBal - totalDebtBal)
          return $ BalanceSheetReport {asset=ast,liability=liab,equity=eqty,reportDate=d}

-- TODO  performance improve here, need to filter txn first
buildCashReport :: P.Asset a => TestDeal a -> Date -> Date -> CashflowReport
buildCashReport t@TestDeal{accounts = accs} sd ed 
  = CashflowReport { inflow = inflowItems
                   , outflow = outflowItems
                   , net = cashChange
                   , startDate = sd
                   , endDate = ed }
      where 
        _txns = concat $ Map.elems $ Map.map (DL.toList . getTxns) $ Map.map A.accStmt accs
        txns = sliceBy EI sd ed _txns
   
        inflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Inflow)  txns
        outflowTxn = sort $ filter (\x -> (getFlow . getTxnComment) x == Outflow) txns
        
        inflowM = Map.mapKeys show $ aggByTxnComment inflowTxn Map.empty
        outflowM = Map.mapKeys show $ aggByTxnComment outflowTxn Map.empty 
        
        inflowItems = ParentItem "Inflow" [ Item k v | (k,v) <- Map.toList inflowM ]
        outflowItems = ParentItem "Outflow" [ Item k v | (k,v) <- Map.toList outflowM ]
        
        cashChange = Item "Net Cash" $ sum (Map.elems inflowM) + sum (Map.elems outflowM)
