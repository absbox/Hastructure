{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Deal.DealValidation (validateRun,validatePreRun,validateReq)
  where 

import Deal.DealBase
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import qualified Waterfall as W
import qualified CreditEnhancement as CE
import qualified Liability as L
import qualified Accounts as A
import qualified Expense as F
import qualified Asset as P
import qualified Assumptions as AP
import qualified InterestRate as IR

import Deal.DealCollection (CollectionRule(..))

import Control.Lens hiding (element)
import Control.Lens.TH

import Data.Maybe
import qualified Assumptions as A


import Debug.Trace
debug = flip trace

extractRequiredRates :: (P.Asset a,IR.UseRate a) => TestDeal a -> Set.Set Types.Index
extractRequiredRates t@TestDeal{accounts = accM 
                               ,fees = feeM 
                               ,bonds = bondM 
                               ,liqProvider = mliqProviderM 
                               ,rateSwap = mrsM 
                               ,rateCap = mRcM
                               ,pool = pool}
  = Set.fromList $ assetIndex ++ accIndex ++ bondIndex ++ liqProviderIndex ++ rsIndex ++ rcIndex
  -- = Set.fromList $ accIndex ++ bondIndex ++ liqProviderIndex ++ rsIndex
    where 
      assetIndex = catMaybes $ IR.getIndex <$> getAllAssetList t
      
      accIndex = catMaybes $ IR.getIndex <$> Map.elems accM 
      bondIndex = concat $ catMaybes $ IR.getIndexes <$> Map.elems bondM 
      liqProviderIndex = case mliqProviderM of 
                           Just liqProviderM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems liqProviderM
                           Nothing -> [] 
      rsIndex = case mrsM of 
                  Just rsM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems rsM
                  Nothing -> []
      rcIndex = case mRcM of 
                  Just rcM -> concat $ catMaybes $ IR.getIndexes <$> Map.elems rcM
                  Nothing -> []
        
      -- note fee is not tested
validateAggRule :: [CollectionRule] -> [PoolId] -> [ResultComponent]
validateAggRule rules validPids =
    [ ErrorMsg ("Pool source "++show ps++" has a weight of "++show r)   | ((pid,ps),r) <- Map.toList oustandingPs ] ++
    [ ErrorMsg ("Pool Id not found "++show ospid++" in "++ show validPids) | ospid <- osPid ]
  where 
    countWeight (Collect (Just pids) ps _) =  Map.fromList [((pid,ps),1.0) | pid <- pids]
    countWeight (Collect Nothing ps _) =  Map.fromList [((PoolConsol,ps),1.0)]
    countWeight (CollectByPct (Just pids) ps lst) = Map.fromList [((pid,ps), pct) | pid <- pids, pct <- fst <$> lst]
    countWeight (CollectByPct Nothing ps lst) = Map.fromList [((PoolConsol, ps),pct)| pct <- fst <$> lst]
    
    sumMap = foldl1 (Map.unionWith (+)) $ countWeight <$> rules  
    oustandingPs = Map.filter (> 1.0) sumMap

    getPids (Collect (Just pids) _ _) = pids  
    getPids (Collect Nothing ps _) = [PoolConsol]
    getPids (CollectByPct (Just pids) _ _) = pids
    getPids (CollectByPct Nothing _ _ ) = [PoolConsol]
    osPid = Set.elems $ Set.difference (Set.fromList (concat (getPids <$> rules))) (Set.fromList validPids)


validateFee :: F.Fee -> [ResultComponent]
-- validateFee (F.Fee fn (F.AnnualRateFee (CurrentBondBalanceOf _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (OriginalBondBalanceOf _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (CurrentPoolBalance _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee (OriginalPoolBalance _) _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee CurrentBondBalance _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee OriginalBondBalance _) _ _ _ _ _ _) = [] 
-- validateFee (F.Fee fn (F.AnnualRateFee ds _) _ _ _ _ _ _ )
--   = [ErrorMsg ("Fee Name "++fn++" has an unsupported base "++show ds)]
validateFee _ = []

--- get required pool id and required revolving pool name
extractRequiredRevolvingPool :: P.Asset a => TestDeal a -> (Set.Set PoolId, Set.Set String)
extractRequiredRevolvingPool t@TestDeal{waterfall = waterfallM} = 
  let 
    poolIds = Set.fromList $ getPoolIds t
    extract accPoolIds accRpoolNames [] = (accPoolIds,accRpoolNames)
    extract accPoolIds accRpoolNames ((W.BuyAsset _ _ _ mPoolId):as) = 
      extract (Set.insert (fromMaybe PoolConsol mPoolId) accPoolIds) accRpoolNames as
    extract accPoolIds accRpoolNames ((W.BuyAssetFrom _ _ _ rPoolName mPoolId):as) = 
      extract (Set.insert (fromMaybe PoolConsol mPoolId) accPoolIds)
              (Set.insert (fromMaybe "Consol" rPoolName) accRpoolNames)
              as
    extract accPoolIds accRpoolNames ((W.ActionWithPre _ subActions):as) = 
      let 
        (subAccPoolIds,subAccRPoolNames) = extract accPoolIds accRpoolNames subActions
      in 
        extract (accPoolIds <> subAccPoolIds) (accRpoolNames <> subAccRPoolNames) as
    extract accPoolIds accRpoolNames ((W.ActionWithPre2 _ subActionsA subActionsB):as) = 
      let 
        (subAccPoolIdsA,subAccRPoolNamesA) = extract accPoolIds accRpoolNames subActionsA
        (subAccPoolIdsB,subAccRPoolNamesB) = extract subAccPoolIdsA subAccRPoolNamesA subActionsB
      in 
        extract subAccPoolIdsB subAccRPoolNamesB as
    extract accPoolIds accRpoolNames (_:as) = extract accPoolIds accRpoolNames as
    requiredByWaterfall = Map.elems $ Map.map (extract (Set.fromList []) (Set.fromList [])) waterfallM
  in 
    (Set.unions $ fst <$> requiredByWaterfall, Set.unions $ snd <$> requiredByWaterfall)


validateReq :: (IR.UseRate a,P.Asset a) => TestDeal a -> AP.NonPerfAssumption -> (Bool,[ResultComponent])
validateReq t@TestDeal{accounts = accMap, fees = feeMap} 
            assump@A.NonPerfAssumption{A.interest = intM, A.issueBondSchedule = mIssuePlan, A.revolving = mRevolvingAssump} 
  = let 
      ratesRequired = extractRequiredRates t
      ratesSupplied = case intM of 
                        Nothing -> Set.empty
                        Just intLst -> Set.fromList $ [ idx | RateFlat idx _ <- intLst ] ++ [ idx | RateCurve idx _ <- intLst ]
      missingIndex = Set.difference ratesRequired ratesSupplied
      missingIndexError = if null missingIndex then 
                            []
                          else
                            [ErrorMsg ("Failed to find index "++show missingIndex++"in assumption rates"++ show ratesSupplied)]

      bgNamesInDeal = Map.keysSet $ view dealBondGroups t
      -- fee validation 
      feeErrors = concatMap validateFee $ Map.elems feeMap
      -- issue plan validation
      issuePlanError = case mIssuePlan of 
                        Nothing -> []
                        Just issueBndEventlist
                          -> let 
                              bgNamesInAssump = Set.fromList $ [ bgName | TsPoint d (A.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
                              bgNameErrors = [ ErrorMsg ("issueBond:Missing Bond Group Name in Deal:"++ missingBgName ) | missingBgName <- Set.elems (Set.difference bgNamesInAssump bgNamesInDeal)]

                              newBndNames = Set.fromList $ [ L.bndName bnd | TsPoint d (A.IssueBondEvent _ _ _ bnd _ _) <- issueBndEventlist ]
                              existingBndNames = Set.fromList $ L.bndName <$> viewDealAllBonds t
                              bndNameErrors = [ ErrorMsg ("issueBond:Existing Bond Name in Deal:"++ existsBndName ) | existsBndName <- Set.elems (Set.intersection newBndNames existingBndNames)]

                              acNamesInAssump = Set.fromList $ [ acName | TsPoint d (A.IssueBondEvent _ _ acName _ _ _) <- issueBndEventlist ]
                              existingAccNames = Map.keysSet accMap
                              accNameErrors = [ ErrorMsg ("issueBond:Missing Account Name in Deal:"++ missingAccName ) | missingAccName <- Set.elems (Set.difference acNamesInAssump existingAccNames)]
                              
                              bndNamesInAssump = [ L.bndName bnd | TsPoint d (A.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
                              bndUniqNames = Set.fromList bndNamesInAssump
                              dupNamesErrors = [ ErrorMsg("Duplicate Bond Names in Funding Plan") | length bndUniqNames /= length bndNamesInAssump]
                             in 
                              bgNameErrors ++ accNameErrors ++ bndNameErrors ++ dupNamesErrors

      -- revolving buy validation
      revolvingBuyError = let 
                            (requiredPoolIds, requiredRPoolNames) =  extractRequiredRevolvingPool t
                            a = 1 
                          in 
                            case mRevolvingAssump of 
                              Nothing -> []
                              Just (A.AvailableAssets _ _ ) -> [ ErrorMsg ("BuyAsset: Missing Pool Id in assumption" ++ show x)  | x <- Set.toList (requiredPoolIds Set.\\ Set.fromList (getPoolIds t))]
                              Just (A.AvailableAssetsBy rMap ) -> [ ErrorMsg ("BuyAsset: Missing Revolving Pool in assumption" ++ show x)  | x <- Set.toList (requiredRPoolNames Set.\\ Set.fromList (Map.keys rMap))] -- `debug` ("requiredRPoolNames 0> "++ show requiredRPoolNames)


      (dealWarnings,dealErrors) = validatePreRun t 
      finalErrors = missingIndexError ++ dealErrors ++ issuePlanError ++ feeErrors ++ revolvingBuyError
      finalWarnings = dealWarnings
    in 
      (null finalErrors,finalErrors++finalWarnings)

validatePreRun :: P.Asset a => TestDeal a -> ([ResultComponent],[ResultComponent])
validatePreRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,rateCap = rcM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM
                      ,pool = pool 
                      ,dates = dates
                      ,status = status} 
  = let 
      poolIds = getPoolIds t 
      -- date check

      -- issuance balance check 
      issuanceBalCheck CurrentDates {} = let 
                                           stats = Map.elems $ getIssuanceStats t Nothing
                                           lookupResult = Map.lookup IssuanceBalance <$> stats
                                         in
                                           if all isNothing lookupResult then
                                             [ErrorMsg "Issuance balance not found for a Ongoing Deal"]
                                           else
                                             []
      issuanceBalCheck _ = []

      -- val on deal status and deal dates

      -- collection rule check
      aggRuleResult = if isResec t then 
                        []
                      else
                        validateAggRule aggRule poolIds 
      -- TODO : collectCash shouldn't overlap with others

      -- waterfall action coverage check 

      -- run result scan

      allErrors = issuanceBalCheck dates ++ aggRuleResult 
      -- check issuance balance 
      
      w1 = if (not (isPreClosing t)) && (length (Map.elems (getIssuanceStats t Nothing))) == 0 then
             [WarningMsg "Deal passes PreClosing status, but not cumulative defaults/delinq at cutoff date?"]
           else 
             []
      warnings = w1
    in 
      (warnings,allErrors) -- Valiation Pass

-- validate deal object after run
validateRun :: TestDeal a -> [ResultComponent]
validateRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM} 
  = let 
      bndList = viewDealAllBonds t
      -- oustanding liability
      --- bond
      bondWarnings = [ WarningMsg ("Bond "++bn++ " is not paid off")  | bn <- L.bndName <$> filter (not . isPaidOff) bndList ]
      --- fee
      feeWarnings = [ WarningMsg ("Fee "++fn++ " is not paid off")  | fn <- Map.elems (Map.map F.feeName $ Map.filter (not . isPaidOff) feeM) ]
      --- liquidity provider 
      liqWarnings = case liqProviderM of 
                      Nothing -> []
                      Just liqM -> [ WarningMsg ("LiquidityProvider "++bn++ " is not paid off")  | bn <- Map.elems (Map.map CE.liqName $ Map.filter (not . isPaidOff)  liqM) ]
      --- rate swap
      rsWarnings = case rsM of 
                     Nothing -> []
                     Just rsM -> []   -- TODO [ WarningMsg ("LiquidityProvider "++bn++ " is not paid off")  | bn <- Map.elems (Map.map CE.liqName $ Map.filter (not . isPaidOff)  rsM) ]

      -- oustanding assets
      --- account
      accWarnings = [ WarningMsg ("Account "++an++ " has cash to be distributed")  | an <- Map.elems (Map.map A.accName $ Map.filter (\x -> A.accBalance x > 0) accM)]
      --- uncollected pool cash

      -- run result scan
    in 
      bondWarnings ++ feeWarnings ++ accWarnings ++ liqWarnings ++ rsWarnings
