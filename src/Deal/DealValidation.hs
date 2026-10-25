{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

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
import qualified Triggers as Trg
import qualified Accounts as A
import qualified Expense as F
import qualified Asset as P
import qualified Assumptions as AP
import qualified InterestRate as IR
import Interface

import Deal.DealCollection (CollectionRule(..))

import Control.Lens hiding (element)
import Control.Lens.TH ()

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
    
    sumMap = foldl (Map.unionWith (+)) Map.empty $ countWeight <$> rules  
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


-- | Names referenced by the deal definition, grouped by the map they should exist in.
-- Used to catch typos before a run instead of crashing mid-run.
data NameRefs = NameRefs {
    refAccounts :: Set.Set String
  , refFees     :: Set.Set String
  , refBonds    :: Set.Set String
  , refLiq      :: Set.Set String
  , refSwaps    :: Set.Set String
  , refCaps     :: Set.Set String
  }

instance Semigroup NameRefs where
  NameRefs a b c d e f <> NameRefs a' b' c' d' e' f' =
    NameRefs (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')
instance Monoid NameRefs where
  mempty = NameRefs Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty
  mappend = (<>)

refsOfAction :: W.Action -> NameRefs
refsOfAction a = case a of
  W.Transfer _ a1 a2 _                  -> accs [a1,a2]
  W.TransferAndBook _ a1 a2 _ _         -> accs [a1,a2]
  W.TransferMultiple srcs a2 _          -> accs (a2 : (snd <$> srcs))
  W.CalcFee fns                         -> fees fns
  W.PayFee _ acc fns _                  -> accs [acc] <> fees fns
  W.PayFeeBySeq _ acc fns _             -> accs [acc] <> fees fns
  W.CalcAndPayFee _ acc fns _           -> accs [acc] <> fees fns
  W.PayFeeResidual _ acc fn             -> accs [acc] <> fees [fn]
  W.CalcBondInt bns                     -> bonds bns
  W.CalcBondIntBy bn _ _                -> bonds [bn]
  W.PayIntOverInt _ acc bns _           -> accs [acc] <> bonds bns
  W.PayInt _ acc bns _                  -> accs [acc] <> bonds bns
  W.PayIntAndBook _ acc bns _ _         -> accs [acc] <> bonds bns
  W.PayIntBySeq _ acc bns _             -> accs [acc] <> bonds bns
  W.PayIntOverIntBySeq _ acc bns _      -> accs [acc] <> bonds bns
  W.AccrueAndPayInt _ acc bns _         -> accs [acc] <> bonds bns
  W.AccrueAndPayIntBySeq _ acc bns _    -> accs [acc] <> bonds bns
  W.PayIntResidual _ acc bn             -> accs [acc] <> bonds [bn]
  W.PayIntByRateIndex _ acc bns _ _     -> accs [acc] <> bonds bns
  W.PayIntByRateIndexBySeq _ acc bns _ _ -> accs [acc] <> bonds bns
  W.CalcBondPrin _ acc bns _            -> accs [acc] <> bonds bns
  W.CalcBondPrin2 _ bns                 -> bonds bns
  W.PayPrinWithDue acc bns _            -> accs [acc] <> bonds bns
  W.PayPrin _ acc bns _                 -> accs [acc] <> bonds bns
  W.PayPrinBySeq _ acc bns _            -> accs [acc] <> bonds bns
  W.PayPrinResidual acc bns             -> accs [acc] <> bonds bns
  W.PayIntPrinBySeq _ acc bns _         -> accs [acc] <> bonds bns
  W.AccrueAndPayIntPrinBySeq _ acc bns _ -> accs [acc] <> bonds bns
  W.PayPrinGroup _ acc bn _ _           -> accs [acc] <> bonds [bn]
  W.AccrueIntGroup bns                  -> bonds bns
  W.PayIntGroup _ acc bn _ _            -> accs [acc] <> bonds [bn]
  W.AccrueAndPayIntGroup _ acc bn _ _   -> accs [acc] <> bonds [bn]
  W.WriteOff _ bn                       -> bonds [bn]
  W.WriteOffAndBook _ bn _              -> bonds [bn]
  W.WriteOffBySeq _ bns                 -> bonds bns
  W.WriteOffBySeqAndBook _ bns _        -> bonds bns
  W.FundWith _ acc bn                   -> accs [acc] <> bonds [bn]
  W.BuyAsset _ _ acc _                  -> accs [acc]
  W.BuyAssetFrom _ _ acc _ _            -> accs [acc]
  W.LiquidatePool _ acc _               -> accs [acc]
  W.LiqSupport _ liq _ _                -> liqs [liq]
  W.LiqRepay _ _ acc liq                -> accs [acc] <> liqs [liq]
  W.LiqYield _ acc liq                  -> accs [acc] <> liqs [liq]
  W.LiqAccrue liqNames                  -> liqs liqNames
  W.SwapAccrue ce                       -> swaps [ce]
  W.SwapReceive acc ce                  -> accs [acc] <> swaps [ce]
  W.SwapPay acc ce                      -> accs [acc] <> swaps [ce]
  W.SwapSettle acc ce                   -> accs [acc] <> swaps [ce]
  W.CollectRateCap acc ce               -> accs [acc] <> caps [ce]
  W.WithPay acc _                       -> accs [acc]
  W.ActionWithPre _ sub                 -> foldMap refsOfAction sub
  W.ActionWithPre2 _ sa sb              -> foldMap refsOfAction sa <> foldMap refsOfAction sb
  _                                     -> mempty
  where
    accs xs   = mempty { refAccounts = Set.fromList xs }
    fees xs   = mempty { refFees     = Set.fromList xs }
    bonds xs  = mempty { refBonds    = Set.fromList xs }
    liqs xs   = mempty { refLiq      = Set.fromList xs }
    swaps xs  = mempty { refSwaps    = Set.fromList xs }
    caps xs   = mempty { refCaps     = Set.fromList xs }

refsOfTriggerEffect :: Trg.TriggerEffect -> NameRefs
refsOfTriggerEffect te = case te of
  Trg.DoAccrueFee fns                 -> mempty { refFees = Set.fromList fns }
  Trg.ChangeReserveBalance acc _      -> mempty { refAccounts = Set.singleton acc }
  Trg.CloseDeal _ _ (_, acc, _) _     -> mempty { refAccounts = Set.singleton acc }
  Trg.BuyAsset acc _                  -> mempty { refAccounts = Set.singleton acc }
  Trg.ChangeBondRate bn _ _           -> mempty { refBonds = Set.singleton bn }
  Trg.AddTrigger trg                  -> refsOfTriggerEffect (Trg.trgEffects trg)
  Trg.TriggerEffects effs             -> foldMap refsOfTriggerEffect effs
  Trg.RunActions acts                 -> foldMap refsOfAction acts
  _                                   -> mempty

refsOfTrigger :: Trg.Trigger -> NameRefs
refsOfTrigger = refsOfTriggerEffect . Trg.trgEffects

refsOfCollectRule :: CollectionRule -> NameRefs
refsOfCollectRule (Collect _ _ acc) = mempty { refAccounts = Set.singleton acc }
refsOfCollectRule (CollectByPct _ _ lst) = mempty { refAccounts = Set.fromList (snd <$> lst) }

-- | Collect every deal-definition name reference and report the ones absent from the
-- corresponding deal map. Bond groups are accepted wherever a bond name is expected.
validateNameReferences :: P.Asset a => TestDeal a -> Set.Set BondName -> [ResultComponent]
validateNameReferences t@TestDeal{waterfall = waterfallM
                                ,accounts = accM
                                ,fees = feeM
                                ,collects = aggRule
                                ,liqProvider = liqProviderM
                                ,rateSwap = rsM
                                ,rateCap = rcM
                                ,triggers = triggerM}
                       extraBondNames
  = let 
      refs = foldMap refsOfAction (concat (Map.elems waterfallM))
             <> foldMap refsOfCollectRule aggRule
             <> maybe mempty (foldMap (foldMap refsOfTrigger)) triggerM
      accNames  = Map.keysSet accM
      feeNames  = Map.keysSet feeM
      bondNames = Set.fromList (L.bndName <$> viewDealAllBonds t) <> Map.keysSet (view dealBondGroups t) <> extraBondNames
      liqNames  = maybe Set.empty Map.keysSet liqProviderM
      swapNames = maybe Set.empty Map.keysSet rsM
      capNames  = maybe Set.empty Map.keysSet rcM
      missing label set valid =
        [ ErrorMsg (label ++ " not found in deal: " ++ x) | x <- Set.toList set, not (Set.member x valid) ]
    in 
      missing "Account" (refAccounts refs) accNames
      ++ missing "Fee" (refFees refs) feeNames
      ++ missing "Bond/BondGroup" (refBonds refs) bondNames
      ++ missing "Liquidity provider" (refLiq refs) liqNames
      ++ missing "Rate swap" (refSwaps refs) swapNames
      ++ missing "Rate cap" (refCaps refs) capNames


validateReq :: (IR.UseRate a,P.Asset a) => TestDeal a -> AP.NonPerfAssumption -> (Bool,[ResultComponent])
validateReq t@TestDeal{accounts = accMap, fees = feeMap} 
            assump@AP.NonPerfAssumption{AP.interest = intM, AP.issueBondSchedule = mIssuePlan, AP.revolving = mRevolvingAssump} 
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
                              bgNamesInAssump = Set.fromList $ [ bgName | TsPoint d (AP.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
                              bgNameErrors = [ ErrorMsg ("issueBond:Missing Bond Group Name in Deal:"++ missingBgName ) | missingBgName <- Set.elems (Set.difference bgNamesInAssump bgNamesInDeal)]

                              newBndNames = Set.fromList $ [ L.bndName bnd | TsPoint d (AP.IssueBondEvent _ _ _ bnd _ _) <- issueBndEventlist ]
                              existingBndNames = Set.fromList $ L.bndName <$> viewDealAllBonds t
                              bndNameErrors = [ ErrorMsg ("issueBond:Existing Bond Name in Deal:"++ existsBndName ) | existsBndName <- Set.elems (Set.intersection newBndNames existingBndNames)]

                              acNamesInAssump = Set.fromList $ [ acName | TsPoint d (AP.IssueBondEvent _ _ acName _ _ _) <- issueBndEventlist ]
                              existingAccNames = Map.keysSet accMap
                              accNameErrors = [ ErrorMsg ("issueBond:Missing Account Name in Deal:"++ missingAccName ) | missingAccName <- Set.elems (Set.difference acNamesInAssump existingAccNames)]
                              
                              bndNamesInAssump = [ L.bndName bnd | TsPoint d (AP.IssueBondEvent _ bgName _ bnd _ _) <- issueBndEventlist ]
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
                              Just (AP.AvailableAssets _ _ ) -> [ ErrorMsg ("BuyAsset: Missing Pool Id in assumption" ++ show x)  | x <- Set.toList (requiredPoolIds Set.\\ Set.fromList (getPoolIds t))]
                              Just (AP.AvailableAssetsBy rMap ) -> [ ErrorMsg ("BuyAsset: Missing Revolving Pool in assumption" ++ show x)  | x <- Set.toList (requiredRPoolNames Set.\\ Set.fromList (Map.keys rMap))] -- `debug` ("requiredRPoolNames 0> "++ show requiredRPoolNames)


      (dealWarnings,dealErrors) = validatePreRunWith t planBondNames
      finalErrors = missingIndexError ++ dealErrors ++ issuePlanError ++ feeErrors ++ revolvingBuyError
      finalWarnings = dealWarnings
      planBondNames = case mIssuePlan of 
                        Nothing -> Set.empty
                        Just evs -> Set.fromList [ L.bndName bnd | TsPoint _ (AP.IssueBondEvent _ _ _ bnd _ _) <- evs ]
    in 
      (null finalErrors,finalErrors++finalWarnings)

validatePreRun :: P.Asset a => TestDeal a -> ([ResultComponent],[ResultComponent])
validatePreRun t = validatePreRunWith t Set.empty

-- | Like 'validatePreRun' but treats @extraBondNames@ (e.g. bonds created by an
-- issuance plan during the run) as known, so they are not flagged as missing.
validatePreRunWith :: P.Asset a => TestDeal a -> Set.Set BondName -> ([ResultComponent],[ResultComponent])
validatePreRunWith t@TestDeal{waterfall=waterfallM
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
                      extraBondNames
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

      allErrors = issuanceBalCheck dates ++ aggRuleResult ++ validateNameReferences t extraBondNames
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
