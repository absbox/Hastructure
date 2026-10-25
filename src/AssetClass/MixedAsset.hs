{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AssetClass.MixedAsset
  (projAssetUnion,projAssetUnionList,projectCashflow, calcAssetUnion,curBal)
  where

import qualified Data.Time as T ()
import qualified Cashflow as CF -- (Cashflow,Amount,Interests,Principals)
import qualified Assumptions as A
import qualified AssetClass.AssetBase as ACM
import InterestRate ()
import Interface ()
import qualified Asset as P
import Lib
import Util ()
import DateUtil ()
import Types
import qualified Data.Map as Map
import Data.List ()
import Data.Maybe ()
import Data.Aeson ()
import Language.Haskell.TH ()
import Data.Aeson.TH ()
import Data.Aeson.Types ()
import GHC.Generics ()

import AssetClass.AssetBase
import AssetClass.Mortgage
import AssetClass.Lease
import AssetClass.Loan ()
import AssetClass.Installment ()
import AssetClass.Receivable ()
import AssetClass.AssetCashflow ()
import AssetClass.FixedAsset ()
import AssetClass.ProjectedCashFlow ()

import Assumptions ()
import qualified Asset as Ast ()



instance P.Asset AssetUnion where

  calcCashflow ma asOfDay mRates = calcAssetUnion ma asOfDay mRates
  
  getCurrentBal ma = curBal ma

  getOriginBal ma = origBal ma

  getOriginRate ma = origRate ma
  
  getCurrentRate ma = currRate ma

  getOriginDate ma = origDate ma
  
  getOriginInfo ma = origInfo ma
  
  isDefaulted = isDefault
  
  getPaymentDates ma n = getPaydates ma n

  getRemainTerms = remainTerms

  projCashflow ma asOfDay assumps mRates = projAssetUnion ma asOfDay assumps mRates
  
  getBorrowerNum = borrowerNum 

  splitWith = splitWith

  updateOriginDate = updateOrigDate
  
  calcAlignDate = calcAlignDate
  
curBal:: ACM.AssetUnion -> Balance
curBal (ACM.MO ast) = P.getCurrentBal ast
curBal (ACM.LO ast) = P.getCurrentBal ast
curBal (ACM.IL ast) = P.getCurrentBal ast
curBal (ACM.LS ast) = P.getCurrentBal ast
curBal (ACM.FA ast) = P.getCurrentBal ast
curBal (ACM.RE ast) = P.getCurrentBal ast
curBal (ACM.PF ast) = P.getCurrentBal ast

origBal :: ACM.AssetUnion -> Balance
origBal (ACM.MO ast) = P.getOriginBal ast
origBal (ACM.LO ast) = P.getOriginBal ast
origBal (ACM.IL ast) = P.getOriginBal ast
origBal (ACM.LS ast) = P.getOriginBal ast
origBal (ACM.FA ast) = P.getOriginBal ast
origBal (ACM.RE ast) = P.getOriginBal ast
origBal (ACM.PF ast) = P.getOriginBal ast

origRate :: ACM.AssetUnion -> IRate
origRate (ACM.MO ast) = P.getOriginRate ast
origRate (ACM.LO ast) = P.getOriginRate ast
origRate (ACM.IL ast) = P.getOriginRate ast
origRate (ACM.LS ast) = P.getOriginRate ast
origRate (ACM.FA ast) = P.getOriginRate ast
origRate (ACM.RE ast) = P.getOriginRate ast
origRate (ACM.PF ast) = P.getOriginRate ast

currRate :: ACM.AssetUnion -> IRate
currRate (ACM.MO ast) = P.getCurrentRate ast
currRate (ACM.LO ast) = P.getCurrentRate ast
currRate (ACM.IL ast) = P.getCurrentRate ast
currRate (ACM.LS ast) = P.getCurrentRate ast
currRate (ACM.FA ast) = P.getCurrentRate ast
currRate (ACM.RE ast) = P.getCurrentRate ast
currRate (ACM.PF ast) = P.getCurrentRate ast


origDate :: ACM.AssetUnion -> Date
origDate (ACM.MO ast) = P.getOriginDate ast
origDate (ACM.LO ast) = P.getOriginDate ast
origDate (ACM.IL ast) = P.getOriginDate ast
origDate (ACM.LS ast) = P.getOriginDate ast
origDate (ACM.FA ast) = P.getOriginDate ast
origDate (ACM.RE ast) = P.getOriginDate ast
origDate (ACM.PF ast) = P.getOriginDate ast
 
 
origInfo :: ACM.AssetUnion -> OriginalInfo
origInfo (ACM.MO ast) = P.getOriginInfo ast
origInfo (ACM.LO ast) = P.getOriginInfo ast
origInfo (ACM.IL ast) = P.getOriginInfo ast
origInfo (ACM.LS ast) = P.getOriginInfo ast
origInfo (ACM.FA ast) = P.getOriginInfo ast
origInfo (ACM.RE ast) = P.getOriginInfo ast
origInfo (ACM.PF ast) = P.getOriginInfo ast

isDefault :: ACM.AssetUnion -> Bool 
isDefault (ACM.MO ast) = P.isDefaulted ast
isDefault (ACM.LO ast) = P.isDefaulted ast
isDefault (ACM.IL ast) = P.isDefaulted ast
isDefault (ACM.LS ast) = P.isDefaulted ast
isDefault (ACM.FA ast) = P.isDefaulted ast
isDefault (ACM.RE ast) = P.isDefaulted ast
isDefault (ACM.PF ast) = P.isDefaulted ast

getPaydates :: ACM.AssetUnion -> Int -> [Date]
getPaydates (ACM.MO ast) n = P.getPaymentDates ast n 
getPaydates (ACM.LO ast) n = P.getPaymentDates ast n 
getPaydates (ACM.IL ast) n = P.getPaymentDates ast n 
getPaydates (ACM.LS ast) n = P.getPaymentDates ast n 
getPaydates (ACM.FA ast) n = P.getPaymentDates ast n
getPaydates (ACM.RE ast) n = P.getPaymentDates ast n
getPaydates (ACM.PF ast) n = P.getPaymentDates ast n

remainTerms :: ACM.AssetUnion -> Int
remainTerms (ACM.MO ast) = P.getRemainTerms ast
remainTerms (ACM.LO ast) = P.getRemainTerms ast
remainTerms (ACM.IL ast) = P.getRemainTerms ast
remainTerms (ACM.LS ast) = P.getRemainTerms ast
remainTerms (ACM.FA ast) = P.getRemainTerms ast
remainTerms (ACM.RE ast) = P.getRemainTerms ast
remainTerms (ACM.PF ast) = P.getRemainTerms ast

borrowerNum :: ACM.AssetUnion -> Int
borrowerNum (ACM.MO ast) = P.getBorrowerNum ast
borrowerNum (ACM.LO ast) = P.getBorrowerNum ast
borrowerNum (ACM.IL ast) = P.getBorrowerNum ast
borrowerNum (ACM.LS ast) = P.getBorrowerNum ast
borrowerNum (ACM.FA ast) = P.getBorrowerNum ast
borrowerNum (ACM.RE ast) = P.getBorrowerNum ast
borrowerNum (ACM.PF ast) = P.getBorrowerNum ast

splitWith :: ACM.AssetUnion -> [Rate] -> [ACM.AssetUnion]
splitWith (ACM.MO ast) rs = ACM.MO <$> P.splitWith ast rs
splitWith (ACM.LO ast) rs = ACM.LO <$> P.splitWith ast rs 
splitWith (ACM.IL ast) rs = ACM.IL <$> P.splitWith ast rs
splitWith (ACM.LS ast) rs = ACM.LS <$> P.splitWith ast rs
splitWith (ACM.FA ast) rs = ACM.FA <$> P.splitWith ast rs
splitWith (ACM.RE ast) rs = ACM.RE <$> P.splitWith ast rs
splitWith (ACM.PF ast) rs = ACM.PF <$> P.splitWith ast rs
-- splitWith (ACM.RE ast) rs = ACM.RE <$> P.splitWith ast rs

updateOrigDate :: ACM.AssetUnion -> Date -> ACM.AssetUnion
updateOrigDate (ACM.MO ast) d = ACM.MO $ P.updateOriginDate ast d 
updateOrigDate (ACM.LO ast) d = ACM.LO $ P.updateOriginDate ast d 
updateOrigDate (ACM.IL ast) d = ACM.IL $ P.updateOriginDate ast d 
updateOrigDate (ACM.LS ast) d = ACM.LS $ P.updateOriginDate ast d 
updateOrigDate (ACM.FA ast) d = ACM.FA $ P.updateOriginDate ast d
updateOrigDate (ACM.RE ast) d = ACM.RE $ P.updateOriginDate ast d
updateOrigDate (ACM.PF ast) d = ACM.PF $ P.updateOriginDate ast d
-- updateOrigDate (ACM.RE ast) d = ACM.RE $ P.updateOriginDate ast d

calcAlignDate :: ACM.AssetUnion -> Date -> Date
calcAlignDate (ACM.MO ast) = P.calcAlignDate ast 
calcAlignDate (ACM.LO ast) = P.calcAlignDate ast 
calcAlignDate (ACM.IL ast) = P.calcAlignDate ast 
calcAlignDate (ACM.LS ast) = P.calcAlignDate ast 
calcAlignDate (ACM.FA ast) = P.calcAlignDate ast 
calcAlignDate (ACM.RE ast) = P.calcAlignDate ast 
calcAlignDate (ACM.PF ast) = P.calcAlignDate ast 
-- calcAlignDate (ACM.RE ast) = P.calcAlignDate ast 

calcAssetUnion :: ACM.AssetUnion -> Date -> Maybe [RateAssumption] -> Either String CF.CashFlowFrame
calcAssetUnion (ACM.MO ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.LO ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.IL ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.LS ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.FA ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.RE ast) d mRates = P.calcCashflow ast d mRates
calcAssetUnion (ACM.PF ast) d mRates = P.calcCashflow ast d mRates

projAssetUnion :: ACM.AssetUnion -> Date -> A.AssetPerf -> Maybe [RateAssumption] 
               -> Either String (CF.CashFlowFrame, Map.Map CutoffFields Balance)
projAssetUnion (ACM.MO ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.LO ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.IL ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.LS ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.FA ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.RE ast) d assumps mRates = P.projCashflow ast d assumps mRates
projAssetUnion (ACM.PF ast) d assumps mRates = P.projCashflow ast d assumps mRates

projAssetUnionList :: [ACM.AssetUnion] -> Date -> A.ApplyAssumptionType -> Maybe [RateAssumption] 
                   -> Either String (CF.CashFlowFrame, Map.Map CutoffFields Balance)
projAssetUnionList [] d (A.PoolLevel assetPerf) mRate = Right $ (CF.CashFlowFrame (0,d,Nothing) [], Map.empty)
projAssetUnionList assets d (A.PoolLevel assetPerf) mRate =
  let 
    prjList = [ projAssetUnion asset d assetPerf mRate | asset <- assets ]
    results::(Either String [(CF.CashFlowFrame, Map.Map CutoffFields Balance)]) = sequenceA prjList
  in 
    do
      r <- results
      let cfs = fst <$> r
      let bals = snd <$> r
      return (foldl1 CF.mergePoolCf2 cfs, Map.unionsWith (+) bals)

projAssetUnionList assets d _ mRate = Left " not implemented on asset level assumption for revolving pool"


projectCashflow :: MixedAsset -> Date -> Map.Map String A.ApplyAssumptionType -> Maybe [RateAssumption] 
                -> Either String (Map.Map String (CF.CashFlowFrame, Map.Map CutoffFields Balance))
projectCashflow (MixedPool assetMap) asOfDate mAssump mRate 
  = let 
      mWithCf = Map.mapWithKey
                  (\k astList -> projAssetUnionList 
                                   astList 
                                   asOfDate
                                   (case Map.lookup k mAssump of 
                                      Just assump -> assump
                                      Nothing -> error ("Failed to read sub assump:"++k))
                                   mRate)
                  assetMap
    in 
      sequenceA mWithCf
