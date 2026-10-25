{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module AssetClass.FixedAsset
  ()
  where

import qualified Data.Time as T
import Data.Ratio

import Data.Aeson
import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Aeson.Types
import GHC.Generics

import qualified Assumptions as A
import Types hiding (startDate)
import Lib
import Util
import DateUtil
import qualified Cashflow as CF

import AssetClass.AssetBase


import AssetClass.AssetCashflow
import qualified Asset as Ast
import Asset (Asset(projCashflow))
import Assumptions (AssetDelinqPerfAssumption(DummyDelinqAssump))


-- life time schedule amortization amount list
calcAmortAmt ::FixedAsset -> Either String [Balance]
calcAmortAmt fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                               ,residualBalance=rb ,capacity=cap} b rt)
  = case ar of
      StraightLine -> Right $ replicate ot $ divideBI (b-rb) rt
      DecliningBalance -> 
        let 
          amortizeRate = realToFrac $ 2 % ot
          futureBals' = scaleByFstElement b $ lastN (succ rt) $ scanl (\acc r -> acc * (1 - r)) ob (replicate ot amortizeRate)
          -- straigh lines
          futureBals'' = scanl 
                          (\acc (bal',amt',rt') ->
                            (acc - (max amt' (divideBI (acc - rb) (rt - rt'))))
                           )
                          (head futureBals')
                          (zip3 futureBals' (diffNum futureBals') [0..succ rt])
        in 
          Right (diffNum futureBals'')

 

instance Ast.Asset FixedAsset where 

  calcCashflow fa@(FixedAsset {}) asOfDay _ = 
     fst <$> projCashflow fa asOfDay (A.FixedAssetAssump (mkTs []) (mkTs []) Nothing, A.DummyDelinqAssump, A.DummyDefaultAssump) Nothing

  getCurrentBal  fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} curBal rt) 
    = curBal

  getOriginBal fa@(FixedAsset FixedAssetInfo{originBalance=ob} _ _) = ob
  -- ^ a depreciation-modelled asset carries no rate
  getOriginRate _ = 0

  resetToOrig fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} b rt) 
    = FixedAsset fai b ot
  
  getPaymentDates 
    (FixedAsset fo@FixedAssetInfo{startDate=sd ,period=p,originTerm=ot} _ rt)
    extra
    = genDates sd p (ot+extra)

  projCashflow fa@(FixedAsset fai@FixedAssetInfo{originBalance=ob, accRule=ar, originTerm=ot
                                                 ,residualBalance=rb ,capacity=cap} curBalance rt) 
               asOfDay
               (A.FixedAssetAssump uCurve pCurve mExtPeriods,_,_)
               _
    = let 
        extPeriods = fromMaybe 0 mExtPeriods
        cfLength =  rt + extPeriods
        pdates = lastN cfLength $ Ast.getPaymentDates fa extPeriods
        capacityCaps = case cap of
                        FixedCapacity b -> replicate cfLength b
                        CapacityByTerm [] -> []
                        CapacityByTerm tbl -> lastN cfLength $ concat [ replicate i b | (i,b)  <- tbl ] ++ (replicate extPeriods (snd (last tbl)))

        utilsVec = getValByDates uCurve Inc pdates
        units = [ mulBR c u | (u,c) <- zip utilsVec capacityCaps]
        prices = getValByDates pCurve Inc pdates
        cash = [ mulBR u p | (p,u) <- zip prices units]
      in 
        do 
          scheduleAmt <- calcAmortAmt fa 
          let amortizedBals = lastN cfLength $ scheduleAmt ++ replicate extPeriods 0 
          let scheduleBals = tail $ scanl (-) curBalance (amortizedBals ++ [0])
          let cumuDep = ob - curBalance
          let cumuDepreciation = tail $ scanl (+) cumuDep amortizedBals 
          let txns = zipWith6 CF.FixedFlow pdates scheduleBals amortizedBals cumuDepreciation units cash
          let futureTxns = cutBy Inc Future asOfDay txns
          let begBal = CF.buildBegBal futureTxns
          return $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) $ futureTxns, Map.empty)