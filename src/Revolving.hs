{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}


module Revolving
  ( RevolvingPool(..)
  , lookupAssetAvailable
  )
  where

import GHC.Generics
import Language.Haskell.TH ()
import Data.Aeson
import qualified Data.Text as T ()
import qualified Cashflow as CF ()
import Data.Aeson.TH
import Data.Aeson.Types ()
import Data.Hashable ()
import Data.Fixed ()
import Data.List
import Types

import AssetClass.AssetBase


data RevolvingPool = ConstantAsset [AssetUnion]          -- ^ Assets in revolving pool stays constant after being bought
                   | StaticAsset [AssetUnion]            -- ^ Assets in revolving pool will decrease afeter being bought
                   | AssetCurve [TsPoint [AssetUnion]]   -- ^ Assets are changing by dates
                   deriving (Show,Generic)


lookupAssetAvailable :: RevolvingPool -> Date -> [AssetUnion]
lookupAssetAvailable (ConstantAsset aus) _ = aus
lookupAssetAvailable (StaticAsset aus) _ = aus
lookupAssetAvailable (AssetCurve ausCurve) d 
  = case find (\(TsPoint _d _) -> d > _d) (reverse ausCurve)  of 
      Just (TsPoint _d v) -> v
      Nothing -> [] 



$(deriveJSON defaultOptions ''RevolvingPool)
