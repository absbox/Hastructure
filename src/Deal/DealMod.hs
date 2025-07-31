{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Deal.DealMod (modDeal, ModifyType(..), AdjStrategy(..)
                     )                      
  where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
import Servant.OpenApi
import Data.OpenApi hiding (Server,contentType,trace)

import qualified Accounts as A
import qualified Ledger as LD
import qualified Asset as Ast
import qualified Expense as F
import qualified Liability as L
import qualified CreditEnhancement as CE
import qualified Hedge as HE
import qualified Waterfall as W
import qualified Cashflow as CF
import qualified Assumptions as AP
import qualified AssetClass.AssetBase as ACM
import qualified Call as C
import qualified InterestRate as IR
import qualified Util as U
import qualified Deal.DealBase as DB
import Stmt
import Lib
import Util
import DateUtil
import Types
import Revolving
import Triggers

import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Set as S
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Ratio
import Data.Aeson hiding (json)
-- import qualified Data.Aeson.Encode.Pretty as Pretty
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.IntMap (filterWithKey)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Pool as P
import qualified Types as CF

import Debug.Trace
import qualified Control.Lens as P
debug = flip trace


data AdjStrategy = ScaleBySpread
                 | ScaleByFactor
                 deriving (Show,Generic)

data ModifyType = AddSpreadToBonds BondName
                | SlideBalances BondName BondName
                deriving (Show,Generic)

-- ^ Modify a deal by various type of recipes
modDeal :: Ast.Asset a => ModifyType -> Double -> DB.TestDeal a -> DB.TestDeal a
modDeal (AddSpreadToBonds bnd) sprd d 
  = let 
      sprd' = (fromRational . toRational) sprd
      bndMap = DB.bonds d
      bndMap' = U.mapWithinMap 
                  (\b -> b & L.interestInfoTraversal %~ L.adjInterestInfoBySpread sprd'
                           & L.curRatesTraversal %~ (+ sprd')) 
                  [bnd]
                  bndMap
    in 
      d {DB.bonds = bndMap'} 

modDeal (SlideBalances bn1 bn2) r d@DB.TestDeal {DB.bonds = bndMap}
  = let 
      totalBalance = sum $ L.originBalance . L.bndOriginInfo <$> DB.viewDealBondsByNames d [bn1, bn2]
      leftBal = mulBR totalBalance (toRational r) -- `debug` ("split ratio" ++ show r)
      rightBal = totalBalance - leftBal 
      bndMap' = DB.updateBondInMap bn1 (L.adjustBalance leftBal) $
                 DB.updateBondInMap bn2 (L.adjustBalance rightBal) bndMap -- `debug` ("leftBal: " ++ show leftBal ++ ", rightBal: " ++ show rightBal )
    in 
      d {DB.bonds = bndMap'}

modDeal x _ _ = error $ "modify deal: not implemented"++ show x


$(deriveJSON defaultOptions ''AdjStrategy)
instance ToSchema AdjStrategy

$(deriveJSON defaultOptions ''ModifyType)
instance ToSchema ModifyType
