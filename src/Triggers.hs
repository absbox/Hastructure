{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Triggers(
    Trigger(..),TriggerEffect(..),TriggerName,trgStatusLens
)
 where

import qualified Data.Text as T
import qualified Stmt as S
import qualified Liability as L
import Text.Read (readMaybe)
import Lib 
import Types
import Accounts (ReserveAmount)
import Waterfall (Action)
import Data.Aeson ( defaultOptions )
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Maybe
import Data.Map
import GHC.Generics
import Control.Lens
import Deal.DealCollection (CollectionRule(..))

type TriggerName = String

data TriggerEffect = DealStatusTo DealStatus                           -- ^ change deal status
                   | DoAccrueFee FeeNames                              -- ^ accure fee
                   | AddTrigger Trigger                                -- ^ add a new trigger
                   | ChangeReserveBalance String ReserveAmount         -- ^ update reserve target balance  
                   | CloseDeal (Int, DatePattern) (Int, DatePattern)
                               (PricingMethod, AccountName, Maybe DealStats)   
                               (Maybe [CollectionRule])                -- ^ close the deal
                   | BuyAsset AccountName PricingMethod                -- ^ buy asset from the assumption using funds from account
                   | ChangeBondRate BondName L.InterestInfo IRate      -- ^ change bond rate
                   | TriggerEffects [TriggerEffect]                    -- ^ a combination of effects above
                   | RunActions [Action]                               -- ^ run a list of waterfall actions
                   | DoNothing                                         -- ^ do nothing
                   deriving (Show, Eq, Generic,Ord)
 
data Trigger = Trigger {
            trgCondition :: Pre                       -- ^ condition to trigger 
            ,trgEffects :: TriggerEffect              -- ^ what happen if it was triggered
            ,trgStatus :: Bool                        -- ^ if it is triggered or not 
            ,trgCurable :: Bool                       -- ^ if it is curable trigger
            ,trgStmt :: Maybe S.Statement             -- ^ Transaction stmt
            } deriving (Show, Eq, Generic,Ord)

makeLensesFor [("trgStatus","trgStatusLens") 
                ,("trgEffects","trgEffectsLens") 
                ,("trgCondition","trgConditionLens") 
                ,("trgCurable","trgCurableLens")] ''Trigger

$(concat <$> traverse (deriveJSON defaultOptions) [''TriggerEffect, ''Trigger])
