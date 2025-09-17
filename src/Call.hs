{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Call(CallOption(..))
 where

import qualified Data.Time as T
import Lib
import Types
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

data CallOption 
    -- | triggered when pool perform balance below threshold
    = PoolBalance Balance    
    -- | triggered when bond current balance below threshold
    | BondBalance Balance    
    -- | triggered when pool factor (pool perform balance/origin balance)
    | PoolFactor Rate        
    -- | triggered when bond factor (total bonds current balance / origin balance)
    | BondFactor Rate        
    -- | triggered at date
    | OnDate Date            
    -- | triggered when after date
    | AfterDate Date         
    -- | triggered when all options were satisfied
    | And [CallOption]       
    -- | triggered when any option is satisfied
    | Or [CallOption]        
    -- | Call when PV of pool fall below
    | PoolPv Balance         
    -- | triggered when predicate evaluates to be True
    | Pre Pre                
    deriving (Show,Generic,Ord,Eq,Read)

$(deriveJSON defaultOptions ''CallOption)
