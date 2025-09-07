{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealDate (DealDates,getClosingDate,getFirstPayDate,getLastPayDate) 
  where

import qualified Data.Map as Map
import Deal.DealBase
import Types
import Lib

class DealDates a where 
  getClosingDate :: a -> Either String Date
  getFirstPayDate :: a -> Date
  getLastPayDate :: a -> Either String Date

instance DealDates DateDesp where 
  getClosingDate (GenericDates m) = case Map.lookup ClosingDate m of 
                                      Just (SingletonDate x) -> Right x
                                      Nothing -> Left $ "ClosingDate not found in GenericDates"++show m
  getClosingDate (PreClosingDates _ x _ _ _ _) = Right x
  getClosingDate (CurrentDates (_,cd) _ _ _ _ ) = Right cd
  -- getClosingDate (AccruedGenericDates m) = getClosingDate (GenericDates m)


  getLastPayDate (GenericDates m) = case Map.lookup LastPayDate m of 
                                      Just (SingletonDate x) -> Right x
                                      Nothing -> Left $ "LastPayDate not found in GenericDates"++ show m
  getLastPayDate (CurrentDates (_,cd) _ _ _ _ ) = Right cd
  getLastPayDate (PreClosingDates {}) = Left "Error : try to get last pay date from PreClosingDates"
  -- getLastPayDate (AccruedGenericDates m) = getLastPayDate (GenericDates m)

  getFirstPayDate (PreClosingDates _ _ _ _ _ (fp,_)) = fp
  getFirstPayDate (CurrentDates _ _ _ _ (cpay,_)) = cpay    
  getFirstPayDate (GenericDates m) = case Map.lookup FirstPayDate m of
                                        Just (SingletonDate x) -> x
                                        Nothing -> error "FirstPayDate not found in GenericDates"            
  -- getFirstPayDate (AccruedGenericDates m) = getFirstPayDate (GenericDates m)

