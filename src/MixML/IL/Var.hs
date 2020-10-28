{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module MixML.IL.Var where

import           Prelude

import           Control.Lens              (over, view)
import           Control.Monad.State.Class (MonadState, gets, modify)
import           Data.Extensible
import           Data.Function             (on)
import qualified Data.Map                  as Map

type Var = String

type VarState = Record
  '[ "count" >: Int
   , "table" >: Map.Map String Int
   ]

initState :: VarState
initState
    = #count @= 1
   <: #table @= Map.empty
   <: emptyRecord

type VarOps m = MonadState VarState m

fresh :: VarOps m => m Var
fresh = do
  n <- gets (view #count)
  modify $ over #count (+ 1)
  pure $ "%" ++ show n

rename :: VarOps m => Var -> m Var
rename name = do
  table <- gets (view #table :: VarState -> Map.Map String Int)
  let n = Map.findWithDefault 0 prefix table + 1
  modify $ over #table (Map.insert prefix n)
  pure $ prefix ++ "%" ++ show n
  where
    prefix = case split '%' name of
      [ident, _] -> ident
      _          -> name
    split c = words . map (\x -> if c == x then ' ' else x)

(+|+) :: Map.Map Var a -> Map.Map Var a -> Either String (Map.Map Var a)
(+|+) = fmap sequence . (Map.unionWithKey (\k _ _ -> Left $ "collision key: " ++ k) `on` fmap Right)

union' :: Map.Map Var a -> Map.Map Var a -> (Map.Map Var a)
union' = Map.unionWith (flip const)
