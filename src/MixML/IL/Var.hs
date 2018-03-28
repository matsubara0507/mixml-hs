{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeOperators     #-}

module MixML.IL.Var where

import           Prelude

import           Control.Lens              (over, view)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, gets, modify)
import           Data.Extensible
import qualified Data.Map                  as Map
import           Text.Megaparsec           (ParsecT)

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

class VarOps m where
  fresh :: m Var
  rename :: Var -> m Var

instance Monad m => VarOps (StateT VarState m) where
  fresh = do
    n <- gets (view #count)
    modify $ over #count (+ 1)
    pure $ "%" ++ show n
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

instance (VarOps m, Monad m)=> VarOps (ParsecT e s m) where
  fresh = lift fresh
  rename v = lift $ rename v
