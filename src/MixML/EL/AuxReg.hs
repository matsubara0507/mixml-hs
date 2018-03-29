-- windows can't use `aux.*` to filename

module MixML.EL.AuxReg where

import           Prelude         hiding (abs)

import qualified Data.Map        as Map
import qualified MixML.EL.Syntax as EL
import qualified MixML.IL.Syntax as IL

data Variance = Plus | Minus deriving (Show, Eq)

data Sign
  = Struct [(IL.Label, Sign)]
  | Funct Funct Variance
  | Term IL.Typ Variance
  | Typ IL.Typ IL.Kind (Maybe Variance)
  deriving (Show, Eq)

data Funct =
  F [(IL.TypVar, IL.Kind)] [(IL.TypVar, IL.Kind)] Sign
  deriving (Show, Eq)

type TypContext = Map.Map String IL.Kind
type ModlContext = Map.Map String Sign
type Locator = Map.Map String [IL.Label]
