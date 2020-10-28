module MixML.IL.Syntax
  ( module MixML.IL.Var
  , Label
  , TypVar
  , Kind (..)
  , Typ (..)
  , Sign (..)
  , Effect (..)
  , Context
  , TypEntry (..)
  , TypSubst
  , TypContext
  , ModlContext
  , Term (..)
  , Modl (..)
  , ModlSubst
  , TypEnv
  ) where

import           Data.Map     (Map)
import           MixML.IL.Var

type Label = Var
type TypVar = Var

data Kind
  = StarK
  | ArrowK Int
  deriving (Show, Eq)

data Typ
  = VarT TypVar
  | IntT
  | StringT
  | TupleT [Typ]
  | VariantT [Typ]
  | ArrowT Sign Typ
  | UnivT [TypVar] Typ
  | PureT [TypVar] Typ Typ
  | LambdaT [TypVar] Typ
  | ApplyT Typ [Typ]
  deriving (Show, Eq)

data Sign
  = TypS Typ Kind
  | TermS Typ
  | StructS [(Label, Sign)]
  | ArrowS Sign Sign
  | LazyS Sign
  | UnivS [(TypVar, Kind)] Sign
  | ExistS [(TypVar, Kind)] Sign
  deriving (Show, Eq)

type TypSubst = Map Var Typ

data Effect
   = DownE [TypVar]
   | EquiE [TypVar] Typ
   | IsoE [TypVar] Typ
   deriving (Show, Eq)

type Context a = Map Var a

data TypEntry
  = Colon Kind
  | Up Kind
  | Down Kind
  | Equi Typ Kind
  | Iso Typ Kind
  deriving (Show, Eq)

type TypContext = Context TypEntry
type ModlContext = Context Sign

data Term
  = ValE Modl
  | IntE Int
  | StringE String
  | PlusE Term Term
  | MinusE Term Term
  | EqualE Term Term
  | LessE Term Term
  | CatE Term Term
  | TupleE [Term]
  | DotE Term Int
  | VariantE Term Int Typ
  | CaseE Term [(Var, Term)]
  | LambdaE Var Sign Term
  | ApplyE Term Modl
  | GenE [TypVar] Term
  | InstE Term [Typ]
  | FoldE TypVar
  | UnfoldE TypVar
  | ConE Term [Typ] Term
  | PrintE Term
  deriving (Show, Eq)

data Modl
  = VarM Var
  | TypM Typ
  | TermM Term
  | StructM [(Label, Modl)]
  | DotM Modl Label
  | LambdaM Var Sign Modl
  | ApplyM Modl Modl
  | LetM Var Modl Modl
  | GenDownM [(TypVar, Kind)] Modl
  | InstDownM Modl [Typ]
  | GenUpM [(TypVar, Kind)] Modl
  | InstUpM Modl [TypVar]
  | NewTypM [(TypVar, Kind)] Modl
  | DefEquiM TypVar Typ Modl Sign TypSubst ModlSubst (Maybe (TypContext, ModlContext)) -- ref
  | DefIsoM TypVar Typ Modl Sign
  | NewTermM Var Sign Modl
  | AssignM Modl Modl
  | ForceM Modl
  deriving (Show, Eq)

type ModlSubst = Context Modl

type TypEnv = [(TypVar, Kind)]
