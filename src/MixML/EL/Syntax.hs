{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MixML.EL.Syntax where

import           Data.Extensible
import qualified MixML.IL.Var    as IL

type Pos = (Int, Int)
type Region = Record '[ "l" >: Pos, "r" >: Pos ]
type Annotated a = Record '[ "it" >: a, "region" >: Region ]

type Label = String
type Var = IL.Var
type TypVar = String
type Labels = [Label]
type Path = (Var, Labels)

data Kind'
  = StarK
  | ArrowK Int
  deriving (Show, Eq)

type Kind = Annotated Kind'

data Typ'
  = ModT Modl
  | LambdaT [TypVar] Typ
  | ApplyT Typ [Typ]
  | IntT
  | StringT
  | TupleT [Typ]
  | VariantT [Typ]
  | ArrowT Typ Typ
  | UnivT [TypVar] Typ
  deriving (Show, Eq)

data Exp'
  = ModE Modl
  | IntE Int
  | StringE String
  | PlusE Exp Exp
  | MinusE Exp Exp
  | EqualE Exp Exp
  | LessE Exp Exp
  | CatE Exp Exp
  | TupleE [Exp]
  | ProjE Exp Int
  | InjE Exp Int Typ
  | CaseE Exp [(Var, Exp)]
  | LambdaE Var Typ Exp
  | ApplyE Exp Exp
  | GenE [TypVar] Exp
  | InstE Exp [Typ]
  | FoldE Modl [Typ] Exp
  | UnfoldE Modl [Typ] Exp
  | LetE Var Modl Exp
  | PrintE Exp
  deriving (Show, Eq)

data Modl'
  = VarM Var
  | EmptyM
  | ValM Exp
  | AbsValM Typ
  | TypM Typ
  | AbsTypM Kind
  | DatTypM Typ
  | AbsDatTypM Typ
  | UnitM Modl
  | AbsUnitM Sign
  | NewM Modl
  | StructM Label Modl
  | DotM Modl Label
  | LinkM Var Modl Modl
  | OLinkM Var Modl Modl
  | SealM Modl Sign
  deriving (Show, Eq)

data Sign'
  = ImportS Modl [Labels]
  | ExportS Modl [Labels]
  deriving (Show, Eq)

type Typ = Annotated Typ'
type Exp = Annotated Exp'
type Modl = Annotated Modl'
type Sign = Annotated Sign'

type Prog = Modl

newtype Error = Errpr (Region, String) deriving (Show, Eq, Ord)
