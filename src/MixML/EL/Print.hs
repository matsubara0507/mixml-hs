{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module MixML.EL.Print where

import           Control.Lens    ((^.))
import           Data.List       (intercalate)
import qualified MixML.EL.AuxReg as Aux
import qualified MixML.EL.Syntax as EL
import qualified MixML.IL.Print  as IL
import qualified MixML.IL.Syntax as IL


strS :: Aux.Sign -> String
strS = \case
  Aux.Typ tau k pmo  ->
    mconcat ["[", IL.strT tau, ":", IL.strK k, "]", maybe "" strPM pmo]
  Aux.Term tau pm    -> "[" ++ IL.strT tau ++ "]" ++ strPM pm
  Aux.Funct phi pm   -> "[" ++ strF phi ++ "]" ++ strPM pm
  Aux.Struct lsigmas ->
    mconcat ["{", list (\(l, sigma) -> l ++ " : " ++ strS sigma) lsigmas, "}"]

list :: (a -> String) -> [a] -> String
list strX = intercalate ", " . map strX

strPM :: Aux.Variance -> String
strPM Aux.Plus  = "+"
strPM Aux.Minus = "-"

strAK :: (IL.Var, IL.Kind) -> String
strAK (alpha, k) = alpha ++ ":" ++ IL.strK k

strF :: Aux.Funct -> String
strF (Aux.F alphaks betaks sigma) =
  mconcat ["!(", list strAK alphaks, ").", "?(", list strAK betaks, ").", strS sigma]

strKind :: EL.Kind -> String
strKind k = case k ^. #it of
  EL.StarK    -> "StarK"
  EL.ArrowK n -> "ArrowK(" ++ show n ++ ")"

strTyp :: EL.Typ -> String
strTyp t = case t ^. #it of
  EL.ModT _           -> "ModT(_)"
  EL.LambdaT alphas _ -> "LambdaT(" ++ strList strVar alphas ++ ", _)"
  EL.ApplyT _ _       -> "ApplyT(_, _)"
  EL.IntT             -> "IntT"
  EL.StringT          -> "StringT"
  EL.TupleT _         -> "TupleT(_)"
  EL.VariantT _       -> "VariantT(_)"
  EL.ArrowT _ _       -> "ArrowT(_, _)"
  EL.UnivT _ _        -> "UnivT(_, _)"

strExp :: EL.Exp -> String
strExp e = case e ^. #it of
  EL.ModE _        -> "ModE(_)"
  EL.IntE n        -> "IntE(" ++ show n ++ ")"
  EL.StringE s     -> "StringE(\"" ++ s ++ "\")"
  EL.PlusE _ _     -> "PlusE(_, _)"
  EL.MinusE _ _    -> "MinusE(_, _)"
  EL.EqualE _ _    -> "EqualE(_, _)"
  EL.LessE _ _     -> "LessE(_, _)"
  EL.CatE _ _      -> "CatE(_, _)"
  EL.TupleE _      -> "TupleE(_)"
  EL.ProjE _ i     -> "ProjE(_, " ++ show i ++ ")"
  EL.InjE _ _ _    -> "InjE(_, _, _)"
  EL.CaseE _ _     -> "CaseE(_, _)"
  EL.LambdaE x _ _ -> "LambdaE(" ++ strVar x ++ ", _, _)"
  EL.ApplyE _ _    -> "ApplyE(_, _)"
  EL.GenE alphas _ -> "GenE(" ++ strList strVar alphas ++ ", _)"
  EL.InstE _ _     -> "Inst(_, _)"
  EL.FoldE _ _ _   -> "FoldE(_, _, _)"
  EL.UnfoldE _ _ _ -> "UnfoldE(_, _, _)"
  EL.LetE x _ _    -> "LetE(" ++ strVar x ++ ", _, _)"
  EL.PrintE _      -> "PrintE(_)"

strModl :: EL.Modl -> String
strModl m = case m ^. #it of
  EL.VarM x       -> "VarM(" ++ strVar x ++ ")"
  EL.EmptyM       -> "EmptyM"
  EL.ValM _       -> "ValM(_)"
  EL.AbsValM _    -> "AbsValM(_)"
  EL.TypM _       -> "TypM(_)"
  EL.AbsTypM _    -> "AbsTypM(_)"
  EL.DatTypM _    -> "DatTypM(_)"
  EL.AbsDatTypM _ -> "AbsDatTypM(_)"
  EL.UnitM _      -> "UnitM(_)"
  EL.AbsUnitM _   -> "AbsUnitM(_)"
  EL.NewM _       -> "NewM(_)"
  EL.StructM l _  -> "StructM(" ++ strVar l ++ ", _)"
  EL.DotM _ l     -> "DotM(_, " ++ strVar l ++ ")"
  EL.LinkM x _ _  -> "LinkM(" ++ strVar x ++ ", _, _)"
  EL.OLinkM x _ _ -> "OLinkM(" ++ strVar x ++ ", _, _)"
  EL.SealM _ _    -> "SealM(_, _)"

strSign :: EL.Sign -> String
strSign s = case s ^. #it of
  EL.ImportS _ lss -> "ImportS(_, " ++ strList strPath lss ++ ")"
  EL.ExportS _ lss -> "ExportS(_, " ++ strList strPath lss ++ ")"

strVar :: EL.Var -> String
strVar x = "\"" ++ x ++ "\""

strPath :: EL.Labels -> String
strPath ls = strVar (intercalate "." ls)

strList :: (a -> String) -> [a] -> String
strList strX xs = "[" ++ intercalate ", " (map strX xs) ++ "]"
