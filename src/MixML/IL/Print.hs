{-# LANGUAGE LambdaCase #-}

module MixML.IL.Print where

import           Data.List       (intercalate)
import qualified MixML.IL.Syntax as IL

topP, bindP, arrowP, assignP, compP, plusP, timesP, applyP, dotP, atomP :: Int
topP    = 0
bindP   = topP + 1
arrowP  = bindP
assignP = arrowP + 1
compP   = assignP
plusP   = compP + 1
timesP  = plusP + 1
applyP  = timesP + 1
dotP    = applyP
atomP   = dotP + 1

strK :: IL.Kind -> String
strK = strK' topP

strT :: IL.Typ -> String
strT = strT' topP

strS :: IL.Sign -> String
strS = strS' topP

paren :: Int -> Int -> String -> String
paren p p' s = if p' > p then "(" ++ s ++ ")" else s

list :: (Int -> a -> String) -> [a] -> String
list str = intercalate ", " . map (str topP)

strK' :: Int -> IL.Kind -> String
strK' _ = \case
  IL.StarK    -> "T"
  IL.ArrowK 1 -> "T->T"
  IL.ArrowK n -> "T++" ++ show n ++ "->T"


strT' :: Int -> IL.Typ -> String
strT' p = \case
  IL.VarT alpha -> alpha
  IL.IntT -> "int"
  IL.StringT -> "string"
  IL.TupleT [] -> "1"
  IL.TupleT [tau] -> "(* " ++ strT' (timesP + 1) tau ++ ")"
  IL.TupleT taus ->
    paren timesP p (intercalate " * " $ map (strT' (timesP + 1)) taus)
  IL.VariantT [] -> "0"
  IL.VariantT [IL.TupleT[], IL.TupleT[]] -> "bool"
  IL.VariantT [tau] -> "(+ " ++ strT' (plusP + 1) tau ++ ")"
  IL.VariantT taus ->
    paren plusP p (intercalate " + " $ map (strT' (plusP + 1)) taus)
  IL.ArrowT sigma tau ->
    paren arrowP p (strS' (arrowP + 1) sigma ++ " -> " ++ strT' arrowP tau)
  IL.UnivT alphas tau ->
    paren bindP p ("![" ++ list strA' alphas ++ "]." ++ strT' bindP tau)
  IL.PureT alphas tau1 tau2 ->
    paren bindP p ("![" ++ list strA' alphas ++ "]." ++ strT' (arrowP + 1) tau1 ++ strT' arrowP tau2)
  IL.LambdaT alphas tau ->
    paren bindP p ("\\[" ++ list strA' alphas ++ "]." ++ strT' bindP tau)
  IL.ApplyT tau taus ->
    paren applyP p (strT' applyP tau ++ "[" ++ list strT' taus ++ "]")

strS' :: Int -> IL.Sign -> String
strS' p = \case
  IL.TypS tau k -> "[=" ++ strT' applyP tau ++ ":" ++ strK' topP k ++ "]"
  IL.TermS tau -> "[" ++ strT' topP tau ++ "]"
  IL.StructS lsigmas -> "{" ++ list (strField' ":" strS') lsigmas ++ "}"
  IL.ArrowS sigma1 sigma2 ->
    paren arrowP p (strS' (arrowP + 1) sigma1 ++ " -> " ++ strS' arrowP sigma2)
  IL.LazyS sigma -> paren applyP p ("$" ++ strS' applyP sigma)
  IL.UnivS alphaks sigma ->
    paren bindP p ("![" ++ list (strAK' "/") alphaks ++ "]." ++ strS' bindP sigma)
  IL.ExistS alphaks sigma ->
    paren bindP p ("?[" ++ list (strAK' "\\") alphaks ++ "]." ++ strS' bindP sigma)

strL' :: Int -> IL.Label -> String
strL' _ = id

strA' :: Int -> IL.Var -> String
strA' _ = id

strAK' :: String -> Int -> (IL.Var, IL.Kind) -> String
strAK' colon p (alpha, k) = strA' atomP alpha ++ colon ++ strK' p k

strField' :: String -> (Int -> a -> String) -> Int -> (IL.Label, a) -> String
strField' conn str _ (l, x) = strL' atomP l ++ conn ++ str topP x
