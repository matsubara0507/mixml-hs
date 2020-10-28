{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module MixML.EL.Ops where

import           Control.Lens    ((&), (.~), (^.))
import           Data.Maybe      (fromMaybe)
import qualified MixML.EL.Syntax as EL
import           MixML.IL.Var    as IL

type PathSubst = [(String, EL.Path)]

renameX :: (Monad m, VarOps m) => EL.Var -> m (EL.Var, PathSubst)
renameX x = do
  x' <- IL.rename x
  pure (x', [(x, (x', []))])

substE :: (Monad m, VarOps m) => PathSubst -> EL.Exp -> m EL.Exp
substE gam exp' = do
  e <- substE' (exp' ^. #it)
  pure (exp' & #it .~ e)
  where
    substE' = \case
      EL.ModE m        -> EL.ModE <$> substM gam m
      EL.FoldE m t e   ->
        EL.FoldE <$> substM gam m <*> mapM (substT gam) t <*> substE gam e
      EL.UnfoldE m t e ->
        EL.UnfoldE <$> substM gam m <*> mapM (substT gam) t <*> substE gam e
      EL.IntE n        -> pure $ EL.IntE n
      EL.StringE s     -> pure $ EL.StringE s
      EL.PlusE e1 e2   -> EL.PlusE <$> substE gam e1 <*> substE gam e2
      EL.MinusE e1 e2  -> EL.MinusE <$> substE gam e1 <*> substE gam e2
      EL.EqualE e1 e2  -> EL.EqualE <$> substE gam e1 <*> substE gam e2
      EL.LessE e1 e2   -> EL.LessE <$> substE gam e1 <*> substE gam e2
      EL.CatE e1 e2    -> EL.CatE <$> substE gam e1 <*> substE gam e2
      EL.TupleE es     -> EL.TupleE <$> mapM (substE gam) es
      EL.ProjE e i     -> flip EL.ProjE i <$> substE gam e
      EL.InjE e i t    -> flip EL.InjE i <$> substE gam e <*> substT gam t
      EL.CaseE e es    -> do
        let f e' (x', gam') = (,) x' <$> substE (gam ++ gam') e'
        es' <- mapM (\(x, e') -> f e' =<< renameX x) es
        flip EL.CaseE es' <$> substE gam e
      EL.LambdaE x t e -> do
        (x', gam') <- renameX x
        EL.LambdaE x' <$> substT gam t <*> substE (gam ++ gam') e
      EL.ApplyE e1 e2  -> EL.ApplyE <$> substE gam e1 <*> substE gam e2
      EL.GenE alphas e -> EL.GenE alphas <$> substE gam e
      EL.InstE e ts    -> EL.InstE <$> substE gam e <*> mapM (substT gam) ts
      EL.LetE x m e    -> do
        (x', gam') <- renameX x
        EL.LetE x' <$> substM gam m <*> substE (gam ++ gam') e
      EL.PrintE e      -> EL.PrintE <$> substE gam e

substT :: (Monad m, VarOps m) => PathSubst -> EL.Typ -> m EL.Typ
substT gam typ' = do
  t <- substT' (typ' ^. #it)
  pure (typ' & #it .~ t)
  where
    substT' = \case
      EL.ModT m           -> EL.ModT <$> substM gam m
      EL.IntT             -> pure EL.IntT
      EL.StringT          -> pure EL.StringT
      EL.TupleT ts        -> EL.TupleT <$> mapM (substT gam) ts
      EL.VariantT ts      -> EL.VariantT <$> mapM (substT gam) ts
      EL.ArrowT t1 t2     -> EL.ArrowT <$> substT gam t1 <*> substT gam t2
      EL.UnivT alphas t   -> EL.UnivT alphas <$> substT gam t
      EL.LambdaT alphas t -> EL.LambdaT alphas <$> substT gam t
      EL.ApplyT t ts      -> EL.ApplyT <$> substT gam t <*> mapM (substT gam) ts

substM :: (Monad m, VarOps m) => PathSubst -> EL.Modl -> m EL.Modl
substM gam mdl' = do
  m <- substM' (mdl' ^. #it)
  pure (mdl' & #it .~ m)
  where
    substM' = \case
      EL.VarM x         -> do
        let (x', ls) = fromMaybe (x, []) $ lookup x gam
        pure $ foldl (\m l -> EL.DotM (mdl' & #it .~ m) l) (EL.VarM x') ls
      EL.EmptyM         -> pure EL.EmptyM
      EL.ValM e         -> EL.ValM <$> substE gam e
      EL.AbsValM t      -> EL.AbsValM <$> substT gam t
      EL.TypM t         -> EL.TypM <$> substT gam t
      EL.AbsTypM k      -> pure $ EL.AbsTypM k
      EL.DatTypM t      -> EL.DatTypM <$> substT gam t
      EL.AbsDatTypM t   -> EL.AbsDatTypM <$> substT gam t
      EL.UnitM m        -> EL.UnitM <$> substM gam m
      EL.AbsUnitM s     -> EL.AbsUnitM <$> substS gam s
      EL.NewM m         -> EL.NewM <$> substM gam m
      EL.StructM l m    -> EL.StructM l <$> substM gam m
      EL.DotM m l       -> flip EL.DotM l <$> substM gam m
      EL.LinkM x m1 m2  -> do
        (x', gam') <- renameX x
        EL.LinkM x' <$> substM gam m1 <*> substM (gam ++ gam') m2
      EL.OLinkM x m1 m2 -> do
        (x', gam') <- renameX x
        EL.OLinkM x' <$> substM gam m1 <*> substM (gam ++ gam') m2
      EL.SealM m s      -> EL.SealM <$> substM gam m <*> substS gam s

substS :: (Monad m, VarOps m) => PathSubst -> EL.Sign -> m EL.Sign
substS gam sign' = do
  s <- substS' (sign' ^. #it)
  pure (sign' & #it .~ s)
  where
    substS' = \case
      EL.ImportS m lss -> flip EL.ImportS lss <$> substM gam m
      EL.ExportS m lss -> flip EL.ExportS lss <$> substM gam m
