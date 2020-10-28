{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module MixML.IL.Ops where

import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)
import           MixML.IL.Syntax (union')
import qualified MixML.IL.Syntax as IL

renamesAK :: (Monad m, IL.VarOps m) => IL.TypEnv -> m (IL.TypEnv, IL.TypSubst)
renamesAK alphaks = do
  let (alphas, ks) = unzip alphaks
  (alphas', del) <- renamesA alphas
  pure (zip alphas' ks, del)

renamesA :: (Monad m, IL.VarOps m) => [IL.TypVar] -> m ([IL.TypVar], IL.TypSubst)
renamesA alphas = do
  alphas' <- mapM IL.rename alphas
  pure (alphas',  Map.fromList $ zip alphas (map IL.VarT alphas'))

substT :: (Monad m, IL.VarOps m) => IL.TypSubst -> IL.Typ -> m IL.Typ
substT del = \case
  IL.VarT alpha -> pure $ fromMaybe (IL.VarT alpha) (Map.lookup alpha del)
  IL.IntT -> pure IL.IntT
  IL.StringT -> pure IL.StringT
  IL.TupleT taus -> IL.TupleT <$> mapM (substT del) taus
  IL.VariantT taus -> IL.VariantT <$> mapM (substT del) taus
  IL.ArrowT sigma tau -> IL.ArrowT <$> substS del sigma <*> substT del tau
  IL.UnivT alphas tau -> do
    (alphas', del') <- renamesA alphas
    IL.UnivT alphas' <$> substT (union' del del') tau
  IL.PureT alphas tau1 tau2 -> do
    (alphas', del') <- renamesA alphas
    IL.PureT alphas' <$> substT (union' del del') tau1 <*> substT (union' del del') tau2
  IL.LambdaT alphas tau -> do
    (alphas', del') <- renamesA alphas
    IL.LambdaT alphas' <$> substT (union' del del') tau
  IL.ApplyT tau taus -> IL.ApplyT <$> substT del tau <*> mapM (substT del) taus

substS :: (Monad m, IL.VarOps m) => IL.TypSubst -> IL.Sign -> m IL.Sign
substS del = \case
  IL.TypS tau k -> flip IL.TypS k <$> substT del tau
  IL.TermS tau -> IL.TermS <$> substT del tau
  IL.StructS lsigmas ->
    IL.StructS <$> mapM (\(a,b) -> (,) a <$> substS del b) lsigmas
  IL.ArrowS sigma1 sigma2 ->
    IL.ArrowS <$> substS del sigma1 <*> substS del sigma2
  IL.LazyS sigma -> IL.LazyS <$> substS del sigma
  IL.UnivS alphaks sigma -> do
    (alphaks', del') <- renamesAK alphaks
    IL.UnivS alphaks' <$> substS (union' del del') sigma
  IL.ExistS alphaks sigma -> do
    (alphaks', del') <- renamesAK alphaks
    IL.ExistS alphaks' <$> substS (union' del del') sigma
