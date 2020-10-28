{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module MixML.EL.Elaboration where

import           Control.Lens                   ((^.))
import           Control.Monad                  (filterM)
import           Control.Monad.Error.Class      (throwError)
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.List                      (intercalate)
import qualified Data.Map                       as Map
import           Data.Monoid                    ((<>))
import qualified Data.Set                       as Set
import qualified MixML.EL.AuxReg                as Aux
import qualified MixML.EL.Syntax                as EL
import qualified MixML.IL.Ops                   as IL
import qualified MixML.IL.Syntax                as IL

type Context = Record
  '[ "typ"  >: Aux.TypContext
   , "modl" >: Aux.ModlContext
   ]

type Elab = Eff
  '[ EitherDef EL.Error
   , StateDef IL.VarState
   ]

emptyContext :: Context
emptyContext = #typ @= mempty <: #modl @= mempty <: nil

elab :: Context -> EL.Prog -> Elab ((Context, Aux.Sign), IL.Modl)
elab ctx mdl =
  elabU ctx mdl >>= \case
    x@((ctx', s@(Aux.Struct _)), _)
      | null (ctx' ^. #typ)
          -> pure x
      | not (Aux.equivS (ctx ^. #typ <> ctx' ^. #typ) (s, Aux.abs s))
          -> throwError $ EL.Error (mdl ^. #region) "program has left-over imports"
    ((ctx', _), _)
      | null (ctx' ^. #typ)
          -> throwError $ EL.Error (mdl ^. #region) "program is not a structure"
    _     -> throwError $ EL.Error (mdl ^. #region) "program has left-over imports"

-- Units
elabU :: Context -> EL.Prog -> Elab ((Context, Aux.Sign), IL.Modl)
elabU ctx mdl = undefined

elabT :: Context -> EL.Typ -> Elab (IL.Typ, IL.Kind)
elabT = undefined

elabK :: EL.Kind -> IL.Kind
elabK k = case k ^. #it of
  EL.StarK    -> IL.StarK
  EL.ArrowK n -> IL.ArrowK n

-- Template Modules
templateM :: Context -> EL.Prog -> [IL.Var] -> Elab (Aux.Sign, IL.TypEnv, IL.TypEnv)
templateM = templateM_ -- todo add trace

templateM_ :: Context -> EL.Prog -> [IL.Var] -> Elab (Aux.Sign, IL.TypEnv, IL.TypEnv)
templateM_ ctx mdl ls = case mdl ^. #it of
  EL.VarM x -> do
    let err = throwError $ EL.Error (mdl ^. #region) ("unbound variable " ++ x)
    sigma <- maybe err pure $ Map.lookup x (ctx ^. #modl)
    pure (Aux.abs $ Aux.staticS sigma, [], [])
  EL.EmptyM    -> pure (Aux.Struct [], [], [])
  EL.ValM _    -> pure (Aux.Term Aux.staticT Aux.Plus, [], [])
  EL.AbsValM _ -> pure (Aux.Term Aux.staticT Aux.Minus, [], [])
  EL.TypM t -> do
    (tau, k) <- elabT ctx t
    pure (Aux.typstruct tau k Nothing, [], [])
  EL.AbsTypM k -> do
    alpha <- IL.rename $ if null ls then "_type" else intercalate "." ls
    let k' = elabK k
    pure (Aux.typstruct (IL.VarT alpha) k' (Just Aux.Minus), [(alpha, k')], [])
  EL.DatTypM t -> do
    (_tau, k) <- elabT ctx t
    beta <- IL.rename $ if null ls then "_data" else intercalate "." ls
    pure (Aux.typstruct (IL.VarT beta) k (Just Aux.Plus), [], [(beta, k)])
  EL.AbsDatTypM t -> do
    (_tau, k) <- elabT ctx t
    alpha <- IL.rename $ if null ls then "_data" else intercalate "." ls
    pure (Aux.typstruct (IL.VarT alpha) k (Just Aux.Minus), [(alpha, k)], [])
  EL.UnitM m ->
    (\f -> (Aux.Funct f Aux.Plus, [], [])) <$> templateU ctx m
  EL.AbsUnitM s -> (\f -> (Aux.Funct f Aux.Minus, [], [])) <$> templateS ctx s []
  EL.NewM m -> do
    (alphaks', betaks', sigma') <- templateM ctx m ls >>= \case
      (Aux.Funct (Aux.F a b c) Aux.Plus, [], []) -> pure (a, b, c)
      (Aux.Funct _ Aux.Minus, _, _) -> throwError $ EL.Error (mdl ^. #region) "undefined unit"
      _                             -> throwError $ EL.Error (mdl ^. #region) "module not a unit"
    (alphaks, del1) <- renamesAKls alphaks' ls
    (betaks,  del2) <- renamesAKls betaks' ls
    sigma <- Aux.substS (IL.union' del1 del2) sigma'
    pure (sigma, alphaks, betaks)
  EL.StructM l modl -> do
    (sigma, alphaks, betaks) <- templateM ctx modl (ls `mappend` [l])
    pure (Aux.Struct [(l, sigma)], alphaks, betaks)
  _ -> undefined


templateU :: Context -> EL.Modl -> Elab Aux.Funct
templateU ctx mdl = do
  -- todo trace and assertion
  (sigma, alphaks, betaks) <- templateM ctx mdl []
  pure $ Aux.F alphaks betaks sigma

templateS :: Context -> EL.Sign -> EL.Labels -> Elab Aux.Funct
templateS ctx sign ls = templateS_ ctx sign ls -- todo trace and assertion

templateS_ :: Context -> EL.Sign -> EL.Labels -> Elab Aux.Funct
templateS_ ctx sign ls = case sign ^. #it of
  EL.ExportS modl lss -> do
    (sigma', alphaks', betaks') <- templateM ctx modl ls
    sigma   <- either toErr pure $ Aux.export lss sigma'
    alphaks <- either toErr pure $ filterM (hasKeyM (Aux.Minus, sigma)) alphaks'
    betaks  <- either toErr pure $ filterM (hasKeyM (Aux.Plus, sigma)) alphaks'
    if not (null betaks') then
      throwError $ EL.Error (sign ^. #region) "non-abstract module in signature expression"
    else
      pure $ Aux.F alphaks betaks sigma
  EL.ImportS modl lss -> do
    (sigma', alphaks', betaks') <- templateM ctx modl ls
    sigma   <- either toErr pure $ Aux.neg <$> Aux.export lss sigma'
    alphaks <- either toErr pure $ filterM (hasKeyM (Aux.Minus, sigma)) alphaks'
    betaks  <- either toErr pure $ filterM (hasKeyM (Aux.Plus, sigma)) alphaks'
    if not (null betaks') then
      throwError $ EL.Error (sign ^. #region) "non-abstract module in signature expression"
    else
      pure $ Aux.F alphaks betaks sigma
  where
    toErr :: Show a => a -> Elab b
    toErr = throwError . EL.Error (sign ^. #region) . show
    hasKeyM (pm, s) (k, _) = (Set.member k . Map.keysSet) <$> Aux.locator pm s

renamesAKls :: IL.TypEnv -> EL.Labels -> Elab (IL.TypEnv, IL.TypSubst)
renamesAKls alphaks ls = do
  (alphaks', del) <- IL.renamesAK alphaks
  let prefix = if null ls then "" else intercalate "." ls ++ "."
      alphaks'' = map (\(alpha, x) -> (prefix ++ alpha, x)) alphaks'
      del' = fmap (\case IL.VarT(alpha) -> IL.VarT(prefix ++ alpha) ; tau -> tau) del
  pure (alphaks'', del')
