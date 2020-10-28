-- windows can't use `aux.*` to filename
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module MixML.EL.AuxReg where

import           Prelude         hiding (abs)

import           Control.Monad   (unless)
import           Data.Foldable   (foldlM)
import qualified Data.Map        as Map
import           Data.Maybe      (mapMaybe)
import qualified MixML.EL.Syntax as EL
import qualified MixML.IL.Ops    as IL
import           MixML.IL.Syntax (union', (+|+))
import qualified MixML.IL.Syntax as IL

data Variance = Plus | Minus deriving (Show, Eq)

data Sign
  = Struct [(IL.Label, Sign)]
  | Funct Funct Variance
  | Term IL.Typ Variance
  | Typ IL.Typ IL.Kind (Maybe Variance)
  deriving (Show, Eq)

data Funct =
  F IL.TypEnv IL.TypEnv Sign
  deriving (Show, Eq)

type TypContext = Map.Map String IL.Kind
type ModlContext = Map.Map String Sign
type Locator = Map.Map IL.Var [IL.Label]

data Error
  = At
  | Locate [IL.Label]
  | Export [IL.Label]
  deriving (Show, Eq)

staticT :: IL.Typ
staticT = IL.TupleT [IL.TupleT []]

staticS :: Sign -> Sign
staticS = \case
  Struct lsigmas -> Struct (map (fmap staticS) lsigmas)
  Funct phi pm   -> Funct (staticF phi) pm
  Term _tau pm   -> Term staticT pm
  Typ tau k pmo  -> Typ tau k pmo

staticF :: Funct -> Funct
staticF (F alphaks betaks sigma) = F alphaks betaks (staticS sigma)

abs :: Sign -> Sign
abs = \case
  Struct lsigmas -> Struct (map (fmap abs) lsigmas)
  Funct phi _pm  -> Funct phi Plus
  Term tau _pm   -> Term tau Plus
  Typ tau k pmo  -> Typ tau k (fmap (const Plus) pmo)

locator :: Variance -> Sign -> Either String Locator
locator pm = \case
  Struct lsigmas ->
    foldlM (+|+) mempty =<< mapM (\(l, sigma) -> fmap (\ls -> l:ls) <$> locator pm sigma) lsigmas
  Funct _phi _pm -> pure mempty
  Term _tau _pm -> pure mempty
  Typ (IL.VarT alpha) _k (Just pm') ->
    pure $ if pm == pm' then Map.singleton alpha [] else mempty
  Typ _tau _k (Just _pm') -> Left "locator: malformed signature"
  Typ _tau _k _  -> pure mempty

neg :: Sign -> Sign
neg = \case
  Struct lsigmas -> Struct (map (fmap neg) lsigmas)
  Funct phi pm   -> Funct phi (inv pm)
  Term tau pm    -> Term tau (inv pm)
  Typ tau k pmo  -> Typ tau k (fmap inv pmo)

inv :: Variance -> Variance
inv Plus  = Minus
inv Minus = Plus

equivS :: TypContext -> (Sign, Sign) -> Bool
equivS = undefined

substS :: (Monad m, IL.VarOps m) => IL.TypSubst -> Sign -> m Sign
substS del = \case
  Struct lsigmas -> Struct <$> mapM (\(x,y) -> (,) x <$> substS del y) lsigmas
  Funct phi pm   -> flip Funct pm <$> substF del phi
  Term tau pm    -> flip Term pm <$> IL.substT del tau
  Typ tau k pmo  -> Typ <$> IL.substT del tau <*> pure k <*> pure pmo

substF :: (Monad m, IL.VarOps m) => IL.TypSubst -> Funct -> m Funct
substF del (F alphaks betaks sigma) = do
  (alphaks', del1') <- IL.renamesAK alphaks
  (betaks',  del2') <- IL.renamesAK betaks
  sigma' <- substS (del `union'` del1' `union'` del2') sigma
  pure $ F alphaks' betaks' sigma'

-- substG :: (Monad m, IL.VarOps m) => IL.TypSubst -> m a -> m a
-- substG = undefined

typstruct :: IL.Typ -> IL.Kind -> Maybe Variance -> Sign
typstruct tau k pmo = Struct [("type", Typ tau k pmo)]

export :: [EL.Labels] -> Sign -> Either Error Sign
export [] sigma = pure $ sigma
export [[]] (Funct phi Minus) = pure $ Funct phi Plus
export [[]] (Term tau Minus) = pure $ Term tau Plus
export [[]] (Typ tau k pmo) = pure $ Typ tau k (fmap (const Plus) pmo)
export lss (Struct lsigmas) = do
  let lss' li = mapMaybe (\case [] -> Just [] ; (l:ls) -> if l == li then Just ls else Nothing) lss
      struc = Map.fromList lsigmas
      appendExportError li = \case
        Right a          -> Right a
        Left (Export ls) -> Left (Export $ li:ls)
        Left e           -> Left e
  mapM_ (\case [] -> pure () ; ls@(l:_) -> unless (Map.member l struc) (Left $ Export ls)) lss
  lsigmas' <- mapM (\(li, psii) -> (,) li <$> appendExportError li (export (lss' li) psii)) lsigmas
  pure $ Struct lsigmas'
export lss _psi = Left $ Export (head lss)
