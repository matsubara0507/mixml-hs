{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}

module MixML.EL.Parser where

import           Prelude                   hiding (exp)

import           Control.Lens              ((%~), (&), (.~), (^.))
import           Control.Monad             (join)
import           Control.Monad.Trans.State (State, runState)
import           Data.Extensible           hiding (State)
import           Data.Functor              (($>))
import           MixML.EL.Ops
import           MixML.EL.Parser.Pos
import           MixML.EL.Parser.Token
import qualified MixML.EL.Syntax           as EL
import           MixML.IL.Var              as IL
import           Text.Megaparsec           (ParsecT, eof, try, (<|>))
import qualified Text.Megaparsec           as Parser

parse :: String -> Either String EL.Prog
parse = either (Left . show) Right . parse' progParser

parse'
  :: Parser a
  -> String
  -> Either (Parser.ParseError (Parser.Token String) EL.Error) a
parse' p = fst . flip runState IL.initState . Parser.runParserT p ""

type Parser = ParsecT EL.Error String (State IL.VarState)

progParser :: Parser EL.Prog
progParser = do
  l <- getPos
  ds <- try (decs <* eof) <|> (\m -> [("it", m)]) <$> (modl <* eof)
  r <- getPos
  struc l r ds

-- |
-- Helper Functions

struc :: EL.Pos -> EL.Pos -> [(String, EL.Modl)] -> Parser EL.Modl
struc l r = struc' []
  where
    struc' _ [] = pure $ annotate l r EL.EmptyM
    struc' gam [d] = struc'' gam d
    struc' gam (d@(x, _) : ds) = do
      self <- rename "_self"
      mdl1 <- struc' (gam ++ [(x, (self, [x]))]) ds
      mdl2 <- struc'' gam d
      pure $ annotate l (mdl1 ^. #region ^. #r) (EL.LinkM self mdl2 mdl1)
    struc'' gam (id', mdl') = do
      mdl'' <- substM gam mdl'
      pure (mdl' & #it .~ EL.StructM id' mdl'')

fct :: EL.Pos -> EL.Pos -> EL.Var -> EL.Modl -> EL.Modl -> Parser EL.Modl
fct l r x mdl1 mdl2 = do
  m <- substM [(x, (x, ["_Arg"]))] mdl2
  let mdl1' = mdl1 & #it .~ EL.StructM "_Arg" mdl1
      mdl2' = mdl2 & #it .~ EL.StructM "_Res" m
  pure (annotate l r $ EL.UnitM (annotate l r $ EL.LinkM x mdl1' mdl2'))

app :: EL.Pos -> EL.Modl -> EL.Modl -> EL.Pos -> Parser EL.Modl
app l mdl1 mdl2 r = do
  name <- rename "_app"
  pure $ annotate l r (EL.DotM (annotate l r $ EL.LinkM name mdl2' mdl1') "_Res")
  where
    mdl1' = mdl1 & #it .~ EL.NewM mdl1
    mdl2' = mdl2 & #it .~ EL.StructM "_Arg" mdl2

lete :: EL.Pos -> EL.Pos -> ([(String, EL.Modl)], EL.Exp) -> EL.Exp
lete l r (ds, e) = foldr (\(x, m) -> annotate l r . EL.LetE x m) e ds

nested :: EL.Pos -> (EL.Var, [EL.Label]) -> EL.Modl -> (EL.Var, EL.Modl)
nested _ (x, []) mdl = (x, mdl)
nested l (x, x' : ls) mdl =
  (x, annotate l (mdl ^. #region ^. #r) $ uncurry EL.StructM (nested l (x', ls) mdl))

pathM :: EL.Pos -> EL.Pos -> (String, [EL.Label]) -> EL.Modl
pathM l r (x, xs) = go (annotate l r $ EL.VarM x, xs)
  where
    go (p, [])   = p
    go (p, y:ys) = go (annotate l r $ EL.DotM p y, ys)

boolE :: EL.Pos -> EL.Pos -> Int -> EL.Exp
boolE l r i = reg $ EL.InjE (reg $ EL.TupleE []) i
  (reg $ EL.VariantT [reg $ EL.TupleT [], reg $ EL.TupleT []])
  where
    reg = annotate l r

lambdaE :: [(EL.Pos, EL.Var, EL.Typ)] -> EL.Exp -> EL.Exp
lambdaE pms e =
  foldr (\(l, x, t) e' -> annotate l r $ EL.LambdaE x t e') e pms
  where
    r = e ^. #region ^. #r

arrowT :: [(EL.Pos, EL.Var, EL.Typ)] -> EL.Typ -> EL.Typ
arrowT pms t0 =
  foldr (\(l, _, t) t' -> annotate l r $ EL.ArrowT t t') t0 pms
  where
    r = t0 ^. #region ^. #r

genE :: Maybe (EL.Pos, [EL.TypVar]) -> EL.Exp -> EL.Exp
genE Nothing e        = e
genE (Just (l, ts)) e = annotate l (e ^. #region ^. #r) (EL.GenE ts e)

univT :: Maybe (EL.Pos, [EL.TypVar]) -> EL.Typ -> EL.Typ
univT Nothing t        = t
univT (Just (l, ts)) t = annotate l (t ^. #region ^. #r) (EL.UnivT ts t)

lambdaT :: Maybe (EL.Pos, [EL.TypVar]) -> EL.Typ -> EL.Typ
lambdaT Nothing t        = t
lambdaT (Just (l, ts)) t = annotate l (t ^. #region ^. #r) (EL.LambdaT ts t)

arrowK :: Maybe (EL.Pos, [EL.TypVar]) -> EL.Kind -> EL.Kind
arrowK Nothing k = k
arrowK (Just (l, ts)) k =
  case k ^. #it of
    EL.StarK -> annotate l (k ^. #region ^. #r) (EL.ArrowK $ length ts)
    _        -> error "ground kind required" -- ToDo

kinds :: Bool -> EL.Modl -> EL.Modl
kinds stat mdl = mdl & #it %~ kinds' stat

kinds' :: Bool -> EL.Modl' -> EL.Modl'
kinds' _stat (EL.VarM x) = EL.VarM x
kinds' _stat EL.EmptyM = EL.EmptyM
kinds' _stat (EL.ValM _) = EL.EmptyM
kinds' _stat (EL.AbsValM _) = EL.EmptyM
kinds' _stat (EL.TypM _) = EL.EmptyM
kinds' _stat (EL.AbsTypM knd) = EL.AbsTypM knd
kinds' _stat (EL.DatTypM _) = EL.EmptyM
kinds' _stat (EL.AbsDatTypM _) = EL.EmptyM
kinds' _stat (EL.UnitM _) = EL.EmptyM
kinds' _stat (EL.AbsUnitM sgn) = EL.NewM (kindsS sgn)
kinds' stat (EL.NewM mdl) = if stat then EL.NewM (kinds stat mdl) else EL.EmptyM
kinds' stat (EL.StructM l mdl) = EL.StructM l (kinds stat mdl)
kinds' stat (EL.DotM mdl l) = EL.DotM (kinds stat mdl) l
kinds' stat (EL.LinkM x mdl1 mdl2) = EL.LinkM x (kinds stat mdl1) (kinds stat mdl2)
kinds' stat (EL.OLinkM _ mdl1 _) = kinds stat mdl1 ^. #it
kinds' _stat (EL.SealM _ sgn) = kindsS sgn ^. #it

kindsS :: EL.Sign -> EL.Modl
kindsS sgn = case sgn ^. #it of
  EL.ImportS mdl _ -> kinds True mdl
  EL.ExportS mdl _ -> kinds True mdl

types :: Bool -> EL.Modl -> EL.Modl
types stat mdl = mdl & #it %~ types' stat

types' :: Bool -> EL.Modl' -> EL.Modl'
types' _stat (EL.VarM x) = EL.VarM x
types' _stat EL.EmptyM = EL.EmptyM
types' _stat (EL.ValM _) = EL.EmptyM
types' _stat (EL.AbsValM t) = EL.AbsValM t
types' _stat (EL.TypM t) = EL.TypM t
types' _stat (EL.AbsTypM knd) = EL.AbsTypM knd
types' _stat (EL.DatTypM t) = EL.DatTypM t
types' _stat (EL.AbsDatTypM t) = EL.AbsDatTypM t
types' _stat (EL.UnitM _) = EL.EmptyM
types' _stat (EL.AbsUnitM sgn) = EL.NewM (typesS sgn)
types' stat (EL.NewM mdl) = if stat then EL.NewM (types stat mdl) else EL.EmptyM
types' stat (EL.StructM l mdl) = EL.StructM l (types stat mdl)
types' stat (EL.DotM mdl l) = EL.DotM (types stat mdl) l
types' stat (EL.LinkM x mdl1 mdl2) = EL.LinkM x (types stat mdl1) (types stat mdl2)
types' stat (EL.OLinkM _ mdl1 _) = types stat mdl1 ^. #it
types' _stat (EL.SealM _ sgn) = typesS sgn ^. #it

typesS :: EL.Sign -> EL.Modl
typesS sgn = case sgn ^. #it of
  EL.ImportS mdl _ -> types True mdl
  EL.ExportS mdl _ -> types True mdl

recm :: EL.Pos -> EL.Pos -> String -> EL.Modl -> EL.Modl
recm l r x mdl =
  annotate l r $ EL.LinkM x (kinds False mdl) (annotate l r $ EL.LinkM x (types False mdl) mdl)

-- |
-- Simple

lit :: Parser EL.Exp
lit
    = try (region $ EL.IntE <$> num)
  <|> try (region $ EL.StringE <$> text)
  <|> boolE <$> getPos <*> (term "true" *> getPos) <*> pure 1
  <|> boolE <$> getPos <*> (term "false" *> getPos) <*> pure 2
  -- <|> region (EL.IntE . read <$> some hexDigitChar)

lab :: Parser EL.Label
lab = ident

labs :: Parser [EL.Label]
labs = (:) <$> lab <*> (term "." *> labs <|> pure [])

labsList, labsList1 :: Parser [[EL.Label]]
labsList = labsList1
labsList1 = (:) <$> labs <*> (comma_ *> labsList1 <|> pure [])

path :: Parser (String, [EL.Label])
path = (,) <$> ident <*> (dot_ *> labs <|> pure [])

typvar :: Parser String
typvar = ident

typvarList :: Parser [String]
typvarList = typvarList1 <|> pure []

typvarList1 :: Parser [String]
typvarList1 = do
  t <- typvar
  (comma_ *> ((t :) <$> typvarList1)) <|> pure [t]

-- Module Parser

modl :: Parser EL.Modl
modl = do
  l <- getPos
  region' l (link_ *> linkModl <|> let_ *> letModl)
    <|> fn_ *> fnModl l
    <|> rec_ *> (recModl l <$> (ident <* in_) <*> modl <*> getPos)
    <|> infmodl >>= infmodl' l
  where
    linkModl = do
      name <- ident
      mdl <- (equal_ *> infmodl) <|> region (colon_ *> fmap EL.NewM infmodl)
      f <- (with_ $> EL.LinkM) <|> (seals_ $> EL.OLinkM)
      f name mdl <$> modl
    letModl = letModl1 <|> letModl2
    letModl1 = do
      l <- getPos
      ds <- decs <* in_
      mdl <- modl
      r <- getPos
      flip EL.DotM "let" <$> struc l r (ds ++ [("let", mdl)])
    letModl2 = do
      l <- getPos
      name <- ident <* equal_
      mdl1 <- modl <* in_
      mdl2 <- modl
      r <- getPos
      flip EL.DotM "let" <$> struc l r [(name, mdl1), ("let", mdl2)]
    fnModl l = do
      name <- ident
      mdl1 <- (equal_ *> infmodl) <|> region (colon_ *> fmap EL.NewM infmodl)
      mdl2 <- in_ *> modl
      r <- getPos
      fct l r name mdl1 mdl2
    recModl l x mdl r =  annotate l r $
      EL.LinkM x (kinds False mdl) (annotate l r $ EL.LinkM x (types False mdl) mdl)
    infmodl' l mdl
        = (with_ *> region' l (flip EL.LinkM mdl <$> rename "_link" <*> modl))
      <|> (seals_ *> region' l (flip EL.OLinkM mdl <$> rename "_link" <*> modl))
      <|> pure mdl

infmodl :: Parser EL.Modl
infmodl = do
  l <- getPos
  m <- appmodl
  untilMaybeM (infmodl' l) m
  where
    infmodl' l m  = seal_ *> fmap Just (region' l $ EL.SealM m <$> sign) <|> pure Nothing

appmodl :: Parser EL.Modl
appmodl
    = region ((bang_ <|> new_) *> (EL.NewM <$> atmodl))
  -- <|> app <$> getPos <*> appmodl <*> atmodl <*> getPos
  <|> atmodl

atmodl :: Parser EL.Modl
atmodl
    = lpar_ *> modl <* rpar_
  -- <|> region (EL.DotM <$> atmodl <*> (term "." *> lab)) -- todo
  <|> join (fmap flip (struc <$> getPos) <*> (lbrace_ *> decs <* rbrace_) <*> getPos)
  <|> region (EL.VarM <$> ident)
  -- <|> region (lbrack_ *> pure EL.EmptyM <* rbrack_)
  <|> bracks (val_ *> fmap EL.ValM exp)
  <|> bracks (val_ *> colon_ *> fmap EL.AbsValM typ)
  <|> bracks (type_ *> fmap EL.TypM typ)
  <|> bracks (type_ *> colon_ *> fmap EL.AbsTypM kind)
  <|> bracks (data_ *> fmap EL.DatTypM typ)
  <|> bracks (data_ *> colon_ *> fmap EL.AbsDatTypM typ)
  <|> bracks (unit_ *> fmap EL.UnitM modl)
  <|> bracks (unit_ *> colon_ *> fmap EL.AbsUnitM sign)
  -- Sugar
  <|> (lbrack_ *> module_ *> modl <* rbrack_)
  <|> bracks (module_ *> colon_ *> fmap EL.NewM modl)
  -- Sugar sugar
  <|> bracks (val_ *> linkM "_val" (fmap EL.ValM exp) (colon_ *> fmap EL.AbsValM typ))
  <|> bracks (type_ *> fmap EL.AbsTypM (region $ pure EL.StarK))
  <|> bracks (type_ *> linkM "_type" (fmap EL.TypM typ) (colon_ *> fmap EL.AbsTypM kind))
  <|> bracks (data_ *> linkM "_data" (fmap EL.DatTypM typ) (colon_ *> fmap EL.AbsTypM kind))
  <|> bracks (data_ *> linkM "_data" (colon_ *> fmap EL.AbsDatTypM typ) (colon_ *> fmap EL.AbsTypM kind))
  <|> bracks (module_ *> (EL.LinkM <$> rename "_module" <*> modl <*> (colon_ *> region (fmap EL.NewM modl))))
  <|> bracks (unit_ *> linkM "_unit" (fmap EL.UnitM modl) (colon_ *> fmap EL.AbsUnitM sign))

linkM :: String -> Parser EL.Modl' -> Parser EL.Modl' -> Parser EL.Modl'
linkM suffix p q = EL.LinkM <$> rename suffix <*> region p <*> region q

decs, dec :: Parser [(String, EL.Modl)]
decs = (++) <$> dec <*> decs <|> pure []
dec = do
  l <- getPos
  module_ *> (path >>= modl' l)
    <|> val_ *> (path >>= valM' l)
    <|> type_ *> typM' l
    <|> data_ *> (path >>= datTypM' l)
    <|> unit_ *> (path >>= unitM' l)
    <|> do_ *> fmap (: []) ((,) <$> rename "_do" <*> region' l (EL.ValM <$> exp))
    <|> comma_ $> []
  where
    modl' l p
        = equal_ *> fmap (: []) (nested l p <$> modl)
      <|> colon_ *> newModl' l p
    valM' l p = do
      g <- space *> gens <* space
      pm <- params <* space
      mdl <-
        equal_ *> fmap EL.ValM (genE g . lambdaE pm <$> exp)
         <|> colon_ *> fmap EL.AbsValM (univT g . arrowT pm <$> typ)
      r <- getPos
      pure [nested l p $ annotate l r mdl]
    typM' l = do
      lp <- getPos
      p <- path
      rp <- getPos
      space
      g <- gens
      space
      equal_ *> fmap (: []) (nested l p <$> region' l (EL.TypM . lambdaT g <$> typ))
        <|> colon_ *> typM'' l p g
        <|> pure [nested l p (annotate l rp $ EL.AbsTypM (arrowK g $ annotate lp rp EL.StarK))]
    typM'' l p g = do
      m1 <- nested l p <$> region' l (EL.AbsTypM . arrowK g <$> kind)
      m2 <- (equal_ *> fmap (: []) (nested l p <$> region' l (EL.TypM . lambdaT g <$> typ))) <|> pure []
      pure $ m1 : m2
    datTypM' l p = do
      g <- gens
      equal_ *> fmap (: []) (nested l p <$> region' l (EL.DatTypM . lambdaT g <$> typ))
        <|> colon_ *> ((typ >>= datTypMCT l p g) <|> (kind >>= datTypMCK l p g))
    datTypMCT l p g t = do
      r <- getPos
      pure [nested l p $ annotate l r (EL.AbsDatTypM $ lambdaT g t)]
    datTypMCK l p g k = do
      r <- getPos
      let m1 = nested l p $ annotate l r (EL.AbsTypM $ arrowK g k)
      m2 <- equal_ *> (nested l p <$> region' l (EL.DatTypM . lambdaT g <$> typ))
              <|> colon_ *> (nested l p <$> region' l (EL.AbsDatTypM . lambdaT g <$> typ))
      pure [m1, m2]
    unitM' l p
        = equal_ *> fmap (: []) (nested l p <$> region' l (EL.UnitM <$> modl))
      <|> colon_ *> unitM'' l p
    unitM'' l p = do
      m1 <- nested l p <$> region' l (EL.AbsUnitM <$> sign)
      m2 <- equal_ *> fmap (: []) (nested l p <$> region' l (EL.UnitM <$> modl)) <|> pure []
      pure $ m1 : m2
    newModl' l p = do
      mdl1 <- nested l p <$> region' l (EL.NewM <$> modl)
      mdl2 <- equal_ *> fmap (: []) (nested l p <$> modl) <|> pure []
      pure $ mdl1 : mdl2

params :: Parser [(EL.Pos, String, EL.Typ)]
params = try params' <|> pure []
  where
    params' = do
      l <- getPos
      name <- lpar_ *> ident
      t <- colon_ *> typ <* rpar_
      ((l, name, t) :) <$> params

gens :: Parser (Maybe (EL.Pos, [String]))
gens = gens' <|> pure Nothing
  where
    gens' = do
      l <- getPos
      ts <- lbrack_ *> typvarList <* rbrack_
      pure $ Just (l, ts)

-- |
-- Signature Parser

sign :: Parser EL.Sign
sign = do
  l <- getPos
  appsign <|> (appmodl >>= sign' l)
  where
    sign' l mdl = do
      r <- getPos
      import_ *> region' l (lpar_ *> (EL.ImportS mdl <$> labsList) <* rpar_)
        <|> export_ *> region' l (lpar_ *> (EL.ExportS mdl <$> labsList) <* rpar_)
        <|> pure (annotate l r $ EL.ImportS mdl [])

appsign :: Parser EL.Sign
appsign = atsign

atsign :: Parser EL.Sign
atsign = do
  l <- getPos
  mdl <- rpar_ *> appmodl
  import_ *> region' l (lpar_ *> (EL.ImportS mdl <$> labsList) <* rpar_ <* rpar_)
    <|> export_ *> region' l (lpar_ *> (EL.ExportS mdl <$> labsList) <* rpar_ <* rpar_)

-- |
-- Kind Parser

kind :: Parser EL.Kind
kind = do
  l <- getPos
  _ <- hash_
  r <- getPos
  arrow_ *> region' l (hash_ $> EL.ArrowK 1)
    <|> (num >>= \n -> region' l $ arrow_ *> hash_ $> EL.ArrowK n)
    <|> pure (annotate l r EL.StarK)

-- |
-- Type Parser

typ :: Parser EL.Typ
typ = do
  l <- getPos
  forall_ *> lbrack_ *> region' l (EL.UnivT <$> (typvarList <* rbrack_) <*> (arrow_ *> typ))
    <|> fn_ *> lbrack_ *> region' l (EL.LambdaT <$> (typvarList <* rbrack_) <*> (arrow_ *> typ))
    <|> inftyp

inftyp :: Parser EL.Typ
inftyp = do
  l <- getPos
  t <- apptyp
  arrow_ *> region' l (EL.ArrowT t <$> inftyp)
    <|> pure t

apptyp :: Parser EL.Typ
apptyp = do
  l <- getPos
  t <- attyp
  untilMaybeM (apptyp' l) t
  where
    apptyp' :: EL.Pos -> EL.Typ -> Parser (Maybe EL.Typ)
    apptyp' l t
        = lbrack_ *> fmap Just (region' l $ EL.ApplyT t <$> (typList <* rbrack_))
      <|> pure Nothing

attyp :: Parser EL.Typ
attyp = do
  l <- getPos
  lpar_ *> attyp' l
    <|> region' l (int_ $> EL.IntT)
    <|> region' l (string_ $> EL.StringT)
    <|> region' l (bang_ *> (EL.ModT <$> atmodl))
    <|> bool_ *> (boolT l <$> getPos)
    <|> region' l (fmap EL.ModT $ flip (pathM l) <$> path <*> getPos)
  where
    attyp' :: EL.Pos -> Parser EL.Typ
    attyp' l
        = try (typList >>= \case [t] -> pure t ; ts -> region' l (EL.TupleT ts <$ rpar_))
      <|> region' l (fmap EL.VariantT typBarlist2 <* rpar_)
    boolT l r = annotate l r $ EL.VariantT [annotate l r (EL.TupleT []), annotate l r (EL.TupleT [])]

typList, typList1, typBarlist1, typBarlist2 :: Parser [EL.Typ]
typList = typList1 <|> pure []
typList1 = (:) <$> typ <*> (comma_ *> typList1 <|> pure [])
typBarlist1 = (:) <$> typ <*> (bar_ *> typBarlist1 <|> pure [])
typBarlist2 = (:) <$> typ <*> typBarlist1

-- |
-- Expression Parser

exp :: Parser EL.Exp
exp = do
  l <- getPos
  case_ *> region' l (EL.CaseE <$> (exp <* of_) <*> idexp_barlist1)
    <|> fn_ *> fnExp l
    <|> let_ *> letExp l
    <|> if_ *> region' l (join $ ifToCase <$> (exp <* then_) <*> (exp <* else_) <*> exp)
    <|> (infexp >>= infexp' l)
  where
    fnExp l
        = lbrace_ *> region' l (EL.GenE <$> (typvarList <* rbrack_ <* arrow_) <*> exp)
      <|> region' l (EL.LambdaE <$> (ident <* colon_) <*> (apptyp <* arrow_) <*> exp)
    letExp l
        = flip (lete l) <$> ((,) <$> decs <*> (in_ *> exp)) <*> getPos
      <|> region' l (EL.LetE <$> (ident <* equal_) <*> region (EL.ValM <$> exp) <*> (in_ *> exp))
    infexp' l e = do
      r <- getPos
      region' l (flip EL.LetE (annotate l r $ EL.ValM e) <$> rename "_seq" <*> (semic_ *> exp))
        <|> pure e

infexp :: Parser EL.Exp
infexp
    = plusexp
  <|> region (EL.EqualE <$> (plusexp <* iseql_) <*> plusexp)
  <|> region (EL.LessE <$> (plusexp <* term "<") <*> plusexp)

plusexp :: Parser EL.Exp
plusexp = do
  l <- getPos
  e <- appexp
  untilMaybeM (plusexp' l) e
  where
    plusexp' l e =
      plus_ *> fmap Just (region' l $ EL.PlusE e <$> appexp)
        <|> minus_ *> fmap Just (region' l $ EL.MinusE e <$> appexp)
        <|> cat_ *> fmap Just (region' l $ EL.CatE e <$> appexp)
        <|> pure Nothing

appexp :: Parser EL.Exp
appexp = do
  l <- getPos
  e <-
    region (EL.FoldE <$> (in_ *> appmodl) <*> (lbrack_ *> typList <*rbrack_) <*> atexp)
    <|> region (EL.UnfoldE <$> (out_ *> appmodl) <*> (lbrack_ *> typList <*rbrack_) <*> atexp)
    <|> region (EL.PrintE <$> (print_ *> atexp))
    <|> region (EL.InjE <$> region (at_ $> EL.TupleE []) <*> num <*> (lbrack_ *> typ <* rbrack_))
    <|> atexp
  untilMaybeM (appexp' l) e
  where
    appexp' l e =
      lbrace_ *> fmap Just (region' l $ EL.InstE e <$> (typList <* rbrack_))
        <|> hash_ *> fmap Just (region' l $ EL.ProjE e <$> num)
        <|> at_ *> fmap Just (region' l $ EL.InjE e <$> num <*> (lbrack_ *> typ <* rbrack_))
        <|> fmap Just (region' l $ EL.ApplyE e <$> atexp)
        <|> pure Nothing

atexp :: Parser EL.Exp
atexp
    = toTuple expList
  <|> region (EL.ModE <$> (bang_ *> atmodl))
  -- Sugar
  <|> region (EL.ModE <$> (fmap flip (pathM <$> getPos) <*> path <*> getPos))
  <|> do
    l <- getPos <* lpar_
    e <- exp <* colon_
    t <- typ <* rpar_
    r <- getPos
    name1 <- rename "_colon"
    name2 <- rename "_val"
    name3 <- rename "_colon"
    pure $ annotate l r $
      EL.LetE name1
        (annotate l r $ EL.LinkM name2
          (#it @= EL.ValM e <: #region @= e ^. #region <: nil)
          (#it @= EL.AbsValM t <: #region @= t ^. #region <: nil))
        (annotate l r $ EL.ModE (annotate l r $ EL.VarM name3))
  <|> lit

expList, expList1 :: Parser [EL.Exp]
expList = expList1 <|> pure []
expList1 = (:) <$> exp <*> (comma_ *> expList1 <|> pure [])

idexp_barlist1 :: Parser [(String, EL.Exp)]
idexp_barlist1 = do
  name <- ident <* arrow_
  try ((:) <$> ((,) name <$> infexp <* bar_) <*> idexp_barlist1)
    <|> fmap (: []) ((,) name <$> exp)

-- |
-- Utils

bracks :: Parser EL.Modl' -> Parser EL.Modl
bracks p = region (lbrack_ *> try p <* rbrack_)

toTuple :: Parser [EL.Exp] -> Parser EL.Exp
toTuple p = do
  l <- getPos <* lpar_
  es <- p <* rpar_
  r <- getPos
  case es of
    [e] -> pure e
    _   -> pure $ annotate l r (EL.TupleE es)

ifToCase :: EL.Exp -> EL.Exp -> EL.Exp -> Parser EL.Exp'
ifToCase e1 e2 e3 = do
  name1 <- rename "_then"
  name2 <- rename "_else"
  pure $ EL.CaseE e1 [(name1, e2), (name2, e3)]

untilMaybeM :: Monad m => (a -> m (Maybe a)) -> a -> m a
untilMaybeM f = go
  where
    go a = maybe (pure a) go =<< f a
