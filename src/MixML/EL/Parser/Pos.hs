{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}

module MixML.EL.Parser.Pos
  ( getPos
  , annotate
  , region
  , region'
  ) where

import           Data.Extensible
import qualified MixML.EL.Syntax as EL
import           Text.Megaparsec (MonadParsec, SourcePos (..), getPosition,
                                  unPos)


region :: MonadParsec e s m => m a -> m (EL.Annotated a)
region p = do
  l <- getPos
  region' l p

region' :: MonadParsec e s m => EL.Pos -> m a -> m (EL.Annotated a)
region' l p = do
  a <- p
  r <- getPos
  pure $ annotate l r a

annotate :: EL.Pos -> EL.Pos -> a -> EL.Annotated a
annotate l r a = #it @= a <: #region @= (#l @= l <: #r @= r <: nil) <: nil

getPos :: MonadParsec e s m => m EL.Pos
getPos = fromSourcePos <$> getPosition

fromSourcePos :: SourcePos -> EL.Pos
fromSourcePos = (,) <$> unPos . sourceLine <*> unPos . sourceColumn
