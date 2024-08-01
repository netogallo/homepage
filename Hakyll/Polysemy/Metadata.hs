{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Hakyll.Polysemy.Metadata (
  module Hakyll.Polysemy.Metadata
) where

import Data.Aeson (Object)
import qualified Hakyll
import Polysemy (embed, Embed, interpret, makeSem, Member, Members, Sem)
import RIO

data MonadMetadata m v where
  GetMetadata :: Hakyll.Identifier -> MonadMetadata m Hakyll.Metadata

$(makeSem ''MonadMetadata)

runMetadata :: 
  forall m r a .
  ( Hakyll.MonadMetadata m
  , Members '[Embed m] r
  ) =>
  Sem (MonadMetadata ': r) a
  -> Sem r a
runMetadata = interpret $ \case
  GetMetadata identifier -> embed (Hakyll.getMetadata identifier :: m Hakyll.Metadata)

class IsMetadata a where
  metadata :: Member MonadMetadata r => a -> Sem r Hakyll.Metadata

instance IsMetadata Hakyll.Metadata where
  metadata = pure

instance IsMetadata Hakyll.Identifier where
  metadata = getMetadata

instance IsMetadata (Hakyll.Item a) where
  metadata = getMetadata . Hakyll.itemIdentifier


