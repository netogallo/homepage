{-# LANGUAGE FlexibleInstances #-}
module Hakyll.Zero (
  module Hakyll.Zero
) where

import Prelude (Maybe(..))
import qualified Control.Lens as Lens

class Zero a where
  zero :: a

instance Zero (Maybe a) where
  zero = Nothing

instance Zero (Maybe a, Maybe b) where
  zero = (Nothing, Nothing)

instance Zero (Maybe a, Maybe b, Maybe c) where
  zero = (Nothing, Nothing, Nothing)

_1 :: forall s a . (Lens.Field1 s s (Maybe a) (Maybe a), Zero s) => a -> s
_1 value = (Lens._1 Lens..~ Just value) (zero @s)

_2 :: forall s a . (Lens.Field2 s s (Maybe a) (Maybe a), Zero s) => a -> s
_2 value = (Lens._2 Lens..~ Just value) (zero @s)

