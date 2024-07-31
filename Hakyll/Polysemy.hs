{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hakyll.Polysemy (
  module Hakyll.Polysemy
) where

import Control.Monad.Fail (fail)
import GHC.Exception.Type (SomeException(..))
import qualified Hakyll
import Polysemy (embed, Embed, interpret, makeSem, Member, Members, runM, Sem)
import Polysemy.Error (Error, runError, throw)
import RIO

data ModuleError =
  ModuleError {
    function :: String,
    message :: String
  }

instance Show ModuleError where
  show (ModuleError function message) =
    "Error in Hakyll.Polysemy:"
      <> "\n\tFunction: " <> function 
      <> "\n\tMessage: " <> message

instance Exception ModuleError

data MonadMetadata m v where
  GetMetadata :: Hakyll.Identifier -> MonadMetadata m Hakyll.Metadata

$(makeSem ''MonadMetadata)

throwError :: Member (Error SomeException) r => String -> String -> Sem r a
throwError fn = throw . SomeException . ModuleError fn

runMetadata :: 
  forall m r a .
  ( Hakyll.MonadMetadata m
  , Member (Embed m) r
  ) =>
  Sem (MonadMetadata ': r) a -> Sem r a
runMetadata = interpret $ \case
  GetMetadata identifier -> embed (Hakyll.getMetadata identifier :: m Hakyll.Metadata)

type CompilerSem a = Sem '[MonadMetadata, Error SomeException, Embed Hakyll.Compiler] a

runCompiler ::
  forall a .
  CompilerSem a ->
  Hakyll.Compiler a
runCompiler a = do
  r <- runM . runError . runMetadata @Hakyll.Compiler $ a
  either (fail . show) pure r

getMetadataValue ::
  (Members '[MonadMetadata, Error SomeException] r) =>
  Hakyll.Identifier ->
  String ->
  Sem r String
getMetadataValue identifier key = do
  metadata <- getMetadata identifier
  maybe
    (throwError "getMetadataValue" $ "Key not found: " ++ key)
    pure
    $ Hakyll.lookupString key metadata

