{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hakyll.Polysemy (
    module Hakyll.Polysemy
  , module Hakyll.Polysemy.Metadata
) where

import Control.Monad.Fail (fail)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (parseEither)
import GHC.Exception.Type (SomeException(..))
import qualified Hakyll
import Polysemy (embed, Embed, Final, interpretH, makeSem, Member, Members, runM, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Resource (Resource(..))
import qualified Polysemy.Error as PE
import RIO

import Hakyll.Polysemy.ExternalResourceCache (ExternalResourceCache, runExternalResourceCache)
import Hakyll.Polysemy.Metadata

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

type HakyllError = Error SomeException

asError :: String -> String -> SomeException
asError fn = SomeException . ModuleError fn

throwError :: Member HakyllError r => String -> String -> Sem r a
throwError fn = throw . asError fn

type CompilerSem a = Sem '[
    MonadMetadata
  , ExternalResourceCache
  , Error SomeException
  , Embed Hakyll.Compiler
  ] a

runCompiler ::
  forall a .
  CompilerSem a ->
  Hakyll.Compiler a
runCompiler a = do
  r <- runM . runError . runExternalResourceCache . runMetadata @Hakyll.Compiler $ a
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

getMetadataObjectValue ::
  ( Members '[MonadMetadata, Error SomeException] r
  , FromJSON value ) =>
  Hakyll.Identifier ->
  String ->
  Sem r value
getMetadataObjectValue identifier key = do
  metadata <- getMetadata identifier
  PE.fromEither $
    mapLeft (asError "getMetadataObjectValue" . show) $
    parseEither ((.: "repository") >=> parseJSON) metadata

