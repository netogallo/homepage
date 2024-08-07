{-# LANGUAGE AllowAmbiguousTypes #-}
module Hakyll.Sass (
  module Hakyll.Sass
) where

import Control.Monad.IO.Class (MonadIO)
import GHC.Exception.Type (SomeException)
import Hakyll (Compiler, Item(..), unsafeCompiler)
import Polysemy (embed, embedToFinal, Embed, Final, Members, runFinal, Sem)
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Resource (Resource, resourceToIOFinal)
import Prelude (String, undefined)
import RIO
import RIO.PrettyPrint.Simple (runSimplePrettyApp)
import Path.Posix (toFilePath)
import System.FilePath ((</>))
import System.Process (callProcess)
import qualified Data.Text as T

import Hakyll.Netogallo.Directory (withTmpFolder)

runSassCompiler ::
  forall m r .
  ( MonadIO m,
    Members '[Error SomeException, Resource, Embed m] r
  ) =>
  String ->
  Sem r String
runSassCompiler body =
  withTmpFolder @m $ \tmpFolder -> do
    let
      inputPath = toFilePath tmpFolder </> "input.sass"
      outputPath = toFilePath tmpFolder </> "output.css"
    embed @m $ runSimplePrettyApp 80 mempty $ do
      writeFileUtf8 inputPath (T.pack body)
      liftIO $ callProcess "sass" [inputPath, outputPath]
      T.unpack <$> readFileUtf8 outputPath

sassCompiler ::
  Item String ->
  Compiler (Item String)
sassCompiler item = do
  result <- unsafeCompiler $ runFinal $ errorToIOFinal $ resourceToIOFinal $ embedToFinal compiler
  either (fail . show) (\body -> pure item{ itemBody = body }) result

  where
    body = itemBody item
    compiler :: Sem '[Embed IO, Resource, Error SomeException, Final IO] String
    compiler = runSassCompiler @IO body
