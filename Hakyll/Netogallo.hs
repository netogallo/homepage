module Hakyll.Netogallo (
  module Hakyll.Netogallo
) where

import Control.Lens ((.~), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), FromJSON(..), withObject)
import qualified Data.Aeson as Json
import Data.Aeson.Types (parseEither)
import Data.Hashable (hash)
import qualified Data.Text as Text
import Hakyll (Context, defaultContext, Item, fromFilePath, itemIdentifier, toFilePath)
import qualified Hakyll
import qualified Hakyll.Core.Store as Store
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Web.Pandoc (defaultHakyllReaderOptions, defaultHakyllWriterOptions, readPandocWith, writePandocWith)
import Network.HTTP.Simple (parseRequest)
import Network.HTTP.Download (download)
import Path.Posix ((</>), Abs, Dir, File, parent, parseAbsDir, Path, Rel, dirname)
-- import Polysemy (embed, runM)
-- import Polysemy.Resource (bracket, runResource)
import Prelude (IO)
import Polysemy (Embed, Member, Members, Sem)
import Polysemy.Error (Error, note, throw)
import RIO
import qualified System.FilePath as Path

import Hakyll.Netogallo.Directory (createDirectoryIfMissing, parseAbsDir, parseRelDir, parseRelFile)
import Hakyll.Polysemy (asError, CompilerSem, HakyllError, IsMetadata(..), metadata, MonadMetadata, getMetadataValue, runCompiler, throwError)
import Hakyll.Polysemy.ExternalResourceCache (fromRepository, TextScope(..))
import Hakyll.Repository (Repository(..), repoUrl)
import qualified Hakyll.Zero as Zero

type SiteM = RIO ()
type SiteSem a = Sem '[MonadMetadata, Error SomeException, Embed SiteM] a

data ProjectMetadata =
  ProjectMetadata {
    projectName :: String,
    projectRepository :: Repository
  }

instance FromJSON ProjectMetadata where
  parseJSON = withObject "ProjectMetadata" $ \o -> do
    (itemType :: String) <- o .: "type"
    unless (itemType == "project") $ fail "The 'type' field must be 'project'"
    name <- o .: "name"
    repository <- o .: "repository"
    pure $ ProjectMetadata name repository

data EntryMetadata =
  EntryMetadata {
    entryName :: String,
    entryProjectName :: String,
    entryCommit :: String
  }

instance FromJSON EntryMetadata where
  parseJSON = withObject "EntryMetadata" $ \o -> do
    (itemType :: String) <- o .: "type"
    unless (itemType == "entry") $ fail "The 'type' field must be 'entry'"
    name <- o .: "name"
    projectName <- o .: "project"
    commit <- o .: "commit"
    pure $ EntryMetadata name projectName commit

field ::
  String ->
  (Item a -> CompilerSem String) ->
  Context a
field key f = Hakyll.field key $ runCompiler . f

functionField :: String -> ([String] -> Item a -> CompilerSem String) -> Context a
functionField key f =
  Hakyll.functionField key $
    \args item -> runCompiler $ f args item

projectMetadata ::
  ( IsMetadata m
  , Members '[HakyllError, MonadMetadata] r ) =>
  m ->
  Sem r ProjectMetadata
projectMetadata item = do
  md <- metadata item
  let
    parser v =
      (Zero._1 <$> parseJSON v)
      <|> (Zero._2 <$> parseJSON v)
  case parseEither parser (Json.Object md) of
    Right (Just project,_) -> pure project
    Right (_,Just item) -> entryProjectMetadata item
    Left error -> throwError "projectMetadata" error

  where
    entryProjectMetadata = projectMetadata . fromFilePath . entryProjectName

entryMetadata ::
  ( IsMetadata m
  , Members '[HakyllError, MonadMetadata] r ) =>
  m ->
  Sem r EntryMetadata
entryMetadata item = do
  md <- metadata item
  case parseEither parseJSON (Json.Object md) of
    Right entry -> pure entry
    Left error -> throwError "entryMetadata" error

repoUrlCtx :: Context String
repoUrlCtx = field "repository-url" $ (repoUrl . projectRepository <$>) . projectMetadata

projectCtx :: Context String
projectCtx = codeIncludeField <> repoUrlCtx <> defaultContext

entryCtx :: Context String
entryCtx = codeIncludeField <> repoUrlCtx <> defaultContext

codeIncludeField :: Context String
codeIncludeField = functionField "code-include" compiler
  where 
    readInt = note (asError "codeIncludeField" "Range must be an integer") . readMaybe
    compiler args item =
      case args of
        [path, start, end] -> do
          repo <- projectRepository <$> projectMetadata item
          entryMeta <- entryMetadata item
          iStart <- readInt start
          iEnd <- readInt end
          result <- fromRepository repo path (Just $ entryCommit entryMeta) (WithinLines iStart iEnd)
          note (asError "codeIncludeField" "Resource not found") (Text.unpack <$> result)
        args -> throwError "codeIncludeField" $ "Invalid arguments: " ++ show args

pandocCompilerForCodeInsertion :: Item String -> Compiler (Item String)
pandocCompilerForCodeInsertion content = do
  itemPandoc <- readPandocWith defaultHakyllReaderOptions content
  itemPandoc' <- traverse (return . id) itemPandoc
  return $ writePandocWith defaultHakyllWriterOptions itemPandoc'
