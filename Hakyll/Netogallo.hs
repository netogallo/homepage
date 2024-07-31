module Hakyll.Netogallo (
  entryCtx,
  projectCtx
) where

import Control.Monad.IO.Class (liftIO)
import Data.Hashable (hash)
import Hakyll (Context, defaultContext, Item, fromFilePath, itemIdentifier, toFilePath)
import qualified Hakyll
import qualified Hakyll.Core.Store as Store
import Network.HTTP.Simple (parseRequest)
import Network.HTTP.Download (download)
import Path.Posix ((</>), Abs, Dir, File, parent, parseAbsDir, Path, Rel, dirname)
-- import Polysemy (embed, runM)
-- import Polysemy.Resource (bracket, runResource)
import Prelude (IO)
import Polysemy (Embed, Member, Members, Sem)
import Polysemy.Error (Error, throw)
import RIO
import qualified System.FilePath as Path

import Hakyll.Netogallo.Directory (createDirectoryIfMissing, parseAbsDir, parseRelDir, parseRelFile)
import Hakyll.Polysemy (CompilerSem, MonadMetadata, getMetadataValue, runCompiler)

type SiteM = RIO ()
type SiteSem a = Sem '[MonadMetadata, Error SomeException, Embed SiteM] a

lookupProjectMetadata ::
  Members '[MonadMetadata, Error SomeException] r =>
  String ->
  Item a ->
  Sem r String
lookupProjectMetadata key item = getMetadataValue projectIdentifier key
  where
    projectDirectory = Path.takeDirectory . Path.takeDirectory . toFilePath $ itemIdentifier item
    projectIdentifier = fromFilePath $ projectDirectory Path.</> "index.md"

field ::
  String ->
  (Item a -> CompilerSem String) ->
  Context a
field key f = Hakyll.field key $ runCompiler . f

repoCtx :: Context String
repoCtx = field "repository" $ lookupProjectMetadata "repository"

projectCtx :: Context String
projectCtx = defaultContext

entryCtx :: Context String
entryCtx = repoCtx <> defaultContext

downloadFromGithub ::
  (Members '[Embed SiteM] r) =>
  Path Abs Dir ->
  String ->
  String ->
  String ->
  String ->
  Sem r (Path Abs File)
downloadFromGithub outFolder user repo revision path = do
  request <- parseRequest url
  destination <- (outFolder </>) <$> parseRelFile suffix
  createDirectoryIfMissing True $ parent destination
  result <- download request (toFilePath destination)
  pure destination
  where
    suffix = user <> "/" <> repo <> "/" <> revision <> "/" <> path
    -- destination = normalise $ outFolder </> suffix 
    url = "https://raw.githubusercontent.com/" ++ suffix

codeIncludeField :: Context String
codeIncludeField = functionField "code-include" compiler
  where 
    compiler = error "todo" 
