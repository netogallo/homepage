module Hakyll.Netogallo (
  entryCtx,
  projectCtx
) where

import Hakyll
import Hakyll.Core.Store 
import System.FilePath ((</>), takeDirectory)

lookupProjectMetadata ::
  (MonadMetadata m, MonadFail m) =>
  String ->
  Item a ->
  m String
lookupProjectMetadata key item = do 
  mValue <- lookupString key <$> getMetadata projectIdentifier
  maybe (fail $ "Project does not exist: " ++ show projectIdentifier) pure mValue
  where
    projectDirectory = takeDirectory . takeDirectory . toFilePath $ itemIdentifier item
    projectIdentifier = fromFilePath $ projectDirectory </> "index.md"

repoCtx :: Context String
repoCtx = field "repository" $ lookupProjectMetadata "repository"

projectCtx :: Context String
projectCtx = defaultContext

entryCtx :: Context String
entryCtx = repoCtx <> defaultContext



codeIncludeField :: Context String
codeIncludeField = functionField "code-include" compiler
  where
    compiler = error "todo" 
