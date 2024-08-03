--------------------------------------------------------------------------------
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as Text
import Hakyll
import RIO

import Hakyll.Netogallo
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "projects/*/index.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
        >>= loadAndApplyTemplate "templates/project.html" projectCtx
        >>= saveSnapshot "preview"
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls

    match "projects/*/entries/*.md" $ do
      route $ setExtension "html"
      compile $
        getResourceBody
          >>= applyAsTemplate codeIncludeField
          >>= pandocCompilerForCodeInsertion
          >>= loadAndApplyTemplate "templates/entry.html" entryCtx
          >>= loadAndApplyTemplate "templates/default.html" entryCtx
          >>= relativizeUrls

    create ["projects.html"] $ do
        route idRoute
        compile $ do
          projects <- loadAllSnapshots "projects/*/index.md" "preview"
          let
            projectsCtx =
                listField "projects" projectSummaryCtx (return projects)
                <> Hakyll.field "page-title" (const $ pure "projects.html")
                <> defaultContext
          makeItem "" 
            >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
            >>= loadAndApplyTemplate "templates/default.html" projectsCtx
            >>= relativizeUrls

    {-
    match "index.html" $ do
        route idRoute
        compile $ do
            projects <- loadAll "projects/*/index.md"
            let indexCtx =
                    listField "projects" defaultContext (return projects) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    -}
    match "templates/*" $ compile templateBodyCompiler


