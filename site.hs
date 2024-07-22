--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Hakyll

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
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls
      

--    match (fromList ["about.rst", "contact.markdown"]) $ do
--        route   $ setExtension "html"
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/default.html" defaultContext
--            >>= relativizeUrls

    match "projects/*/entries/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/entry.html" entryCtx
            >>= loadAndApplyTemplate "templates/default.html" entryCtx
            >>= relativizeUrls

--    create ["archive.html"] $ do
--        route idRoute
--        compile $ do
--            posts <- recentFirst =<< loadAll "posts/*"
--            let archiveCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Archives"            `mappend`
--                    defaultContext
--
--            makeItem ""
--                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                >>= relativizeUrls

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

    match "templates/*" $ compile templateBodyCompiler


