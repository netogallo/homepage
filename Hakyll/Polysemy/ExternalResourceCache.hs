{-# LANGUAGE TemplateHaskell #-}
module Hakyll.Polysemy.ExternalResourceCache (
) where

import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (MonadIO)
import Data.Hashable (Hashable(..))
import Data.Kind (Type)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Hakyll.Core.Compiler (cached, Compiler, unsafeCompiler)
import Hakyll.Core.Identifier (fromFilePath, Identifier)
import GHC.Exception.Type (SomeException)
import Network.HTTP.Simple (parseRequest)
import Network.HTTP.Download (download)
import Path.Posix (parseRelFile)
import qualified Path.Posix as P
import Polysemy (embed, Embed, interpret, interpretH, makeSem, Member, runM, runTSimple, Sem, Tactical)
import qualified Polysemy.Error as PE
import Polysemy.Final (Final, embedToFinal, runFinal)
import qualified Polysemy.Resource as PR
import Prelude 
import RIO.PrettyPrint.Simple (runSimplePrettyApp)
import System.FilePath ((</>))
import Type.Errors (ErrorMessage(..), TypeError)

import Hakyll.Netogallo.Directory (withTmpFolder)

data Resource =
  GitHub {
    githubOwner :: String,
    githubRepo :: String,
    githubPath :: String,
    githubRef :: String
  } deriving (Eq)

asFileName :: Resource -> String
asFileName res = "hakyll-" <> name
  where
    name = show . abs . hash $ res

asIdentifier :: Resource -> Identifier
asIdentifier = fromFilePath . ("hakyll-" <>) . asFileName 

loopMProj :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
loopMProj proj body initial
  | proj initial = loopM body' initial
  | otherwise = pure initial
  where
    body' v = do
      result <- body v
      pure $
        if proj result
        then Left result
        else Right result

instance Hashable Resource where
  hashWithSalt salt (GitHub owner repo path ref) =
    salt
      `hashWithSalt` owner
      `hashWithSalt` repo
      `hashWithSalt` path
      `hashWithSalt` ref

getResourceUrl :: Resource -> String
getResourceUrl (GitHub owner repo path ref) =
  "https://raw.githubusercontent.com/" <> owner <> "/" <> repo <> "/" <> ref <> "/" <> path

data TextScope =
  WithinLines {
    lineStart :: Int,
    lineEnd :: Int
  }

data TextValue =
  TextValue {
    resourceText :: Maybe Text,
    resourceId :: Resource,
    resourceScope :: TextScope
  }

data WithTextResource m v where
  GetTextSlice :: TextScope -> WithTextResource m (Maybe Text)

$(makeSem ''WithTextResource)

type family TextResourceHandler (a :: Type -> Type) where
  TextResourceHandler (Sem r) = Sem (WithTextResource ': r)
  TextResourceHandler m = TypeError ('Text "Action must be the underlying 'Sem r' monad with the 'WithTextResource' effect.")

data ExternalResourceCache m v where
  UseTextResource :: Resource -> TextResourceHandler m v -> ExternalResourceCache m v

$(makeSem ''ExternalResourceCache)

scopeText ::
  TextScope ->
  Text ->
  Maybe Text
scopeText (WithinLines start end) text
  | List.null begin = Nothing
  | otherwise = Just . Text.unlines . take (end - start) $ begin
  where
    begin = drop start $ Text.lines text

createResource ::
  Resource ->
  Compiler Text
createResource res = do
  result <- unsafeCompiler 
    . runFinal
    . PR.resourceToIOFinal
    . PE.errorToIOFinal
    $ action' 
  either (fail . show) pure result
  where
    url = getResourceUrl res
    action' :: Sem '[PE.Error SomeException, PR.Resource, Final IO] Text
    action' = embedToFinal action
    action :: Sem '[Embed IO, PE.Error SomeException, PR.Resource, Final IO] Text
    action = withTmpFolder @IO $ \tmp -> runSimplePrettyApp 80 mempty $ do
      req <- parseRequest url
      path <- parseRelFile $ asFileName res
      result <- download req (tmp P.</> path)
      pure undefined

runTextResourceHandler ::
  Text ->
  TextResourceHandler (Sem r) v ->
  Sem r v
runTextResourceHandler text = interpret $ \case
  GetTextSlice scope -> do
    pure $ scopeText scope text

interpreterExternalResourceCache ::
  forall r0 r v .
  Member (Embed Compiler) r =>
  ExternalResourceCache (Sem r0) v ->
  Tactical ExternalResourceCache (Sem r0) r v
interpreterExternalResourceCache (UseTextResource res acc) = do
  content <- embed $ cached (asFileName res) (createResource res)
  runTSimple $ runTextResourceHandler content acc

runExternalResourceCache ::
  forall r v .
  Member (Embed Compiler) r =>
  Sem (ExternalResourceCache ': r) v ->
  Sem r v
runExternalResourceCache = interpretH interpreterExternalResourceCache

