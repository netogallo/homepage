module Hakyll.Repository (
  module Hakyll.Repository
) where

import Data.Aeson ((.:), FromJSON(..), withObject)
import RIO

data Repository =
  GitHub {
    owner :: String,
    repo :: String
  }

instance FromJSON Repository where
  parseJSON = withObject "Repository" $ \o -> do
    gitHub <- o .: "github"
    GitHub <$> gitHub .: "owner" <*> gitHub .: "repo"

repoUrl :: Repository -> String
repoUrl (GitHub owner repo) = "https://github.com/" <> owner <> "/" <> repo
