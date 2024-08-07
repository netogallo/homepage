{-# LANGUAGE AllowAmbiguousTypes #-}
module Hakyll.Netogallo.Directory (
  module Hakyll.Netogallo.Directory
) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Either as E
import Data.Hashable (hash)
import Data.Kind (Type)
import GHC.Exception.Type (SomeException)
import Polysemy (Embed, embed, Member, Members, Sem)
import qualified Polysemy.Error as PE
import Polysemy.Resource (bracket, Resource)
import RIO hiding (bracket)
import qualified RIO.Directory as RD
import Path.Posix ((</>), Path, Abs, Dir, File, Rel, toFilePath)
import qualified Path.Posix as P

parseAbsDir ::
  Members '[PE.Error SomeException] r =>
  FilePath ->
  Sem r (Path Abs Dir)
parseAbsDir = PE.fromEither . P.parseAbsDir

parseRelDir ::
  Members '[PE.Error SomeException] r =>
  FilePath ->
  Sem r (Path Rel Dir)
parseRelDir = PE.fromEither . P.parseRelDir

parseRelFile ::
  Members '[PE.Error SomeException] r =>
  FilePath ->
  Sem r (Path Rel File)
parseRelFile = PE.fromEither . P.parseRelFile

getDirectoryContents ::
  forall m r .
  ( MonadIO m
  , Members '[Embed m, PE.Error SomeException] r
  ) =>
  Path Abs Dir ->
  Sem r [Path Abs Dir]
getDirectoryContents path = do
  contents <- embed (RD.getDirectoryContents (toFilePath path) :: m [FilePath])
  mapM (fmap (path </>) . parseRelDir) [d | d <- contents, d `notElem` [".", ".."]]

createDirectoryIfMissing ::
  forall m r .
  (MonadIO m, Members '[Embed m] r) =>
  Bool ->
  Path Abs Dir ->
  Sem r ()
createDirectoryIfMissing createParents path =
  embed (RD.createDirectoryIfMissing createParents (toFilePath path) :: m ())

removeDirectoryRecursive ::
  forall m r .
  (MonadIO m, Members '[Embed m] r) =>
  Path Abs Dir ->
  Sem r ()
removeDirectoryRecursive path =
  embed (RD.removeDirectoryRecursive (toFilePath path) :: m ())

getTemporaryDirectory ::
  forall m r .
  (MonadIO m, Members '[Embed m, PE.Error SomeException] r) =>
  Sem r (Path Abs Dir)
getTemporaryDirectory = embed @m RD.getTemporaryDirectory >>= parseAbsDir

withTmpFolder ::
  forall m r a .
  (MonadIO m, Members '[Embed m, PE.Error SomeException, Resource] r) =>
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withTmpFolder action = do
  tmpFolder <- (getTemporaryDirectory @m)
  suffix <- ("hakyll-" <>) . show . abs . hash <$> (getDirectoryContents @m) tmpFolder
  relDir <- parseRelDir suffix
  let
    tmpFolder' = tmpFolder </> relDir
    create = createDirectoryIfMissing @m True tmpFolder' >> pure tmpFolder'
    destroy = removeDirectoryRecursive @m
  bracket create destroy action


