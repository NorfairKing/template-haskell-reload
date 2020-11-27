{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Load
  ( -- * Core types
    LoadT (..),
    Load,
    load,
    loadIO,
    Mode (..),
    loadMode,

    -- ** Text files
    embedReadTextFile,

    -- *** Auxiliary functions
    embedReadTextFileLive,
    embedReadTextFileLiveRun,
    embedReadTextFileBakedIn,
    embedReadTextFileBakedInRun,

    -- ** List directory
    embedListDir,

    -- *** Auxiliary functions
    embedListDirLive,
    embedListDirLiveRun,
    embedListDirBakedIn,
    embedListDirBakedInRun,

    -- ** Files in directory
    embedTextFilesIn,
    embedTextFilesInWith,
  )
where

import Conduit
import Control.Monad
import qualified Data.ByteString as SB
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO

type Load a = LoadT IO a

data LoadT m a = Live (m a) | BakedIn a
  deriving (Generic, Lift)

instance Functor f => Functor (LoadT f) where
  fmap f = \case
    Live ioa -> Live $ f <$> ioa
    BakedIn a -> BakedIn $ f a

instance Applicative f => Applicative (LoadT f) where
  pure = BakedIn
  BakedIn f <*> BakedIn a = BakedIn $ f a
  BakedIn f <*> Live ma = Live $ f <$> ma
  Live mf <*> BakedIn a = Live $ ($ a) <$> mf
  Live mf <*> Live ma = Live $ mf <*> ma

-- | This instance turns the second _BakedIn_ into a _Live_, so watch out.
instance Monad m => Monad (LoadT m) where
  ra >>= f = case ra of
    BakedIn a -> f a
    Live ma -> Live $ do
      a <- ma
      case f a of
        Live b -> b
        BakedIn b -> pure b

instance MonadIO m => MonadIO (LoadT m) where
  liftIO = Live . liftIO

-- | Load a 'LoadT'. This runs a 'Live' action and uses 'pure' on a 'BakedIn' value.
load :: Applicative m => LoadT m a -> m a
load = \case
  Live action -> action
  BakedIn a -> pure a

-- | Load a 'LoadT IO' in any 'MonadIO'.
--
-- If you just need the result in 'IO', you can use 'load' instead.
loadIO :: MonadIO m => Load a -> m a
loadIO = liftIO . load

data Mode = LoadLive | BakeIn
  deriving (Show, Eq, Generic, Lift)

-- | Check whether a value is baked-in or not.
loadMode :: LoadT m a -> Mode
loadMode = \case
  Live _ -> LoadLive
  BakedIn _ -> BakeIn

-- | Embed a text file
--
-- This will throw on a non-utf8 files
embedReadTextFile :: Mode -> Path Rel File -> Q (TExp (Load Text))
embedReadTextFile = \case
  LoadLive -> embedReadTextFileLive
  BakeIn -> embedReadTextFileBakedIn

embedReadTextFileLive :: Path Rel File -> Q (TExp (Load Text))
embedReadTextFileLive fp = [||Live $ embedReadTextFileLiveRun fp||]

embedReadTextFileLiveRun :: Path Rel File -> IO Text
embedReadTextFileLiveRun fp = TE.decodeUtf8 <$> (SB.readFile (fromRelFile fp))

embedReadTextFileBakedIn :: Path Rel File -> Q (TExp (Load Text))
embedReadTextFileBakedIn fp = do
  cts <- embedReadTextFileBakedInRun fp
  [||BakedIn cts||]

embedReadTextFileBakedInRun :: Path Rel File -> Q Text
embedReadTextFileBakedInRun fp = do
  runIO $ putStrLn $ unwords ["Baking-in file:", fromRelFile fp]
  qAddDependentFile (fromRelFile fp)
  contents <- runIO (SB.readFile (fromRelFile fp))
  let textContents = TE.decodeUtf8 contents
  pure textContents

-- | Embed a directory, ignore hidden files and directories
embedListDir :: Mode -> Path Rel Dir -> Q (TExp (Load [Path Rel File]))
embedListDir = \case
  LoadLive -> embedListDirLive
  BakeIn -> embedListDirBakedIn

embedListDirBakedIn :: Path Rel Dir -> Q (TExp (Load [Path Rel File]))
embedListDirBakedIn rd = do
  runIO $ putStrLn $ unwords ["Baking-in directory:", fromRelDir rd]
  cts <- embedListDirBakedInRun rd
  [||BakedIn cts||]

embedListDirBakedInRun :: Path Rel Dir -> Q [Path Rel File]
embedListDirBakedInRun rd = runIO $ Conduit.sourceToList $ sourceFilesInNonHiddenDirsRecursively rd

embedListDirLive :: Path Rel Dir -> Q (TExp (Load [Path Rel File]))
embedListDirLive rd = [||Live $ embedListDirLiveRun rd||]

embedListDirLiveRun :: Path Rel Dir -> IO [Path Rel File]
embedListDirLiveRun rd = Conduit.sourceToList $ sourceFilesInNonHiddenDirsRecursively rd

sourceFilesInNonHiddenDirsRecursively ::
  forall m i.
  MonadIO m =>
  Path Rel Dir ->
  ConduitT i (Path Rel File) m ()
sourceFilesInNonHiddenDirsRecursively rd = do
  here <- getCurrentDir
  walkDirRel go $ here </> rd
  where
    go ::
      Path Rel Dir ->
      [Path Rel Dir] ->
      [Path Rel File] ->
      ConduitT i (Path Rel File) m (WalkAction Rel)
    go curdir subdirs files = do
      Conduit.yieldMany $
        map (curdir </>) $
          filter (not . hidden) files
      pure $ WalkExclude $ filter (isHiddenIn curdir) subdirs

embedTextFilesIn ::
  Mode ->
  Path Rel Dir ->
  Q (TExp (Load (Map (Path Rel File) Text)))
embedTextFilesIn = embedTextFilesInWith id [||id||] (flip const) [||flip const||]

embedTextFilesInWith ::
  (Ord a, Lift a, Lift b) =>
  -- | A function to change the key
  (Path Rel File -> a) ->
  -- | An expression for that same function to change the key
  Q (TExp (Path Rel File -> a)) ->
  -- | An expression to change the value
  (a -> Text -> b) ->
  -- | An expression for that same function to change the value
  Q (TExp (a -> Text -> b)) ->
  Mode ->
  -- | The directory to load
  Path Rel Dir ->
  Q (TExp (Load (Map a b)))
embedTextFilesInWith keyFunc qKeyFunc valFunc qValFunc heated rd = case heated of
  LoadLive ->
    [||
    Live $ do
      files <- embedListDirLiveRun rd
      tups <- forM files $ \file -> do
        let key = $$(qKeyFunc) file
        val <- $$(qValFunc) key <$> embedReadTextFileLiveRun (rd </> file)
        pure (key, val)
      pure $ M.fromList tups
    ||]
  BakeIn -> do
    files <- embedListDirBakedInRun rd
    tups <- forM files $ \file -> do
      let key = keyFunc file
      val <- valFunc key <$> embedReadTextFileBakedInRun (rd </> file)
      pure (key, val)
    [||BakedIn $ M.fromList tups||]

hidden :: Path Rel File -> Bool
hidden = goFile
  where
    goFile :: Path Rel File -> Bool
    goFile f = isHiddenIn (parent f) f || goDir (parent f)
    goDir :: Path Rel Dir -> Bool
    goDir f
      | parent f == f = False
      | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp
