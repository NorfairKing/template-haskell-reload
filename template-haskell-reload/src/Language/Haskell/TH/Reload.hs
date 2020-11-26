{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Reload
  ( -- * Core types
    LoadT (..),
    Load,
    load,
    loadIO,
    Mode (..),
    loadMode,

    -- ** Text files
    embedReadTextFile,
    embedReadTextFileLive,
    embedReadTextFileLiveRun,
    embedReadTextFileBakedIn,
    embedReadTextFileBakedInRun,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path

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

load :: Applicative m => LoadT m a -> m a
load = \case
  Live action -> action
  BakedIn a -> pure a

loadIO :: MonadIO m => Load a -> m a
loadIO = liftIO . load

data Mode = LoadLive | BakeIn
  deriving (Show, Eq, Generic, Lift)

loadMode :: LoadT m a -> Mode
loadMode = \case
  Live _ -> LoadLive
  BakedIn _ -> BakeIn

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
  runIO $ putStrLn $ unwords ["Adding file", fromRelFile fp]
  qAddDependentFile (fromRelFile fp)
  contents <- runIO (SB.readFile (fromRelFile fp))
  let textContents = TE.decodeUtf8 contents
  pure textContents
