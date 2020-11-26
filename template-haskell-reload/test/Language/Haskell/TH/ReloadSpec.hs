{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.ReloadSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Reload
import Path
import Test.Hspec

thisFileBakedIn :: Load Text
thisFileBakedIn = $$(embedReadTextFile BakeIn [relfile|test/Language/Haskell/TH/ReloadSpec.hs|])

thisFileLive :: Load Text
thisFileLive = $$(embedReadTextFile LoadLive [relfile|test/Language/Haskell/TH/ReloadSpec.hs|])

spec :: Spec
spec = do
  describe "embedReadTextFile" $ do
    describe "Baked in" $ do
      it "is baked in" $ do
        loadMode thisFileBakedIn `shouldBe` BakeIn
      it "contains the module name" $ do
        contents <- loadIO thisFileBakedIn
        T.unpack contents `shouldContain` "Language.Haskell.TH.ReloadSpec"
    describe "Live" $ do
      it "is baked in" $ do
        loadMode thisFileLive `shouldBe` LoadLive
      it "contains the module name" $ do
        contents <- loadIO thisFileLive
        T.unpack contents `shouldContain` "Language.Haskell.TH.ReloadSpec"
