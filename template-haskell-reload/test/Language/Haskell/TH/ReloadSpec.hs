{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.ReloadSpec (spec) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Reload
import Path
import Test.Hspec

thisFileBakedIn :: Load Text
thisFileBakedIn = $$(embedReadTextFile BakeIn [relfile|test/Language/Haskell/TH/ReloadSpec.hs|])

thisFileLive :: Load Text
thisFileLive = $$(embedReadTextFile LoadLive [relfile|test/Language/Haskell/TH/ReloadSpec.hs|])

testDirBakedIn :: Load [Path Rel File]
testDirBakedIn = $$(embedListDir BakeIn [reldir|test|])

testDirLive :: Load [Path Rel File]
testDirLive = $$(embedListDir LoadLive [reldir|test|])

testFilesBakedIn :: Load (Map (Path Rel File) Text)
testFilesBakedIn = $$(embedTextFilesIn BakeIn [reldir|test|])

testFilesLive :: Load (Map (Path Rel File) Text)
testFilesLive = $$(embedTextFilesIn LoadLive [reldir|test|])

spec :: Spec
spec = do
  describe "embedReadTextFile" $ do
    describe "Baked in" $ do
      it "is baked in" $ do
        loadMode thisFileBakedIn `shouldBe` BakeIn
      it "contains the module name" $ do
        contents <- load thisFileBakedIn
        T.unpack contents `shouldContain` "Language.Haskell.TH.ReloadSpec"
    describe "Live" $ do
      it "is baked in" $ do
        loadMode thisFileLive `shouldBe` LoadLive
      it "contains the module name" $ do
        contents <- load thisFileLive
        T.unpack contents `shouldContain` "Language.Haskell.TH.ReloadSpec"
  describe "embedListDir" $ do
    describe "Baked in" $ do
      it "is baked in" $ do
        loadMode testDirBakedIn `shouldBe` BakeIn
      it "contains this module" $ do
        files <- load testDirBakedIn
        files `shouldContain` [[relfile|Language/Haskell/TH/ReloadSpec.hs|]]
    describe "Live" $ do
      it "is live" $ do
        loadMode testDirLive `shouldBe` LoadLive
      it "contains this module" $ do
        files <- load testDirLive
        files `shouldContain` [[relfile|Language/Haskell/TH/ReloadSpec.hs|]]
  describe "embedTextFilesIn" $ do
    describe "Baked in" $ do
      it "is baked in" $ do
        loadMode testFilesBakedIn `shouldBe` BakeIn
      it "contains this module" $ do
        files <- load testFilesBakedIn
        files `shouldSatisfy` (M.member [relfile|Language/Haskell/TH/ReloadSpec.hs|])
    describe "Live" $ do
      it "is live" $ do
        loadMode testFilesLive `shouldBe` LoadLive
      it "contains this module" $ do
        files <- load testFilesLive
        files `shouldSatisfy` (M.member [relfile|Language/Haskell/TH/ReloadSpec.hs|])
