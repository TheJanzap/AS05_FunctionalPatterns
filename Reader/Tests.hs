{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import Test.Hspec ( hspec, describe, it, shouldBe )
import Reader.Example

name :: String
name = "first.last"

address :: String
address = "abc@def.gh"

production, development :: Config
production = Config { devStage = False }
development = Config { devStage = True }

main :: IO ()
main = hspec $ do
  describe "Reader versions of createBody', createAddr' and createMail'" $ do

    it "createBody == createBody'" $ do
      runWithConfig (createBody address) development `shouldBe` createBody' address development
      runWithConfig (createBody address) production `shouldBe` createBody' address production

    it "createAddr == createAddr'" $ do
      runWithConfig (createAddr name) development `shouldBe` createAddr' name development
      runWithConfig (createAddr name) production `shouldBe` createAddr' name production

    it "createMail == createMail'" $ do
      runWithConfig (createMail name) development `shouldBe` createMail'  name development
      runWithConfig (createMail name) production `shouldBe` createMail' name production

  describe "Optional Env Reader versions" $ do

    it "createBody == createBody''" $ do
      runInEnv (createBody'' address) development `shouldBe` createBody' address development
      runInEnv (createBody'' address) production `shouldBe` createBody' address production

    it "createAddr == createAddr''" $ do
      runInEnv (createAddr'' name) development `shouldBe` createAddr' name development
      runInEnv (createAddr'' name) production `shouldBe` createAddr' name production

    it "createMail == createMail''" $ do
      runInEnv (createMail'' name) development `shouldBe` createMail'  name development
      runInEnv (createMail'' name) production `shouldBe` createMail' name production