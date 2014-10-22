{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Data.Aeson.TransformSpec (main, spec) where

import Test.Hspec
import Data.Aeson.Transform
import Data.HashMap.Strict (fromList)

import Data.Aeson.QQ

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "transform" $ do
    context "Id" $ do
      it "leaves objects unchanged" $ do
        let obj = [aesonQQ| { foo: 1, bar: 2 }|]
        transform Id obj `shouldBe` obj
      it "leaves arrays unchanged" $ do
        let arr = [aesonQQ| [1,2] |]
        transform Id arr `shouldBe` arr
    context "At" $
      it "moves builder deeper" $ do
        let obj = [aesonQQ| { foo: { bar: { baz: 1 }}} |]
        transform (At "foo" $ At "bar" Id) obj `shouldBe` [aesonQQ| { baz: 1 } |]
    context "Index" $
      it "moves builder into array" $ do
        let arr = [aesonQQ| [{a:1}, {a:2}, {a:7}] |]
        transform (Index 1 Id) arr `shouldBe` [aesonQQ| {a:2} |]
    context "Attr" $
      it "extracts a value" $ do
        let obj = [aesonQQ| {a:1, b:2} |]
        transform (Attr "a") obj `shouldBe` [aesonQQ| 1 |]
    context "Attrs" $ do
      it "filters objects by keys" $ do
        let obj = [aesonQQ| {a:1, b:2, c:3} |]
        transform (Attrs ["a", "b"]) obj `shouldBe` [aesonQQ| {a:1, b:2} |]
      it "ignores undiscovered keys" $ do
        let obj = [aesonQQ| {a:1, b:2, c:3} |]
        transform (Attrs ["a", "b", "z"]) obj `shouldBe` [aesonQQ| {a:1, b:2} |]
    context "Map" $
      it "replaces an array" $ do
        let arr = [aesonQQ| [{a:1}, {a:2}, {a:7}] |]
        transform (Map $ Attr "a") arr `shouldBe` [aesonQQ| [1,2,7] |]
    context "Obj" $
      it "builds an object with specified keys" $ do
        let obj = [aesonQQ| {a:1} |]
            result = transform
              (Obj $ fromList [("foo", Attr "a") , ("bar", Attr "a")])
              obj
        result `shouldBe` [aesonQQ| {foo: 1, bar: 1} |]
