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
    context "at" $
      it "moves builder deeper" $ do
        let o = [aesonQQ| { foo: { bar: { baz: 1 }}} |]
        (at "foo" $ at "bar" id) o `shouldBe` [aesonQQ| { baz: 1 } |]
    context "atIndex" $
      it "moves builder deeper" $ do
        let o = [aesonQQ| [ [ 1, 2 ] ] |]
        (atIndex 0 $ atIndex 1 id) o `shouldBe` [aesonQQ| 2 |]
    context "index" $
      it "moves builder into array" $ do
        let arr = [aesonQQ| [{a:1}, {a:2}, {a:7}] |]
        index 1 arr `shouldBe` [aesonQQ| {a:2} |]
    context "attr" $
      it "extracts a value" $ do
        let o = [aesonQQ| {a:1, b:2} |]
        attr "a" o `shouldBe` [aesonQQ| 1 |]
    context "keep" $ do
      it "filters objects by keys" $ do
        let o = [aesonQQ| {a:1, b:2, c:3} |]
        keep ["a", "b"] o `shouldBe` [aesonQQ| {a:1, b:2} |]
      it "ignores undiscovered keys" $ do
        let o = [aesonQQ| {a:1, b:2, c:3} |]
        keep ["a", "b", "z"] o `shouldBe` [aesonQQ| {a:1, b:2} |]
    context "map" $
      it "replaces an array" $ do
        let arr = [aesonQQ| [{a:1}, {a:2}, {a:7}] |]
        Data.Aeson.Transform.map (attr "a") arr `shouldBe`
          [aesonQQ| [1,2,7] |]
    context "obj" $
      it "builds an object with specified keys" $ do
        let o = [aesonQQ| {a:1} |]
            result = obj (fromList [("foo", attr "a") , ("bar", attr "a")]) o
        result `shouldBe` [aesonQQ| {foo: 1, bar: 1} |]
    context "merge" $
      it "combines objects with previous value at a key winning against later value" $ do
         let o = [aesonQQ| { foo: {a:1, b:2}, bar: {b:3, c:4} } |]
         merge (attr "foo") (attr "bar") o `shouldBe`
          [aesonQQ| { a:1, b:2, c:4 } |]
