{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Hspec
import Text.IndentedString.QQ

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "indented" $ do
  it "removes indentation" $ do
    [indented|
      foo
       bar
      baz
    |] `shouldBe` unlines
      [ "foo"
      , " bar"
      , "baz"
      ]

  it "preserves empty lines in the middle" $ do
    [indented|
      foo

      bar
    |] `shouldBe` unlines
      [ "foo"
      , ""
      , "bar"
      ]

  it "preserves empty lines at the beginning and end" $ do
    [indented|

      foo
      bar

    |] `shouldBe` unlines
      [ ""
      , "foo"
      , "bar"
      , ""
      ]

  it "works when first line is indented more than others" $ do
    [indented|
        foo
      bar
    |] `shouldBe` unlines
      [ "  foo"
      , "bar"
      ]
