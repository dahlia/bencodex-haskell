{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bencodex.WriterSpec (spec) where

import Control.Monad
import Data.String

import Data.ByteString.Lazy
import Test.Hspec

import Data.Bencodex.SpecDiscovery
import Data.Bencodex.Types
import Data.Bencodex.Writer

spec :: Spec
spec = do
    let Right specs = listSpecs

    specify "specs" $
        specs `shouldNotSatisfy` Prelude.null

    describe "encodeLazy" $ do
        forM_ specs $ \ Spec' {..} ->
            specify specPath $
                encodeLazy expectedValue `shouldBe` fromStrict expectedEncoding 

        testEncoder encodeLazy

    describe "encodeStrict" $ do
        forM_ specs $ \ Spec' {..} ->
            specify specPath $
                encodeStrict expectedValue `shouldBe` expectedEncoding 

        testEncoder encodeStrict

testEncoder :: (Show a, Eq a, IsString a) => (BValue -> a) -> SpecWith ()
testEncoder e = do
    specify "BNull" $
        e BNull `shouldBe` "n"

    specify "BBool" $ do
        e (BBool True) `shouldBe` "t"
        e (BBool False) `shouldBe` "f"

    specify "BInteger" $ do
        e (BInteger 0) `shouldBe` "i0e"
        e (BInteger 123) `shouldBe` "i123e"
        e (BInteger (-456)) `shouldBe` "i-456e"

    specify "BByteString" $ do
        e (BByteString "") `shouldBe` "0:"
        e (BByteString "hello") `shouldBe` "5:hello"

    specify "BByteString" $ do
        e (BText "") `shouldBe` "u0:"
        e (BText "hello") `shouldBe` "u5:hello"
        e (BText "\xc720\xb2c8\xcf54\xb4dc") `shouldBe`
            "u12:\xec\x9c\xa0\xeb\x8b\x88\xec\xbd\x94\xeb\x93\x9c"

    specify "BList" $ do
        e (BList []) `shouldBe` "le"
        let elems =
                [ BNull, BBool True, BBool False, BInteger 123
                , BByteString "hello", BText "world"
                , BList [], BList [BNull]
                , BMap [(BByteStringKey "k", BByteString "v")]
                ]
        e (BList elems) `shouldBe` "lntfi123e5:hellou5:worldlelned1:k1:vee"

    specify "BMap" $ do
        e (BMap []) `shouldBe` "de"
        let pairs = BMap
                [ (BTextKey "a", BNull)
                , (BByteStringKey "b", BBool True)
                , (BTextKey "c", BBool False)
                , (BByteStringKey "d", BInteger 123)
                , (BTextKey "e", BByteString "hello")
                , (BByteStringKey "f", BText "world")
                , (BTextKey "g", BList [BNull])
                , (BByteStringKey "h", BMap [])
                ]
        e pairs `shouldBe`
            "d1:bt1:di123e1:fu5:world1:hdeu1:anu1:cfu1:e5:hellou1:glnee"
