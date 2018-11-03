{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bencodex.ReaderSpec (spec) where

import Control.Monad

import qualified Data.ByteString
import Data.ByteString.Lazy
import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Bencodex.Reader
import Data.Bencodex.SpecDiscovery
import Data.Bencodex.Types

type BS = Data.ByteString.ByteString

spec :: Spec
spec = do
    let Right specs = listSpecs

    specify "specs" $
        specs `shouldNotSatisfy` Prelude.null

    describe "decodeStrict" $
        forM_ specs $ \ Spec' {..} ->
            specify specPath $
                decodeStrict expectedEncoding `shouldBe` Right expectedValue

    describe "decodeLazy" $
        forM_ specs $ \ Spec' {..} ->
            specify specPath $ do
                let Done remain result = decodeLazy $
                        fromStrict expectedEncoding
                remain `shouldSatisfy` Data.ByteString.Lazy.null
                result `shouldBe` expectedValue

    describe "bValue" $ do
        forM_ specs $ \ Spec' {..} ->
            specify specPath $
                expectedEncoding ~> bValue `shouldParse` expectedValue

        specify "et cetra" $ do
            ("n" :: BS) ~> bValue `shouldParse` BNull
            ("i0e" :: BS) ~> bValue `shouldParse` BInteger 0
            ("i1234e" :: BS) ~> bValue `shouldParse` BInteger 1234
            ("i-5678e" :: BS) ~> bValue `shouldParse` BInteger (-5678)
            bValue `shouldFailOn` ("i12.34e" :: BS)
            ("0:" :: BS) ~> bValue `shouldParse` BByteString ""
            ("5:hello" :: BS) ~> bValue `shouldParse` BByteString "hello"
            bValue `shouldFailOn` ("6:hello" :: BS)
            ("u0:" :: BS) ~> bValue `shouldParse` BText ""
            ("u5:hello" :: BS) ~> bValue `shouldParse` BText "hello"
            bValue `shouldFailOn` ("u6:hello" :: BS)
            ("le:helloe" :: BS) ~> bValue `shouldParse` BList []
            ("lntfi12eu5:helloe" :: BS) ~> bValue `shouldParse` BList
                [BNull, BBool True, BBool False, BInteger 12, BText "hello"]
            bValue `shouldFailOn` ("ltf" :: BS)
            ("de" :: BS) ~> bValue `shouldParse` BMap []
            ("d1:k1:ve" :: BS) ~> bValue `shouldParse`
                BMap [(BByteStringKey "k", BByteString "v")]
            ("du1:ku1:ve" :: BS) ~> bValue `shouldParse`
                BMap [(BTextKey "k", BText "v")]
            bValue `shouldFailOn` ("dnne" :: BS)
            bValue `shouldFailOn` ("dtne" :: BS)
            bValue `shouldFailOn` ("dfne" :: BS)
            bValue `shouldFailOn` ("di123ene" :: BS)
            bValue `shouldFailOn` ("dlene" :: BS)
            bValue `shouldFailOn` ("ddene" :: BS)

    specify "null'" $ do
        null' `shouldSucceedOn` ("n" :: BS)
        null' `shouldFailOn` ("t" :: BS)

    specify "true" $ do
        true `shouldSucceedOn` ("t" :: BS)
        true `shouldFailOn` ("f" :: BS)

    specify "false" $ do
        false `shouldSucceedOn` ("f" :: BS)
        false `shouldFailOn` ("t" :: BS)

    specify "integer" $ do
        ("i0e" :: BS) ~> integer `shouldParse` 0
        ("i1234e" :: BS) ~> integer `shouldParse` 1234
        ("i-5678e" :: BS) ~> integer `shouldParse` (-5678)
        false `shouldFailOn` ("i12.34e" :: BS)

    specify "byteString" $ do
        ("0:" :: BS) ~> byteString `shouldParse` ""
        ("5:hello" :: BS) ~> byteString `shouldParse` "hello"
        byteString `shouldFailOn` ("6:hello" :: BS)

    specify "text" $ do
        ("u0:" :: BS) ~> text `shouldParse` ""
        ("u5:hello" :: BS) ~> text `shouldParse` "hello"
        text `shouldFailOn` ("u6:hello" :: BS)

    specify "list" $ do
        ("le:helloe" :: BS) ~> list `shouldParse` []
        ("lntfi12eu5:helloe" :: BS) ~> list `shouldParse`
            [BNull, BBool True, BBool False, BInteger 12, BText "hello"]
        list `shouldFailOn` ("ltf" :: BS)

    specify "map'" $ do
        ("de" :: BS) ~> map' `shouldParse` []
        ("d1:k1:ve" :: BS) ~> map' `shouldParse`
            [(BByteStringKey "k", BByteString "v")]
        ("du1:ku1:ve" :: BS) ~> map' `shouldParse` [(BTextKey "k", BText "v")]
        map' `shouldFailOn` ("dnne" :: BS)
        map' `shouldFailOn` ("dtne" :: BS)
        map' `shouldFailOn` ("dfne" :: BS)
        map' `shouldFailOn` ("di123ene" :: BS)
        map' `shouldFailOn` ("dlene" :: BS)
        map' `shouldFailOn` ("ddene" :: BS)
