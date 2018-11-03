{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Bencodex.SpecDiscovery
    ( Spec' (..)
    , listSpecs
    ) where

import Data.Char

import Data.Bencodex.Types
import Data.ByteString hiding (unpack)
import Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Text
import Data.Text.Encoding
import Data.YAML
import Data.YAML.Event hiding (Scalar)
import System.FilePath

data Spec' = Spec'
    { specPath :: FilePath
    , dataPath :: FilePath
    , expectedValue :: BValue
    , expectedEncoding :: ByteString
    } deriving (Eq, Ord, Show)

listSpecs :: Either String [Spec']
listSpecs = sequence
    [ case fromYaml specContents of
        Left msg -> Left (specPath' ++ ": " ++ msg)
        Right expValue -> Right $ Spec' specPath' dataPath' expValue expEnc
    | (specPath', specContents, dataPath', expEnc) <- files
    ]
  where
    dir :: HashMap.HashMap FilePath ByteString
    dir = HashMap.fromList
            $(embedDir $ joinPath ["spec", "testsuite"])
    files :: [(FilePath, ByteString, FilePath, ByteString)]
    files =
        [ (fileName, valueContents, fileName -<.> "dat", dataContents)
        | (fileName, valueContents) <- HashMap.toList dir
        , "yaml" `isExtensionOf` fileName || "yml" `isExtensionOf` fileName
        , Just dataContents <- [HashMap.lookup (fileName -<.> "dat") dir]
        ]

fromYaml :: ByteString -> Either String BValue
fromYaml fileContents =
    case decodeNode (fromStrict fileContents) of
        Right [Doc node] -> mapNode node
        Right _ -> Left "A YAML file must have only one document."
        Left msg -> Left msg

mapNode :: Node -> Either String BValue
mapNode (Scalar SNull) = Right BNull
mapNode (Scalar (SBool bool)) = Right $ BBool bool
mapNode (Scalar (SInt int)) = Right $ BInteger int
mapNode (Scalar (SUnknown tag base64))
  | tagToText tag == Just "tag:yaml.org,2002:binary" =
        case decodeBase64 base64 of
            Left msg -> Left msg
            Right bs -> Right $ BByteString bs
  | otherwise = Left ("Unsupported tag: " ++ show tag)
mapNode (Scalar (SStr text)) = Right $ BText text
mapNode (Scalar (SFloat _)) = Left "Float is unsupported."
mapNode (Sequence _ nodes) = BList <$> mapM mapNode nodes
mapNode (Mapping _ map') =
    BMap . HashMap.fromList <$> sequence translatedPairs
  where
    pairs :: [(Node, Node)]
    pairs = Map.toList map'
    translatedPairs :: [Either String (BKey, BValue)]
    translatedPairs =
        [ case (k, mapNode v) of
            (Scalar (SStr k'), Right v') -> Right (BTextKey k', v')
            (Scalar (SUnknown tag k64), Right v')
                | tagToText tag == Just "tag:yaml.org,2002:binary" ->
                    case decodeBase64 k64 of
                        Left msg -> Left msg
                        Right k' -> Right (BByteStringKey k', v')
                | otherwise -> Left ("Unsupported tag: " ++ show tag)
            (_, Left msg) -> Left msg
            _ -> Left "Unsupported key type."
        | (k, v) <- pairs
        ]
mapNode (Anchor _ _) = Left "Anchor is unsupported."

decodeBase64 :: Text -> Either String ByteString
decodeBase64 base64 =
    case Base64.decode (encodeUtf8 base64') of
        Right b -> Right b
        Left msg -> Left (msg ++ ":\n" ++ unpack base64')
  where
    base64' :: Text
    base64' = Data.Text.filter (not . isSpace) base64
