module Data.Bencodex.Writer
    ( encodeLazy
    , encodeStrict
    ) where

import Data.List (sort)

import Data.ByteString hiding (sort)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import Data.HashMap.Strict
import Data.Text.Encoding

import Data.Bencodex.Types

encodeLazy :: BValue -> Data.ByteString.Lazy.ByteString
encodeLazy = toLazyByteString . renderBValue

encodeStrict :: BValue -> ByteString
encodeStrict = Data.ByteString.Lazy.toStrict . encodeLazy

renderBValue :: BValue -> Builder
renderBValue BNull = char7 'n'
renderBValue (BBool True) = char7 't'
renderBValue (BBool False) = char7 'f'
renderBValue (BInteger int) = mconcat
    [ char7 'i'
    , integerDec int
    , char7 'e'
    ]
renderBValue (BByteString bs) = mconcat
    [ intDec (Data.ByteString.length bs)
    , char7 ':'
    , byteString bs
    ]
renderBValue (BText txt) = mconcat
    [ char7 'u'
    , intDec (Data.ByteString.length encoded)
    , char7 ':'
    , byteString encoded
    ]
  where
    encoded :: ByteString
    encoded = encodeUtf8 txt
renderBValue (BList elements) =
    char7 'l' <> mconcat (renderBValue <$> elements) <> char7 'e'
renderBValue (BMap hmap) =
    char7 'd' <> mconcat renderedPairs <> char7 'e'
  where
    pairs :: [(KeyType, ByteString, BValue)]
    pairs = sort
        [ case k of
            BTextKey k' -> (UnicodeKey, encodeUtf8 k', v)
            BByteStringKey k' -> (BytesKey, k', v)
        | (k, v) <- toList hmap
        ]
    renderedPairs :: [Builder]
    renderedPairs =
        [ renderKey kt k <> renderBValue v
        | (kt, k, v) <- pairs
        ]

data KeyType = BytesKey | UnicodeKey deriving (Eq, Ord, Show)

renderKey :: KeyType -> ByteString -> Builder
renderKey kt bs =
    case kt of
        BytesKey -> common
        UnicodeKey -> char7 'u' <> common
  where
    common :: Builder
    common = mconcat
        [ intDec (Data.ByteString.length bs)
        , char7 ':'
        , byteString bs
        ]
