module Data.Bencodex.Reader
    ( Result (..)
    , bValue
    , byteString
    , decodeLazy
    , decodeStrict
    , false
    , integer
    , list
    , map'
    , null'
    , text
    , true
    ) where

import Control.Monad
import Data.Functor
import Data.Word

import Data.Attoparsec.ByteString hiding (Result)
import Data.Attoparsec.ByteString.Lazy as LP
import Data.ByteString
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error

import Data.Bencodex.Types

decodeLazy :: LB.ByteString -> Result BValue
decodeLazy = LP.parse bValue

decodeStrict :: ByteString -> Either String BValue
decodeStrict = parseOnly bValue

bValue :: Parser BValue
bValue = choice
    [ (null' $> BNull) <?> "null"
    , (BBool <$> true) <?> "true"
    , (BBool <$> false) <?> "false"
    , (BInteger <$> integer) <?> "integer"
    , (BByteString <$> byteString) <?> "byte string"
    , (BText <$> text) <?> "unicode text"
    , (BList <$> list) <?> "list"
    , (BMap <$> map') <?> "dictionary"
    ]

skipWord8 :: Word8 -> Parser ()
skipWord8 = void . word8

null' :: Parser ()
null' = skipWord8 0x6e  -- 'n'

true :: Parser Bool
true = do
    skipWord8 0x74  -- 't'
    return True

false :: Parser Bool
false = do
    skipWord8 0x66  -- 'f'
    return False

digits :: Num a => Parser a
digits = do
    digits' <- takeWhile1 (\ w -> 0x30 <= w && w <= 0x39) -- '0' - '9'
    return $ Data.ByteString.foldl'
        (\ i b -> i * 10 + fromIntegral (b - 0x30))
        0
        digits'

integer :: Parser Integer
integer = do
    skipWord8 0x69  -- 'i'
    sign <- option 1 (word8 0x2d $> -1)  -- 0x2d: '-'
    digits' <- digits
    skipWord8 0x65  -- 'e'
    return $ sign * digits'

byteString :: Parser ByteString
byteString = do
    size' <- digits
    skipWord8 0x3a  -- ':'
    Data.Attoparsec.ByteString.take size'

text :: Parser Text
text = do
    skipWord8 0x75  -- 'u'
    size' <- digits
    skipWord8 0x3a  -- ':'
    bytes <- Data.Attoparsec.ByteString.take size'
    case decodeUtf8' bytes of
        Right decoded -> return decoded
        Left (DecodeError msg _) -> fail msg
        Left e -> fail ("unexpected error: " ++ show e)

list :: Parser [BValue]
list = do
    skipWord8 0x6c  -- 'l'
    elements <- many' bValue
    skipWord8 0x65  -- 'e'
    return elements

map' :: Parser (HashMap BKey BValue)
map' = do
    skipWord8 0x64  -- 'd'
    pairs <- many' $ do
        key <- bKey
        value <- bValue
        return (key, value)
    skipWord8 0x65  -- 'e'
    return $ Data.HashMap.Strict.fromList pairs

bKey :: Parser BKey
bKey = choice
    [ BByteStringKey <$> byteString
    , BTextKey <$> text
    ]
