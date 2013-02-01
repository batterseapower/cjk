{-# LANGUAGE CPP #-}
module CJK.Utilities where

import Control.Applicative

import Data.Char
import qualified Data.ByteString.Lazy as BS

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL

import qualified Data.Attoparsec.Text as DAT
import qualified Data.Attoparsec.Text.Lazy as DATL


readUTF8File :: FilePath -> IO TextL.Text
readUTF8File = fmap TextL.decodeUtf8 . BS.readFile

parseLazy :: DAT.Parser a -> TextL.Text -> a
#ifndef MIN_VERSION_attoparsec
#define MIN_VERSION_attoparsec(A,B,C) 0
#endif
#if MIN_VERSION_attoparsec(0,10,4)
-- Lazy parse is bugged in attoparsec-0.10.3 and below (see ticket #10/#21)
parseLazy p s = case DATL.eitherResult (DATL.parse p s) of
    Left err -> error $ "parseLazy: " ++ err
    Right x  -> x
#else
parseLazy p s = case DAT.parseOnly p (TextL.toStrict s) of
    Left err -> error $ "parseLazy: " ++ err
    Right x  -> x
#endif


charP :: DAT.Parser Char
charP = fmap chr $ DAT.string (Text.pack "U+") *> DAT.hexadecimal

canParse :: DAT.Parser a -> DAT.Parser Bool
canParse p = p *> pure True <|> pure False

lineTerminator :: DAT.Parser ()
lineTerminator = DAT.endOfLine <|> DAT.endOfInput

skipTrueSpace :: DAT.Parser ()
skipTrueSpace = DAT.skipWhile isTrueSpace

isTrueSpace :: Char -> Bool
isTrueSpace c = isSpace c && c /= '\r' && c /= '\n'


liftA4 :: Applicative t => (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
liftA4 f ma mb mc md = pure f <*> ma <*> mb <*> mc <*> md
