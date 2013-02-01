module CJK.Data.Internal where

import qualified CJK.Data.Jyutping as Jyutping
import qualified CJK.Data.Pinyin   as Pinyin
import CJK.Data.Types

import Control.Applicative

import Data.Char
import Data.Attoparsec.Text


jyutpingP :: Parser Jyutping.Phone
jyutpingP = liftA2 Jyutping.Phone (takeWhile1 (\c -> isAsciiUpper c || isAsciiLower c || c == '[' || c == ']')) jyutpingToneP -- Some kCheungBauer says [ng]ai1

jyutpingToneP :: Parser Jyutping.Tone
jyutpingToneP = char '1' *> pure Jyutping.HighLevel
            <|> char '2' *> pure Jyutping.MidRising
            <|> char '3' *> pure Jyutping.MidLevel
            <|> char '4' *> pure Jyutping.LowFalling
            <|> char '5' *> pure Jyutping.LowRising
            <|> char '6' *> pure Jyutping.LowLevel


tonedPinyinP :: Parser Pinyin.Phone
tonedPinyinP = liftA2 Pinyin.Phone (takeWhile1 (\c -> isAsciiUpper c || isAsciiLower c)) pinyinToneP

pinyinToneP :: Parser Pinyin.Tone
pinyinToneP = char '1' *> pure Pinyin.Flat
          <|> char '2' *> pure Pinyin.Rising
          <|> char '3' *> pure Pinyin.FallingRising
          <|> char '4' *> pure Pinyin.Falling
          <|> char '5' *> pure Pinyin.Neutral

hdzEntryP :: Parser HDZEntry
hdzEntryP = takeWhile1 (\c -> isDigit c || c == '.')
