{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module CJK.Data.CEDICT (
    Reading, showReading,
    Word(..), showHeadWord,
    DefinitionToken(..), WordDefinition(..), Definition(..),
    entries
  ) where

import CJK.Utilities
import CJK.Data.Internal
import CJK.Data.Pinyin

import Control.Applicative
import Data.Maybe
import Data.List (intercalate)

import qualified Data.ByteString.Lazy as BS

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL

import Data.Char
import Data.Monoid
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text hiding (parse, eitherResult)
import Data.Attoparsec.Text.Lazy

import System.IO.Unsafe

import Prelude hiding (takeWhile)


type Reading = [Either Text.Text Phone]

showReading :: Reading -> String
showReading yins = intercalate " " (map (either Text.unpack show) yins)

showReadingAccented :: Reading -> String
showReadingAccented yins = intercalate " " (map (either Text.unpack (Text.unpack . toAccented)) yins)


data Word = Word {
    traditional :: [Char],
    simplified  :: [Char],
    reading     :: Reading
  }

bracketed s = "[" ++ s ++ "]"

instance Show Word where
    show word | simplified word == traditional word = traditional word                           ++ bracketed (showReading (reading word))
              | otherwise                           = traditional word ++ "|" ++ simplified word ++ bracketed (showReading (reading word))

-- | Show a word as in the head of a dictionary entry
showHeadWord :: Word -> String
showHeadWord word = traditional word ++ " " ++ simplified word ++ " " ++ bracketed (showReading (reading word))

mkWord :: [Char] -> [Char] -> Reading -> Word
-- Fix problems in dictionary:
mkWord trad simp yins
  | ntrad /= nsimp = error $ "mkWord: differing numbers of traditional and simplified characters (" ++ show trad ++ " vs. " ++ show simp ++ ")"
  | otherwise      = case (trad, simp) of
    ("中國左翼作家聯盟",           "中国左翼作家联盟")           | nyins == 6 -> Word trad simp ([Right (Phone "Zhong" Flat), Right (Phone "guo" Rising)] ++ yins)
    ("甘孜藏族自治州甘孜藏族自治州", "甘孜藏族自治州甘孜藏族自治州") | nyins == 7 -> Word "甘孜藏族自治州" "甘孜藏族自治州" yins
    ("睿宗",                     "睿宗")                    | nyins == 3 -> Word trad simp (tail yins)
    ("泰米爾納德",                "泰米尔纳德")                | nyins == 6 -> Word trad simp (init yins)
    ("Zhou周文王",               "Zhou周文王")               | nyins == 3 -> Word "周文王" "周文王" yins
    ("美國５１區",                "美国５１区")                | nyins == 6 -> Word "美國五十一區" "美国五十一区" yins
    _ -- Check for missing 市 suffix which is present in yins in examples like 棗莊|枣庄
      | ntrad + 1 == nyins, Right (Phone "shi" Falling) <- last yins -> Word trad simp (init yins)
      -- Check for 市 suffix which is missing in yins in examples like 鹿泉市
      | ntrad == nyins + 1, '市'                        <- last trad -> Word trad simp (yins ++ [Right (Phone "shi" Falling)])
      -- Last-ditch check for an unhandled error
      | ntrad /= nyins -> error $ "mkWord: differing numbers of characters and readings (" ++ show trad ++ " vs. " ++ bracketed (showReading yins) ++ ")"
      | otherwise      -> Word trad simp yins
  where ntrad = length trad
        nsimp = length simp
        nyins = length yins


data DefinitionToken = PlainToken Text.Text
                     | WordToken Word

instance Show DefinitionToken where
    show (PlainToken text) = Text.unpack text
    show (WordToken word)  = show word


data WordDefinition = WordClassifiers [Word]
                    | WordDefinition [DefinitionToken]

instance Show WordDefinition where
    show (WordClassifiers wrds)  = "CL:" ++ intercalate "," (map show wrds)
    show (WordDefinition tokens) = concatMap show tokens


data Definition = Definition {
    word        :: Word,
    definitions :: [WordDefinition]
  }

instance Show Definition where
    show definition = showHeadWord (word definition) ++ " /" ++ intercalate "/" (map show (definitions definition)) ++ "/"


{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8File "data/cedict_1_0_ts_utf-8_mdbg.txt")

entries :: [Definition]
entries = parseLazy fileP contents

fileP :: Parser [Definition]
fileP = fmap catMaybes (many lineP)

lineP :: Parser (Maybe Definition)
lineP = char '#' *> manyTill anyChar lineTerminator *> pure Nothing
    <|> liftA4 (\trad simp yins defs -> Just (Definition { word = mkWord trad simp yins, definitions = defs })) nonSpaceP nonSpaceP (readingP <* space) definitionsP <* lineTerminator

readingP :: Parser Reading
readingP = char '[' *> (yinP `sepBy1` space) <* char ']'

yinP :: Parser (Either Text.Text Phone)
yinP = liftA Right tonedPinyinP
   <|> liftA Left  (takeWhile1 (\c -> not (isSpace c) && c /= ']')) -- CEDICT explicitly writes tone 5, so any missing tones must be for non-Chinese

toneP :: Parser Tone
toneP = char '1' *> pure Flat
    <|> char '2' *> pure Rising
    <|> char '3' *> pure FallingRising
    <|> char '4' *> pure Falling
    <|> char '5' *> pure Neutral

definitionsP :: Parser [WordDefinition]
definitionsP = char '/' *> many1 (definitionP <* char '/')

definitionP :: Parser WordDefinition
definitionP = liftA WordClassifiers (string "CL:" *> (wordP `sepBy1` (char ',' >> skipWhile isSpace))) -- In entries like 個|个[ge4] or CL:個|个[ge4],隻|只[zhi1] the characters do not have to have a space before them, so special case it
          <|> liftA WordDefinition (many tokenP)

tokenP :: Parser DefinitionToken
tokenP = liftA WordToken wordP
     <|> liftA3 (\hoklo chars end -> PlainToken (hoklo <> chars <> end)) (string "Hoklo:") (takeWhile (/= ']')) (string "]") -- There are two rogue entries containing Hoklo: 無甚物[bô-siáⁿ-mi̍h]
     <|> liftA PlainToken (takeWhile1 (\c -> not (isSpace c || c == '(') && c /= '/'))
     <|> liftA PlainToken (takeWhile1 (\c -> isTrueSpace c || c == '('))

wordP :: Parser Word
wordP = liftA3 (\trad mb_simp yins -> mkWord trad (fromMaybe trad mb_simp) yins) chineseP (optional (char '|' *> chineseP)) readingP
  where
    chineseP :: Parser [Char]
    chineseP = many1 (satisfy (\c -> not (isSpace c) && c /= '/' && c /= '|' && c /= '['))

nonSpaceP :: Parser [Char]
nonSpaceP = many1 (satisfy (not . isSpace)) <* space
