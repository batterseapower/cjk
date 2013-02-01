{-# LANGUAGE OverloadedStrings #-}
module CJK.Data.Unihan.DictionaryLikeData (
    cangjie,
    CheungBauer(..), cheungBauer,
    cihai,
    Fenn(..), fenn,
    fourCornerCode,
    frequency,
    gradeLevel,
    hdzRadBreak,
    hkGlyph,
    phonetic,
    totalStrokes
  ) where

import qualified CJK.Data.Jyutping as Jyutping
import CJK.Data.Internal
import CJK.Data.Types
import CJK.Utilities

import Control.Applicative

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Data.Attoparsec.Text

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Data.List

import System.IO.Unsafe


data CheungBauer = CB {
    cbRadicalStrokeCount :: RadicalStrokeCount KangXiRadical,
    cbCangjie            :: Maybe CangjieInputCode,
    cbReading            :: [Jyutping.Phone] -- ^ Readings are in alphabetical order
  } deriving (Show) -- Useful for debugging in GHCi

data Fenn = Fenn {
    fennSoothill  :: Maybe Int, -- ^ Soothill number of the character's phonetic, if any
    fennFrequency :: Maybe Int  -- ^ Number from 1 to 11 indicating roughly which group of 500 most popular characters this character is included in (i.e. 1 is the first 500 characters, 2 the next 500 characters etc). Nothing if the character is rare.
  } deriving (Show) -- Useful for debugging in GHCi

-- | The cangjie input code for the character
cangjie :: Char -> Maybe CangjieInputCode
cangjie c = M.lookup c (kCangjie dictionaryLikes)

-- | Data regarding the character in Cheung Kwan-hin and Robert S. Bauer, _The Representation of Cantonese with Chinese Characters_, Journal of Chinese Linguistics, Monograph Series Number 18, 2002
cheungBauer :: Char -> [CheungBauer]
cheungBauer c = M.findWithDefault [] c (kCheungBauer dictionaryLikes)

-- | The position(s) of this character in the Cihai (辭海) dictionary, single volume edition, published in Hong Kong by the Zhonghua Bookstore, 1983 (reprint of the 1947 edition), ISBN 962-231-005-2.
--
-- The position is indicated by a decimal number. The digits to the left of the decimal are the page number. The first digit after the decimal is the row on the page, and the remaining two digits after the decimal are the position on the row.
cihai :: Char -> [Text.Text]
cihai c = M.findWithDefault [] c (kCihaiT dictionaryLikes)

-- | Data on the character from The Five Thousand Dictionary (aka Fenn’s Chinese-English Pocket Dictionary) by Courtenay H. Fenn, Cambridge, Mass.: Harvard University Press, 1979.
fenn :: Char -> [Fenn]
fenn c = M.findWithDefault [] c (kFenn dictionaryLikes)

-- | The four-corner code(s) for the character
--
-- The four-corner system assigns each character a four-digit code from 0 through 9. The digit is derived from the “shape” of the four corners of the character (upper-left, upper-right, lower-left, lower-right). An optional fifth digit
-- can be used to further distinguish characters; the fifth digit is derived from the shape in the character’s center or region immediately to the left of the fourth corner.
--
-- The four-corner system is now used only rarely. Full descriptions are available online, e.g., at <http://en.wikipedia.org/wiki/Four_corner_input>.
fourCornerCode :: Char -> [Text.Text]
fourCornerCode c = M.findWithDefault [] c (kFourCornerCode dictionaryLikes)

-- | A rough frequency measurement for the character based on analysis of traditional Chinese USENET postings; characters with a kFrequency of 1 are the most common, those with a kFrequency of 2 are less common, and so on, through a kFrequency of 5.
frequency :: Char -> Maybe Int
frequency c = M.lookup c (kFrequency dictionaryLikes)

-- | The primary grade in the Hong Kong school system by which a student is expected to know the character; this data is derived from 朗文初級中文詞典, Hong Kong: Longman, 2001
gradeLevel :: Char -> Maybe Int
gradeLevel c = M.lookup c (kGradeLevel dictionaryLikes)

-- | Does 《漢語大字典》 Hanyu Da Zidian have a radical break beginning at this character’s position? If so, returns the radical and the Hanyu Da Zidian position as in the kHanyu field.
hdzRadBreak :: Char -> Maybe (Char, HDZEntry)
hdzRadBreak c = M.lookup c (kHDZRadBreak dictionaryLikes)

-- | The index of the character in 常用字字形表 (二零零零年修訂本),香港: 香港教育學院, 2000, ISBN 962-949-040-4. This publication gives the “proper” shapes for 4759 characters as used in the Hong Kong school system
hkGlyph :: Char -> [Int]
hkGlyph c = M.findWithDefault [] c (kHKGlyph dictionaryLikes)

-- | The phonetic index for the character from _Ten Thousand Characters: An Analytic Dictionary_, by G. Hugh Casey, S.J. Hong Kong: Kelley and Walsh, 1980
phonetic :: Char -> [Text.Text]
phonetic c = M.findWithDefault [] c (kPhonetic dictionaryLikes)

-- | The total number of strokes in the character (including the radical), that is, the stroke count most commonly associated with the character in modern text using customary fonts.
--
-- The first value is preferred for zh-Hans (CN) and the second is preferred for zh-Hant (TW)
totalStrokes :: Char -> Maybe (StrokeCount, StrokeCount)
totalStrokes c = M.lookup c (kTotalStrokes dictionaryLikes)


data DictionaryLikesMap = DMS {
    kCangjie        :: M.Map Char CangjieInputCode,
    kCheungBauer    :: M.Map Char [CheungBauer],
    kCihaiT         :: M.Map Char [Text.Text],
    kFenn           :: M.Map Char [Fenn],
    kFourCornerCode :: M.Map Char [Text.Text],
    kFrequency      :: M.Map Char Int,
    kGradeLevel     :: M.Map Char Int,
    kHDZRadBreak    :: M.Map Char (Char, HDZEntry),
    kHKGlyph        :: M.Map Char [Int],
    kPhonetic       :: M.Map Char [Text.Text],
    kTotalStrokes   :: M.Map Char (StrokeCount, StrokeCount)
  } deriving (Show) -- Useful for debugging in GHCi

emptyDictionaryLikesMap :: DictionaryLikesMap
emptyDictionaryLikesMap = DMS M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

unionDictionaryLikesMap :: DictionaryLikesMap -> DictionaryLikesMap -> DictionaryLikesMap
unionDictionaryLikesMap  (DMS a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11) (DMS b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11)
  = DMS (plus a1 b1) (plus a2 b2) (plus a3 b3) (plus a4  b4)  (plus a5  b5)  (plus a6  b6)
        (plus a7 b7) (plus a8 b8) (plus a9 b9) (plus a10 b10) (plus a11 b11)
  where plus = M.unionWith (error "unionReadingsMap: impossible") -- There is at most one line for each (character, field name) combination

{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8File "data/Unihan/Unihan_DictionaryLikeData.txt")

dictionaryLikes :: DictionaryLikesMap
dictionaryLikes = parseLazy fileP contents


fileP :: Parser DictionaryLikesMap
fileP = fmap (foldl' unionDictionaryLikesMap emptyDictionaryLikesMap) (lineP `manyTill` endOfInput)

lineP :: Parser DictionaryLikesMap
lineP = do { c <- charP <* skipSpace; dataP <- dictionaryLikeP c <* skipSpace; dataP <* skipTrueSpace <* lineTerminator }
    <|> char '#' *> manyTill anyChar lineTerminator *> pure emptyDictionaryLikesMap
    <|> manyTill skipTrueSpace lineTerminator *> pure emptyDictionaryLikesMap
    <?> "line"

dictionaryLikeP :: Char -> Parser (Parser DictionaryLikesMap)
dictionaryLikeP c = string "kCangjie"        *> pure (liftA (\x -> emptyDictionaryLikesMap { kCangjie        = mk x }) cangjieP)
                <|> string "kCheungBauer"    *> pure (liftA (\x -> emptyDictionaryLikesMap { kCheungBauer    = mk x }) (cheungBauerP `sepBy1` skipTrueSpace))
                <|> string "kCihaiT"         *> pure (liftA (\x -> emptyDictionaryLikesMap { kCihaiT         = mk x }) (takeWhile1 (\c -> isDigit c || c == '.') `sepBy1` skipTrueSpace))
                <|> string "kFenn"           *> pure (liftA (\x -> emptyDictionaryLikesMap { kFenn           = mk x }) (fennP `sepBy1` skipTrueSpace))
                <|> string "kFourCornerCode" *> pure (liftA (\x -> emptyDictionaryLikesMap { kFourCornerCode = mk x }) (takeWhile1 (\c -> isDigit c || c == '.') `sepBy1` skipTrueSpace))
                <|> string "kFrequency"      *> pure (liftA (\x -> emptyDictionaryLikesMap { kFrequency      = mk x }) decimal)
                <|> string "kGradeLevel"     *> pure (liftA (\x -> emptyDictionaryLikesMap { kGradeLevel     = mk x }) decimal)
                <|> string "kHDZRadBreak"    *> pure (liftA (\x -> emptyDictionaryLikesMap { kHDZRadBreak    = mk x }) hdzRadBreakP)
                <|> string "kHKGlyph"        *> pure (liftA (\x -> emptyDictionaryLikesMap { kHKGlyph        = mk x }) (decimal `sepBy1` skipTrueSpace))
                <|> string "kPhonetic"       *> pure (liftA (\x -> emptyDictionaryLikesMap { kPhonetic       = mk x }) (takeWhile1 (\c -> isDigit c || isAsciiUpper c || c == '*') `sepBy1` skipTrueSpace))
                <|> string "kTotalStrokes"   *> pure (liftA (\x -> emptyDictionaryLikesMap { kTotalStrokes   = mk x }) totalStrokesP)
  where mk x = M.singleton c x

cangjieP :: Parser CangjieInputCode
cangjieP = takeWhile1 isAsciiUpper

cheungBauerP :: Parser CheungBauer
cheungBauerP = liftA3 CB rscP (char ';' *> optional cangjieP) (char ';' *> liftA concat (jyutpingPatternP `sepBy1` char ','))
  where rscP = liftA2 RSC (fmap KangXi decimal) (char '/' *> decimal)

jyutpingPatternP :: Parser [Jyutping.Phone]
jyutpingPatternP = liftA2 (\sounds tones -> [Jyutping.Phone sound tone | sound <- sounds, tone <- tones]) soundP toneP
  where
    -- Some kCheungBauer says [ng]ai1
    soundP = liftA2 (\opt nexts -> [here | next <- nexts, here <- [next, opt <> next]]) (char '[' *> takeWhile1 (/= ']') <* char ']') soundP
         <|> liftA (\x -> [x]) (takeWhile1 (\c -> isAsciiUpper c || isAsciiLower c))

    -- Some kCheungBauer says min6/2
    toneP = jyutpingToneP `sepBy1` char '/'

fennP :: Parser Fenn
fennP = liftA2 Fenn groupP (optional (char 'a') *> frequencyP) -- Can't find any info on what the optional 'a' means
  where groupP = char '0' *> pure Nothing -- Characters which have a frequency letter but no Soothill phonetic group
             <|> fmap Just decimal
        frequencyP = char 'A' *> return (Just 1)
                 <|> char 'B' *> return (Just 2)
                 <|> char 'C' *> return (Just 3)
                 <|> char 'D' *> return (Just 4)
                 <|> char 'E' *> return (Just 5)
                 <|> char 'F' *> return (Just 6)
                 <|> char 'G' *> return (Just 7)
                 <|> char 'H' *> return (Just 8)
                 <|> char 'I' *> return (Just 9)
                 <|> char 'J' *> return (Just 10)
                 <|> char 'K' *> return (Just 11)
                 <|> char 'P' *> return Nothing -- Conflate these two cases:
                 <|> char '*' *> return Nothing -- who really cares?

hdzRadBreakP :: Parser (Char, HDZEntry)
hdzRadBreakP = liftA2 (,) anyChar (char '[' *> string "U+" *> takeWhile1 isHexDigit *> char ']' *> char ':' *> hdzEntryP)

totalStrokesP :: Parser (Int, Int)
totalStrokesP = liftA2 (\simp mb_trad -> (simp, fromMaybe simp mb_trad)) decimal (skipTrueSpace *> optional decimal)
