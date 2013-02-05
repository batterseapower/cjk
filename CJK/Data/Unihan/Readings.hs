{-# LANGUAGE OverloadedStrings #-}
module CJK.Data.Unihan.Readings (
    CharDefinition, definition,
    -- * Mandarin
    OccurrenceCount, IsHDZSubstitution,
    mandarinBestEffort, mandarin, hanyuPinlu, hanyuPinyin, xhc1983,
    -- * Cantonese
    cantonese,
    -- * Ancient Chinese
    CommonTangCharacter, tang,
    -- * Korean
    hangul, korean,
    -- * Japanese
    japaneseKun, japaneseOn,
    -- * Vietnamese
    vietnamese
  ) where

import qualified CJK.Data.Hangul     as Hangul
import qualified CJK.Data.Jyutping   as Jyutping
import qualified CJK.Data.KoreanYale as KoreanYale
import qualified CJK.Data.Pinyin     as Pinyin
import qualified CJK.Data.QuocNgu    as QuocNgu
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
import Data.List

import System.IO.Unsafe


type CharDefinition = Text.Text

-- | The sum total of the frequencies of the pronunciations of the character as given in 《現代漢語頻率詞典》 <Xiandai Hanyu Pinlu Cidian>
type OccurrenceCount = Int

-- | Whether the word or morpheme represented in toto or in part by the given character with the given reading occurs more than
-- four times in the seven hundred poems covered by "T’ang Poetic Vocabulary" by Hugh M. Stimson, Far Eastern Publications, Yale Univ. 1976
type CommonTangCharacter = Bool

-- | Whether this reference had an encoded variant substituted for an unencoded character used by the Hànyǔ Dà Zìdiǎn
type IsHDZSubstitution = Bool


-- | Returns how to pronounce an ideograph in Mandarin, making the best effort to use all of the CEDICT data to get a good answer.
-- Readings are returned in approximate frequency order.
--
-- This algorithm is based on the Unihan FAQ <http://www.unicode.org/faq/han_cjk.html>, which states that the best way is to use the kHanyuPinlu, kXHC1983,
-- and kHanyuPinyin fields in that order. The kMandarin field may have some readings the other three do not but should be used with caution. The kHanyuPinlu
-- field lists the most common readings for ideographs in order of frequency of use and is the most useful for most purposes. The kXHC1983
-- field contains the most important readings for characters in modern use, and the kHanyuPinyin field contains an exhaustive set of readings
-- for a large set of characters, but includes obscure readings of historic interest only
mandarinBestEffort :: Char -> [Pinyin.Phone]
mandarinBestEffort c = nubBy eq $ map fst (hanyuPinlu c) ++
                                  concatMap snd (xhc1983 c) ++
                                  concatMap snd (hanyuPinyin c) ++
                                  (case mandarin c of Nothing -> []; Just (simp, trad) -> [simp, trad]) -- NB: technically an abuse since this data is differentiated by mainland/Taiwan
  where yin1 `eq` yin2 = Text.toLower (Pinyin.sound yin1) == Text.toLower (Pinyin.sound yin2) && Pinyin.tone yin1 == Pinyin.tone yin2

-- | The Cantonese pronunciation(s) for this character using the jyutping romanization.
-- Cantonese pronunciations are sorted alphabetically, not in order of frequency.
--
-- Cantonese data are derived from the following sources:
--  * Casey, G. Hugh, S.J. Ten Thousand Characters: An Analytic Dictionary. Hong Kong: Kelley and Walsh,1980 (kPhonetic).
--  * Cheung Kwan-hin and Robert S. Bauer, The Representation of Cantonese with Chinese Characters, Journal of Chinese Linguistics Monograph Series Number 18, 2002.
--  * Roy T. Cowles, A Pocket Dictionary of Cantonese, Hong Kong: University Press, 1999 (kCowles).
--  * Sidney Lau, A Practical Cantonese-English Dictionary, Hong Kong: Government Printer, 1977 (kLau).
--  * Bernard F. Meyer and Theodore F. Wempe, Student’s Cantonese-English Dictionary, Maryknoll, New York: Catholic Foreign Mission Society of America, 1947 (kMeyerWempe).
--  * 饒秉才, ed. 廣州音字典, Hong Kong: Joint Publishing (H.K.) Co., Ltd., 1989.
--  * 中華新字典, Hong Kong:中華書局, 1987.
--  * 黃港生, ed. 商務新詞典, Hong Kong: The Commercial Press, 1991.
--  * 朗文初級中文詞典, Hong Kong: Longman, 2001.
cantonese :: Char -> [Jyutping.Phone]
cantonese c = M.findWithDefault [] c (kCantonese readings)

-- | An English definition for this character. Definitions are for modern written Chinese and are usually (but not always) the
-- same as the definition in other Chinese dialects or non-Chinese languages. In some cases, synonyms are indicated. Fuller variant
-- information can be found using the various variant fields.
--
-- Definitions specific to non-Chinese languages or Chinese dialects other than modern Mandarin are marked, e.g., (Cant.) or (J).
-- Minor definitions are separated by commas.
definition :: Char -> [CharDefinition]
definition c = M.findWithDefault [] c (kDefinition readings)

-- | The modern Korean pronunciation(s) for this character in Hangul.
hangul :: Char -> [Hangul.Phone]
hangul c = M.findWithDefault [] c (kHangul readings)

-- | The Pronunciations and Frequencies of this character, based in part on those appearing in
-- 《現代漢語頻率詞典》 <Xiandai Hanyu Pinlu Cidian> (XDHYPLCD) [Modern Standard Beijing Chinese Frequency Dictionary].
--
-- Where more than one pronunciation exists, these are sorted by descending frequency.
-- The occurrence count indicates the sum total of the frequencies of the pronunciations of the character as given in HYPLCD.
--
-- You may want to use 'mandarinBestEffort' instead of this function.
hanyuPinlu :: Char -> [(Pinyin.Phone, OccurrenceCount)]
hanyuPinlu c = M.findWithDefault [] c (kHanyuPinlu readings)

-- | The 漢語拼音 Hànyǔ Pīnyīn reading(s) appearing in the edition of 《漢語大字典》 Hànyǔ Dà Zìdiǎn (HDZ).
--
-- Where multiple pīnyīn readings are associated with a given mapping, these are ordered as in HDZ
-- (for the most part reflecting relative commonality).
--
-- Individual entries are in same order as they are found in the Hanyu Da Zidian. This is true both for
-- the locations and the individual readings. While this is generally in the order of utility for modern Chinese, such is not invariably the case.
--
-- You may want to use 'mandarinBestEffort' instead of this function.
hanyuPinyin :: Char -> [([HDZEntry], [Pinyin.Phone])]
hanyuPinyin c = M.findWithDefault [] c (kHanyuPinyin readings)

-- | The Japanese kun'yomi pronunciation of this character, in an undefined romanization system.
-- It is recommended that you use kanjidic2 <http://www.csse.monash.edu.au/~jwb/kanjidic2/> instead of this data.
japaneseKun :: Char -> [Text.Text]
japaneseKun c = M.findWithDefault [] c (kJapaneseKun readings)

-- | The Japanese on'yomi pronunciation of this character, in an undefined romanization system.
-- It is recommended that you use kanjidic2 <http://www.csse.monash.edu.au/~jwb/kanjidic2/> instead of this data.
japaneseOn :: Char -> [Text.Text]
japaneseOn c = M.findWithDefault [] c (kJapaneseOn readings)

-- | The Korean pronunciation(s) of this character, using the Yale romanization system.
korean :: Char -> [KoreanYale.Phone]
korean c = M.findWithDefault [] c (kKorean readings)

-- | The most customary pinyin reading for this character; that is, the reading most commonly used in modern text,
-- with some preference given to readings most likely to be in sorted lists. 
--
-- The first value returned is preferred for zh-Hans (CN) and the second is preferred for
-- zh-Hant (TW). Commonly, they will be exactly the same.
--
-- You may want to use 'mandarinBestEffort' instead of this function.
mandarin :: Char -> Maybe (Pinyin.Phone, Pinyin.Phone)
mandarin c = M.lookup c (kMandarin readings)

-- | The Tang dynasty pronunciation(s) of this character, in an undefined romanization.
tang :: Char -> [(CommonTangCharacter, Text.Text)]
tang c = M.findWithDefault [] c (kTang readings)

-- | The character’s pronunciation(s) in Quốc ngữ.
vietnamese :: Char -> [QuocNgu.Phone]
vietnamese c = M.findWithDefault [] c (kVietnamese readings)

-- | One or more Hànyǔ Pīnyīn readings as given in the Xiàndài Hànyǔ Cídiǎn.
--
-- You may want to use 'mandarinBestEffort' instead of this function.
xhc1983 :: Char -> [([(HDZEntry, IsHDZSubstitution)], [Pinyin.Phone])]
xhc1983 c = M.findWithDefault [] c (kXHC1983 readings)


data ReadingsMap = RMS  {
    kCantonese   :: !(M.Map Char [Jyutping.Phone]),
    kDefinition  :: !(M.Map Char [CharDefinition]),
    kHangul      :: !(M.Map Char [Hangul.Phone]),
    kHanyuPinlu  :: !(M.Map Char [(Pinyin.Phone, OccurrenceCount)]),
    kHanyuPinyin :: !(M.Map Char [([HDZEntry], [Pinyin.Phone])]),
    kJapaneseKun :: !(M.Map Char [Text.Text]), -- Kun and On readings are in mixed
    kJapaneseOn  :: !(M.Map Char [Text.Text]), -- romanization systems! Worthless...
    kKorean      :: !(M.Map Char [KoreanYale.Phone]),
    kMandarin    :: !(M.Map Char (Pinyin.Phone, Pinyin.Phone)),
    kTang        :: !(M.Map Char [(CommonTangCharacter, Text.Text)]), -- Who knows how this is romanized?
    kVietnamese  :: !(M.Map Char [QuocNgu.Phone]),
    kXHC1983     :: !(M.Map Char [([(HDZEntry, IsHDZSubstitution)], [Pinyin.Phone])])
  } deriving (Show) -- Useful for debugging in GHCi

emptyReadingsMap :: ReadingsMap
emptyReadingsMap = RMS M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

unionReadingsMap :: ReadingsMap -> ReadingsMap -> ReadingsMap
unionReadingsMap (RMS a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) (RMS b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
  = RMS (plus a1 b1) (plus a2 b2) (plus a3 b3) (plus a4  b4)  (plus a5  b5)  (plus a6  b6)
        (plus a7 b7) (plus a8 b8) (plus a9 b9) (plus a10 b10) (plus a11 b11) (plus a12 b12)
  where plus = M.unionWith (error "unionReadingsMap: impossible") -- There is at most one line for each (character, field name) combination

{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8DataFile "data/Unihan/Unihan_Readings.txt")

readings :: ReadingsMap
readings = parseLazy fileP contents


fileP :: Parser ReadingsMap
fileP = fmap (foldl' unionReadingsMap emptyReadingsMap) (lineP `manyTill` endOfInput)

lineP :: Parser ReadingsMap
lineP = do { c <- charP <* skipSpace; dataP <- readingP c <* skipSpace; dataP <* skipTrueSpace <* lineTerminator }
    <|> char '#' *> manyTill anyChar lineTerminator *> pure emptyReadingsMap
    <|> manyTill skipTrueSpace lineTerminator *> pure emptyReadingsMap
    <?> "line"

readingP :: Char -> Parser (Parser ReadingsMap)
readingP c = string "kCantonese"   *> pure (liftA (\x -> emptyReadingsMap { kCantonese   = mk x }) (jyutpingP `sepBy1` skipTrueSpace))
         <|> string "kDefinition"  *> pure (liftA (\x -> emptyReadingsMap { kDefinition  = mk x }) definitionsP)
         <|> string "kHangul"      *> pure (liftA (\x -> emptyReadingsMap { kHangul      = mk x }) (hangulP `sepBy1` skipTrueSpace))
         <|> string "kHanyuPinlu"  *> pure (liftA (\x -> emptyReadingsMap { kHanyuPinlu  = mk x }) (hanyuPinluP `sepBy1` skipTrueSpace))
         <|> string "kHanyuPinyin" *> pure (liftA (\x -> emptyReadingsMap { kHanyuPinyin = mk x }) (hanyuPinyinP `sepBy1` skipTrueSpace))
         <|> string "kJapaneseKun" *> pure (liftA (\x -> emptyReadingsMap { kJapaneseKun = mk x }) (takeWhile1 isAsciiUpper `sepBy1` skipTrueSpace))
         <|> string "kJapaneseOn"  *> pure (liftA (\x -> emptyReadingsMap { kJapaneseOn  = mk x }) (takeWhile1 isAsciiUpper `sepBy1` skipTrueSpace))
         <|> string "kKorean"      *> pure (liftA (\x -> emptyReadingsMap { kKorean      = mk x }) (yaleP `sepBy1` skipTrueSpace))
         <|> string "kMandarin"    *> pure (liftA (\x -> emptyReadingsMap { kMandarin    = mk x }) mandarinP)
         <|> string "kTang"        *> pure (liftA (\x -> emptyReadingsMap { kTang        = mk x }) (tangP `sepBy1` skipTrueSpace))
         <|> string "kVietnamese"  *> pure (liftA (\x -> emptyReadingsMap { kVietnamese  = mk x }) (quocNguP `sepBy1` skipTrueSpace))
         <|> string "kXHC1983"     *> pure (liftA (\x -> emptyReadingsMap { kXHC1983     = mk x }) (xhc1983P `sepBy1` skipTrueSpace))
  where mk x = M.singleton c x

definitionsP :: Parser [CharDefinition]
definitionsP = takeWhile1 (\c -> c /= '\r' && c /= '\n' && c /= ';') `sepBy1` (takeWhile1 (== ';') <* skipTrueSpace) -- Entry for U+4156 mistakely includes a double ;;

hangulP :: Parser Hangul.Phone
hangulP = liftA Hangul.fromJamos (takeWhile1 (not . isSpace))

hanyuPinluP :: Parser (Pinyin.Phone, OccurrenceCount)
hanyuPinluP = liftA2 (,) tonedPinyinP (char '(' *> decimal <* char ')')

mandarinP :: Parser (Pinyin.Phone, Pinyin.Phone)
mandarinP = liftA2 (\simp mb_trad -> (simp, fromMaybe simp mb_trad)) accentedPinyinP (optional (skipTrueSpace *> accentedPinyinP))

accentedPinyinP :: Parser Pinyin.Phone
accentedPinyinP = liftA Pinyin.fromAccented (takeWhile1 (\c -> not (isSpace c) && c /= ','))

hanyuPinyinP :: Parser ([HDZEntry], [Pinyin.Phone])
hanyuPinyinP = liftA2 (,) (hdzEntryP `sepBy1` char ',') (char ':' *> (accentedPinyinP `sepBy1` char ','))

yaleP :: Parser KoreanYale.Phone
yaleP = takeWhile1 isAsciiUpper

tangP :: Parser (CommonTangCharacter, Text.Text)
tangP = liftA2 (,) (canParse (char '*')) (takeWhile1 (not . isSpace))

quocNguP :: Parser QuocNgu.Phone
quocNguP = takeWhile1 (not . isSpace)

xhc1983P :: Parser ([(HDZEntry, IsHDZSubstitution)], [Pinyin.Phone])
xhc1983P = liftA2 (,) (locP `sepBy1` char ',') (char ':' *> (accentedPinyinP `sepBy1` char ','))
  where locP = liftA2 (,) hdzEntryP (canParse (char '*'))
