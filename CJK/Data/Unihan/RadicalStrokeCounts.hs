{-# LANGUAGE OverloadedStrings #-}
module CJK.Data.Unihan.RadicalStrokeCounts (
    -- * Dictionary- and standard-consistent radicals
    IsSimplifiedKangXi, unicode, kangXi, kanWa,

    -- * Language-consistent radicals
    korean, japanese,
    
    -- * Font-consistent radicals
    AdobeJapan1_6(..), adobeJapan1_6
  ) where

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


-- | Whether the character is formed from the simplifed version of the radical
type IsSimplifiedKangXi = Bool

data AdobeJapan1_6 = AJ1_6 {
    aJ1_6IsDirect           :: Bool, -- ^ True if the Unicode code point maps directly to the Adobe-Japan1-6 CID, or False if it is a variant form which is not directly encoded
    aJ1_6CID                :: Int,  -- ^ The ID of the character in the Adobe-Japan1-6 font
    aJ1_6RadicalStrokeCount :: RadicalStrokeCount (KangXiRadical, StrokeCount)
  } deriving (Show) -- Useful for debugging in GHCi

-- | Radical/stroke count in the Adobe-Japan1-6 font
--
-- This data is unusual in that it explicitly includes the stroke count for the form that the radical takes in the glyph.
adobeJapan1_6 :: Char -> [AdobeJapan1_6]
adobeJapan1_6 c = M.findWithDefault [] c (kRSAdobe_Japan1_6 strokeCounts)

-- | Radical/stroke counts usually used in Japanese
japanese :: Char -> [RadicalStrokeCount KangXiRadical]
japanese c = M.findWithDefault [] c (kRSJapanese strokeCounts)

-- | Radical/stroke counts consistent with the KangXi dictionary
kangXi :: Char -> [RadicalStrokeCount KangXiRadical]
kangXi c = M.findWithDefault [] c (kRSKangXi strokeCounts)

-- | Radical/stroke counts consistent with the Morohashi dictionary
kanWa :: Char -> [RadicalStrokeCount KangXiRadical]
kanWa c = M.findWithDefault [] c (kRSKanWa strokeCounts)

-- | Radical/stroke counts usually used in Japanese
korean :: Char -> [RadicalStrokeCount KangXiRadical]
korean c = M.findWithDefault [] c (kRSKorean strokeCounts)

-- | Radical/stroke count consistent with ISO/IEC 10646
--
-- The first value in the returned list, if any, is equal to the normative radical-stroke value defined in ISO/IEC 10646.
unicode :: Char -> [RadicalStrokeCount (KangXiRadical, IsSimplifiedKangXi)]
unicode c = M.findWithDefault [] c (kRSUnicode strokeCounts)


data StrokeCountsMap = SMS  {
    kRSAdobe_Japan1_6 :: !(M.Map Char [AdobeJapan1_6]),
    kRSJapanese       :: !(M.Map Char [RadicalStrokeCount KangXiRadical]),
    kRSKangXi         :: !(M.Map Char [RadicalStrokeCount KangXiRadical]),
    kRSKanWa          :: !(M.Map Char [RadicalStrokeCount KangXiRadical]),
    kRSKorean         :: !(M.Map Char [RadicalStrokeCount KangXiRadical]),
    kRSUnicode        :: !(M.Map Char [RadicalStrokeCount (KangXiRadical, IsSimplifiedKangXi)])
  } deriving (Show) -- Useful for debugging in GHCi

emptyStrokeCountsMap :: StrokeCountsMap
emptyStrokeCountsMap = SMS M.empty M.empty M.empty M.empty M.empty M.empty

unionStrokeCountsMap :: StrokeCountsMap -> StrokeCountsMap -> StrokeCountsMap
unionStrokeCountsMap (SMS a1 a2 a3 a4 a5 a6) (SMS b1 b2 b3 b4 b5 b6)
  = SMS (plus a1 b1) (plus a2 b2) (plus a3 b3) (plus a4 b4) (plus a5 b5) (plus a6 b6)
  where plus = M.unionWith (error "unionStrokeCountsMap: impossible") -- There is at most one line for each (character, field name) combination

{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8File "data/Unihan/Unihan_RadicalStrokeCounts.txt")

strokeCounts :: StrokeCountsMap
strokeCounts = parseLazy fileP contents


fileP :: Parser StrokeCountsMap
fileP = fmap (foldl' unionStrokeCountsMap emptyStrokeCountsMap) (lineP `manyTill` endOfInput)

lineP :: Parser StrokeCountsMap
lineP = do { c <- charP <* skipSpace; dataP <- strokeCountP c <* skipSpace; dataP <* skipTrueSpace <* lineTerminator }
    <|> char '#' *> manyTill anyChar lineTerminator *> pure emptyStrokeCountsMap
    <|> manyTill skipTrueSpace lineTerminator *> pure emptyStrokeCountsMap
    <?> "line"

strokeCountP :: Char -> Parser (Parser StrokeCountsMap)
strokeCountP c = string "kRSAdobe_Japan1_6" *> pure (liftA (\x -> emptyStrokeCountsMap { kRSAdobe_Japan1_6 = mk x }) (rsAdobe_Japan1_6P `sepBy1` skipTrueSpace))
             <|> string "kRSJapanese"       *> pure (liftA (\x -> emptyStrokeCountsMap { kRSJapanese       = mk x }) (radicalStrokeCountP `sepBy1` skipTrueSpace))
             <|> string "kRSKangXi"         *> pure (liftA (\x -> emptyStrokeCountsMap { kRSKangXi         = mk x }) (radicalStrokeCountP `sepBy1` skipTrueSpace))
             <|> string "kRSKanWa"          *> pure (liftA (\x -> emptyStrokeCountsMap { kRSKanWa          = mk x }) (radicalStrokeCountP `sepBy1` skipTrueSpace))
             <|> string "kRSKorean"         *> pure (liftA (\x -> emptyStrokeCountsMap { kRSKorean         = mk x }) (radicalStrokeCountP `sepBy1` skipTrueSpace))
             <|> string "kRSUnicode"        *> pure (liftA (\x -> emptyStrokeCountsMap { kRSUnicode        = mk x }) (rsUnicodeP `sepBy1` skipTrueSpace))
  where mk x = M.singleton c x

rsAdobe_Japan1_6P :: Parser AdobeJapan1_6
rsAdobe_Japan1_6P = liftA3 AJ1_6 isDirectP (char '+' *> decimal) (char '+' *> rscP)
  where isDirectP = char 'C' *> pure True
                <|> char 'V' *> pure False
        rscP = liftA3 (\kx kxn n -> RSC (KangXi kx, kxn) n) decimal (char '.' *> decimal) (char '.' *> decimal)

rsUnicodeP :: Parser (RadicalStrokeCount (KangXiRadical, IsSimplifiedKangXi))
rsUnicodeP = liftA3 (\kx is_simp n -> RSC (KangXi kx, is_simp) n) decimal (canParse (char '\'')) (char '.' *> decimal)

radicalStrokeCountP :: Parser (RadicalStrokeCount KangXiRadical)
radicalStrokeCountP = liftA2 RSC (fmap KangXi decimal) (char '.' *> decimal)
