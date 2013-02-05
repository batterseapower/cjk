{-# LANGUAGE OverloadedStrings #-}
module CJK.Data.Unihan.NumericValues (
    NumericUse(..), numericValue
  ) where

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


data NumericUse = AccountingUse -- ^ Used in the writing of accounting numerals (to prevent fraud)
                | OtherUse      -- ^ Used in certain unusual, specialized contexts
                | PrimaryUse    -- ^ Used in the writing of numbers in the standard fashion
                deriving (Eq, Show)

-- | The value of the character and the contexts in which it is used
numericValue :: Char -> Maybe (NumericUse, Integer)
numericValue c = M.lookup c numericValues


type NumericValuesMap = M.Map Char (NumericUse, Integer)

emptyNumericValuesMap :: NumericValuesMap
emptyNumericValuesMap = M.empty

unionNumericValuesMap :: NumericValuesMap -> NumericValuesMap -> NumericValuesMap
unionNumericValuesMap = M.unionWith (error "unionNumericValuesMap: impossible") -- There is at most one line for each (character, field name) combination

{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8DataFile "data/Unihan/Unihan_NumericValues.txt")

numericValues :: NumericValuesMap
numericValues = parseLazy fileP contents


fileP :: Parser NumericValuesMap
fileP = fmap (foldl' unionNumericValuesMap emptyNumericValuesMap) (lineP `manyTill` endOfInput)

lineP :: Parser NumericValuesMap
lineP = do { c <- charP <* skipSpace; dataP <- numericValueP c <* skipSpace; dataP <* skipTrueSpace <* lineTerminator }
    <|> char '#' *> manyTill anyChar lineTerminator *> pure emptyNumericValuesMap
    <|> manyTill skipTrueSpace lineTerminator *> pure emptyNumericValuesMap
    <?> "line"

numericValueP :: Char -> Parser (Parser NumericValuesMap)
numericValueP c = string "kAccountingNumeric" *> pure (liftA (mk AccountingUse) decimal)
              <|> string "kOtherNumeric"      *> pure (liftA (mk OtherUse)      decimal)
              <|> string "kPrimaryNumeric"    *> pure (liftA (mk PrimaryUse)    decimal)
  where mk use x = M.singleton c (use, x)
