{-# LANGUAGE OverloadedStrings #-}
module CJK.Data.Unihan.Variants (
    SemanticVariantType(..),
    VariantSource, VariantCitation, Variant,
    compatibilityVariants, zVariants,
    semanticVariants, specializedSemanticVariants,
    simplifiedVariants, traditionalVariants
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


data SemanticVariantType = T -- T for tòng, U+540C 同. The indicated source explicitly indicates the two are the same (e.g., by saying that the one character is “the same as” the other).
                         | B -- T for bù, U+4E0D 不. The source explicitly indicates that the two are used improperly one for the other.
                         | Z -- T for zhèng, U+6B63 正. The source explicitly indicates that the given character is the preferred form
                         | F -- T for fán, U+7E41 繁. The source explicitly indicates that the given character is the traditional form.
                         | J -- T for jiǎn U+7C21 簡/U+7B80 简. The source explicitly indicates that the given character is the simplified form.
                         deriving (Eq, Ord, Show)

type VariantSource = Text.Text
type VariantCitation = [(VariantSource, [SemanticVariantType])]

type Variant = (Char, VariantCitation)

-- | The compatibility decomposition for this ideograph
compatibilityVariants :: Char -> [Char]
compatibilityVariants c = case variants of VMS mp _ _ _ _ _ -> M.findWithDefault [] c mp

-- | A semantic variant is an x- or y-variant with similar or identical meaning which can generally be used in place of the indicated character
semanticVariants :: Char -> [Variant]
semanticVariants c = case variants of VMS _ mp _ _ _ _ -> M.findWithDefault [] c mp

-- | Simplified Chinese variant(s) for this character
simplifiedVariants :: Char -> [Char]
simplifiedVariants c = case variants of VMS _ _ mp _ _ _ -> M.findWithDefault [] c mp

-- | A specialized semantic variant is an x- or y-variant with similar or identical meaning only in certain contexts (such as accountants’ numerals)
specializedSemanticVariants :: Char -> [Variant]
specializedSemanticVariants c = case variants of VMS _ _ _ mp _ _ -> M.findWithDefault [] c mp

-- | Traditional Chinese variant(s) for this character
traditionalVariants :: Char -> [Char]
traditionalVariants c = case variants of VMS _ _ _ _ mp _ -> M.findWithDefault [] c mp

-- | The z-variant(s) for this character
zVariants :: Char -> [Variant]
zVariants c = case variants of VMS _ _ _ _ _ mp -> M.findWithDefault [] c mp


type VariantMap      = M.Map Char [Char]
type CitedVariantMap = M.Map Char [Variant]
data VariantsMap = VMS !VariantMap !CitedVariantMap !VariantMap !CitedVariantMap !VariantMap !CitedVariantMap
                 deriving (Show) -- Useful for debugging in GHCi

emptyVariantsMap :: VariantsMap
emptyVariantsMap = VMS M.empty M.empty M.empty M.empty M.empty M.empty

unionVariantsMap :: VariantsMap -> VariantsMap -> VariantsMap
unionVariantsMap (VMS a1 a2 a3 a4 a5 a6) (VMS b1 b2 b3 b4 b5 b6)
  = VMS (M.unionWith (++) a1 b1) (M.unionWith (++) a2 b2) (M.unionWith (++) a3 b3) (M.unionWith (++) a4 b4) (M.unionWith (++) a5 b5) (M.unionWith (++) a6 b6)


{-# NOINLINE contents #-}
contents :: TextL.Text
contents = unsafePerformIO (readUTF8File "data/Unihan/Unihan_Variants.txt")

variants :: VariantsMap
variants = parseLazy fileP contents


fileP :: Parser VariantsMap
fileP = fmap (foldl' unionVariantsMap emptyVariantsMap) (lineP `manyTill` endOfInput)

lineP :: Parser VariantsMap
lineP = do { c <- charP <* skipSpace; dataP <- variantP c <* skipSpace; dataP <* skipTrueSpace <* lineTerminator }
    <|> char '#' *> manyTill anyChar lineTerminator *> pure emptyVariantsMap
    <|> manyTill skipTrueSpace lineTerminator *> pure emptyVariantsMap
    <?> "line"

variantP :: Char -> Parser (Parser VariantsMap)
variantP c = string "kCompatibilityVariant"       *> pure (liftA (\x -> VMS (mk x) M.empty M.empty M.empty M.empty M.empty) charsP)
         <|> string "kSemanticVariant"            *> pure (liftA (\x -> VMS M.empty (mk x) M.empty M.empty M.empty M.empty) variantsP)
         <|> string "kSimplifiedVariant"          *> pure (liftA (\x -> VMS M.empty M.empty (mk x) M.empty M.empty M.empty) charsP)
         <|> string "kSpecializedSemanticVariant" *> pure (liftA (\x -> VMS M.empty M.empty M.empty (mk x) M.empty M.empty) variantsP)
         <|> string "kTraditionalVariant"         *> pure (liftA (\x -> VMS M.empty M.empty M.empty M.empty (mk x) M.empty) charsP)
         <|> string "kZVariant"                   *> pure (liftA (\x -> VMS M.empty M.empty M.empty M.empty M.empty (mk x)) variantsP)
         <?> "variant"
  where mk x = M.singleton c x

charsP :: Parser [Char]
charsP = charP `sepBy1` skipTrueSpace

variantsP :: Parser [Variant]
variantsP = liftA2 (,) charP variantCitationP `sepBy1` skipTrueSpace

semanticVariantTypeP :: Parser SemanticVariantType
semanticVariantTypeP = char 'T' *> pure T
                   <|> char 'B' *> pure B
                   <|> char 'Z' *> pure Z
                   <|> char 'F' *> pure F
                   <|> char 'J' *> pure J
                   <?> "semantic variant type"

variantCitationP :: Parser VariantCitation
variantCitationP = char '<' *> (entryP `sepBy1` char ',')
               <|> pure [] -- Z-variants are commonly uncited
               <?> "variant citation"
  where entryP = liftA2 (\which mb_xs -> (which, fromMaybe [] mb_xs)) sourceP (optional (char ':' *> many1 semanticVariantTypeP))
        sourceP = takeWhile1 isAlphaNum
