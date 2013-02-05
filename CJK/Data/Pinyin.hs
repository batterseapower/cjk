{-# LANGUAGE BangPatterns #-}
module CJK.Data.Pinyin where

import qualified Data.Text as Text
import qualified Data.Text.ICU.Normalize as Text
import Data.Maybe


data Tone = Flat
          | Rising
          | FallingRising
          | Falling
          | Neutral
          deriving (Eq, Ord)

instance Show Tone where
    show = show . toneNumber

toneNumber :: Tone -> Int
toneNumber Flat          = 1
toneNumber Rising        = 2
toneNumber FallingRising = 3
toneNumber Falling       = 4
toneNumber Neutral       = 5

-- | Returns the Unicode combining character used to produce the accent for this tone. Returns Nothing if no accent is required.
toneCombiningMark :: Tone -> Maybe Char
toneCombiningMark Flat          = Just '\x304'
toneCombiningMark Rising        = Just '\x301'
toneCombiningMark FallingRising = Just '\x30C'
toneCombiningMark Falling       = Just '\x300'
toneCombiningMark Neutral       = Nothing

-- | Returns the tone associated with this Unicode combining character, if any.
combiningMarkTone :: Char -> Maybe Tone
combiningMarkTone '\x304' = Just Flat
combiningMarkTone '\x301' = Just Rising
combiningMarkTone '\x30C' = Just FallingRising
combiningMarkTone '\x300' = Just Falling
combiningMarkTone _       = Nothing


data Phone = Phone {
    sound :: Text.Text,
    tone  :: Tone
  }

instance Show Phone where
    show yin = Text.unpack (sound yin) ++ show (tone yin)

fromAccented :: Text.Text -> Phone
fromAccented s = go [] Nothing $ Text.unpack $ Text.normalize Text.NFD s
  where go !tser !mb_tone cs = case cs of
            []           -> Phone { sound = Text.pack (reverse tser), tone = fromMaybe Neutral mb_tone }
            (c:cs)       -> case combiningMarkTone c of
                              Just tone -> go tser     (jst tone) cs
                              Nothing   -> go (c:tser) mb_tone    cs
          where jst tone' = case mb_tone of
                              Just tone | tone /= tone' -> error $ "Conflicting tones " ++ show tone ++ " and " ++ show tone' ++ " in " ++ Text.unpack s
                              _                         -> Just tone' -- Allow multiple tones of the same time, even if it is technically incorrect

toAccented :: Phone -> Text.Text
toAccented yin = Text.normalize Text.NFC $ Text.pack $ go $ Text.unpack $ sound yin
  where go []                 = show (tone yin) -- All pinyin contain a vowel, so this can only happen when the pinyin is in fact invalid
        go (c:cs) | isVowel c = maybeToList (toneCombiningMark (tone yin)) ++ c:cs
                  | otherwise = c : go cs

        isVowel 'a' = True
        isVowel 'e' = True
        isVowel 'i' = True
        isVowel 'o' = True
        isVowel 'u' = True
        isVowel 'A' = True
        isVowel 'E' = True
        isVowel 'I' = True
        isVowel 'O' = True
        isVowel 'U' = True
        isVowel _   = False
