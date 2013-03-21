{-# LANGUAGE BangPatterns, PatternGuards #-}
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

-- Places an accent mark on the Pinyin according to these rules (from <http://talkbank.org/pinyin/Tone_marks.php>):
--
--  1. If there is only one vowel, it takes the diacritic.
--  2. If there is more than one vowel, then the vowels {a}, {e}, or {o} take the diacritic.
--  3. If the vowel cluster is {ao}, then {a} takes the diacritic.
--  4. If the vowel cluster is {iu} or {ui}, the last letter takes the diacritic.
toAccented :: Phone -> Text.Text
toAccented yin = Text.normalize Text.NFC $ Text.pack $ go $ Text.unpack $ sound yin
  where go cs = case span isVowel cs of
          ([],  [])   -> [] -- All pinyin contain a vowel, so this can only happen when the pinyin is in fact invalid
          ([],  c:cs) -> c:go cs
          (vws, cs)   -> go' vws ++ cs

        go' :: String -> String
        -- 1. If there is only one vowel, it takes the diacritic.
        go' [vw] = vw : mark
        -- 2. If there is more than one vowel, then the vowels {a}, {e}, or {o} take the diacritic.
        go' vws | (vws1, vw:vws2) <- span (\vw -> not (isA vw || isE vw || isO vw)) vws = vws1 ++ (vw : mark ++ vws2)
        -- 3. If the vowel cluster is {ao}, then {a} takes the diacritic.
        go' [vw1, vw2] | isA vw1 && isO vw2 = vw1 : mark ++ [vw2]
        -- 4. If the vowel cluster is {iu} or {ui}, the last letter takes the diacritic.
        go' [vw1, vw2] | (isI vw1 && isU vw2) || (isU vw1 && isI vw2) = [vw1, vw2] ++ mark
        -- Default to just after the first vowel
        go' (vw:vws) = vw : mark ++ vws

        isA, isE, isI, isO, isU, isVowel :: Char -> Bool

        isA 'a' = True
        isA 'A' = True
        isA _   = False

        isE 'e' = True
        isE 'E' = True
        isE _   = False

        isI 'i' = True
        isI 'I' = True
        isI _   = False

        isO 'o' = True
        isO 'O' = True
        iso _   = False

        isU 'u' = True
        isU 'U' = True
        isU _   = False

        isVowel c = isA c || isE c || isI c || isO c || isU c

        mark = maybeToList (toneCombiningMark (tone yin))
