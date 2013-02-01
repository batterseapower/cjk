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
            ('\x304':cs) -> go tser     (jst Flat)          cs
            ('\x301':cs) -> go tser     (jst Rising)        cs
            ('\x30C':cs) -> go tser     (jst FallingRising) cs
            ('\x300':cs) -> go tser     (jst Falling)       cs
            (c:cs)       -> go (c:tser) mb_tone             cs
          where jst tone' = case mb_tone of
                              Nothing   -> Just tone'
                              Just tone -> error $ "Conflicting tones " ++ show tone ++ " and " ++ show tone' ++ " in " ++ Text.unpack s
