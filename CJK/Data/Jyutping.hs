module CJK.Data.Jyutping where

import qualified Data.Text as Text


data Tone = HighLevel
          | MidRising
          | MidLevel
          | LowFalling
          | LowRising
          | LowLevel
          deriving (Eq, Ord)

instance Show Tone where
    show = show . toneNumber

toneNumber :: Tone -> Int
toneNumber HighLevel  = 1
toneNumber MidRising  = 2
toneNumber MidLevel   = 3
toneNumber LowFalling = 4
toneNumber LowRising  = 5
toneNumber LowLevel   = 6


data Phone = Phone {
    sound :: Text.Text,
    tone  :: Tone
  }

instance Show Phone where
    show jyut = Text.unpack (sound jyut) ++ show (tone jyut)
