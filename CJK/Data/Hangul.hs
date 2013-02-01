module CJK.Data.Hangul where

import qualified Data.Text as Text
import qualified Data.Text.ICU.Normalize as Text


-- TODO: flesh out this definition
type Phone = Char

fromJamos :: Text.Text -> Phone
fromJamos s = case Text.unpack (Text.normalize Text.NFC s) of
    [c] -> c
    cs -> error $ "Certainly non-Korean phone " ++ cs
