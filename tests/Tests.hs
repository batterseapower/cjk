module Main where

import Control.Monad
import Data.Maybe
import System.Exit

import qualified CJK.Data.Unihan.DictionaryLikeData  as DictionaryLikeData
import qualified CJK.Data.Unihan.NumericValues       as NumericValues
import qualified CJK.Data.Unihan.RadicalStrokeCounts as RadicalStrokeCounts
import qualified CJK.Data.Unihan.Readings            as Readings
import qualified CJK.Data.Unihan.Variants            as Variants
import qualified CJK.Data.CEDICT as CEDICT


-- Just check a single data point from each module. As long as the data parses
-- successfully there is a 90% chance everything is working
main :: IO ()
main = do
    checkNot $ isNothing $ DictionaryLikeData.cangjie '好'
    checkNot $ isNothing $ NumericValues.numericValue '十'
    checkNot $ length (RadicalStrokeCounts.unicode '好') == 0
    checkNot $ length (Readings.mandarinBestEffort '好') == 0
    checkNot $ length (Variants.traditionalVariants '电') == 0
    checkNot $ length CEDICT.entries == 0

checkNot :: Bool -> IO ()
checkNot p = when p $ exitWith (ExitFailure 1)