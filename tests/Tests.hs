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
    checkNot "DictionaryLikeData"  $ isNothing $ DictionaryLikeData.cangjie '好'
    checkNot "NumericValues"       $ isNothing $ NumericValues.numericValue '十'
    checkNot "RadicalStrokeCounts" $ length (RadicalStrokeCounts.unicode '好') == 0
    checkNot "Readings"            $ length (Readings.mandarinBestEffort '好') == 0
    checkNot "Variants"            $ length (Variants.traditionalVariants '电') == 0
    checkNot "CEDICT"              $ length CEDICT.entries == 0

checkNot :: String -> Bool -> IO ()
checkNot msg p = do
    putStr (msg ++ ": ")
    if p then putStrLn "failure" >> exitWith (ExitFailure 1)
         else putStrLn "success"