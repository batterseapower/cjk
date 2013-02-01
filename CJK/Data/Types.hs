module CJK.Data.Types where

import qualified Data.Text as Text


newtype KangXiRadical = KangXi {
    kangXiRadicalNumber :: Int -- ^ Radical number in the range 1 to 214 inclusive
  } deriving (Show) -- Useful for debugging in GHCi

type StrokeCount = Int

data RadicalStrokeCount a = RSC {
    radical           :: a,          -- ^ The radical which is considered to form the main part of the character
    additionalStrokes :: StrokeCount -- ^ The “additional strokes” value is the residual stroke-count, the count of all strokes remaining after eliminating all strokes associated with the radical.
  } deriving (Show) -- Useful for debugging in GHCi

-- | Location of the associated information in the《漢語大字典》 Hànyǔ Dà Zìdiǎn
type HDZEntry = Text.Text

-- | How to input the character in Cangjie
type CangjieInputCode = Text.Text
