module Scrabble where

import Data.Monoid
import Data.Char (toLower)

data Score = Score { getScore :: Int } deriving (Show)

instance Monoid Score where
     mempty = Score 0
     mappend a b = Score $  getScore a + getScore b

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString =  mconcat . map score  

