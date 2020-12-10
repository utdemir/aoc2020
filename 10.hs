{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done, takeWhile)
import qualified Data.ByteString as BS
import Data.List (findIndices, foldl1, maximum, minimum, tail)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Relude hiding (Constraint, tail)
import System.IO (hPutStrLn)

parser :: Parser [Int]
parser = many1 (decimal <* endOfLine)

run1 :: [Int] -> Int
run1 adapters =
  let sorted = 0 : sort adapters ++ [selfJoltage]
      diffs =
        zip sorted (tail sorted)
          & map (\(i, j) -> j - i)
   in length (filter (== 1) diffs) * length (filter (== 3) diffs)
  where
    selfJoltage = maximum adapters + 3

run2 :: [Int] -> Integer
run2 adapters =
  let sorted = sort adapters
   in go [(0, 1)] sorted
  where
    go :: [(Int, Integer)] -> [Int] -> Integer
    go ((n, c) : _) [] = c
    go ps (curr : xs) =
      let ps' = takeWhile ((>= curr - 3) . fst) ps
       in go ((curr, sum (map snd ps')) : ps) xs

main :: IO ()
main = do
  bs <- BS.readFile "10.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
