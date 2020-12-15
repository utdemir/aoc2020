{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done, take, takeWhile)
import qualified Data.ByteString as BS
import Data.IntMap (IntMap)
import Data.IntMap as IM
import Data.List (findIndices, foldl1, maximum, maximumBy, minimum, minimumBy, tail)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Show
import Relude hiding (Constraint)
import qualified Relude.Unsafe as Unsafe
import System.IO (hPutStrLn)
import Prelude (last)

input :: [Int]
input = [8, 0, 17, 4, 1, 12]

play :: [Int] -> [Int]
play input =
  go
    ( IM.fromList (zip (Unsafe.init input) [1 ..]),
      length input,
      Unsafe.last input
    )
    & (Unsafe.init input ++)
  where
    go (past, round, curr) =
      curr :
      go
        ( IM.insert curr round past,
          succ round,
          case IM.lookup curr past of
            Nothing -> 0
            Just v -> round - v
        )

run1 :: [Int] -> _
run1 input =
  play input
    & (Unsafe.!! (2020 - 1))

run2 :: [Int] -> _
run2 input =
  play input
    & (Unsafe.!! (30000000 - 1))

main :: IO ()
main = do
  print $ run1 input
  print $ run2 input
