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

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Action
  = AMove Direction Int
  | ALeft Int
  | ARight Int
  | AForward Int
  deriving (Eq, Show)

type Input = [Action]

type St = (Int, Int, Direction)

parser :: Parser Input
parser = many1 (action <* endOfLine)
  where
    action =
      (char 'N' *> decimal <&> AMove North)
        <|> (char 'S' *> decimal <&> AMove South)
        <|> (char 'E' *> decimal <&> AMove East)
        <|> (char 'W' *> decimal <&> AMove West)
        <|> (char 'L' *> decimal <&> ALeft)
        <|> (char 'R' *> decimal <&> ARight)
        <|> (char 'F' *> decimal <&> AForward)

step :: Action -> St -> St
step (AMove North n) (x, y, dir) = (x, y - n, dir)
step (AMove South n) (x, y, dir) = (x, y + n, dir)
step (AMove East n) (x, y, dir) = (x - n, y, dir)
step (AMove West n) (x, y, dir) = (x + n, y, dir)
step (ALeft n) (x, y, dir) = (x, y, turnLeft dir n)
step (ARight n) (x, y, dir) = (x, y, turnRight dir n)
step (AForward n) (x, y, dir) = step (AMove dir n) (x, y, dir)

turnLeft :: Direction -> Int -> Direction
turnLeft dir deg =
  case divMod deg 90 of
    (n, 0) -> go n dir
    _ -> error "invalid angle"
  where
    go 0 d = d
    go i North = go (i -1) West
    go i West = go (i -1) South
    go i South = go (i -1) East
    go i East = go (i -1) North

turnRight :: Direction -> Int -> Direction
turnRight dir deg =
  case divMod deg 90 of
    (n, 0) -> go n dir
    _ -> error "invalid angle"
  where
    go 0 d = d
    go i North = go (i -1) East
    go i East = go (i -1) South
    go i South = go (i -1) West
    go i West = go (i -1) North

run1 :: Input -> Int
run1 input =
  let (x, y, _) = foldl' (\st act -> step act st) (0, 0, East) input
   in abs x + abs y

-- stage2

type St2 = ((Int, Int), (Int, Int))

step2 :: Action -> St2 -> St2
step2 (AMove North n) (shipPos, (x, y)) = (shipPos, (x, y - n))
step2 (AMove South n) (shipPos, (x, y)) = (shipPos, (x, y + n))
step2 (AMove East n) (shipPos, (x, y)) = (shipPos, (x - n, y))
step2 (AMove West n) (shipPos, (x, y)) = (shipPos, (x + n, y))
step2 (ALeft n) (shipPos, wpPos) = (shipPos, rotate (negate n) wpPos)
step2 (ARight n) (shipPos, wpPos) = (shipPos, rotate n wpPos)
step2 (AForward n) ((sx, sy), (wx, wy)) = ((sx + wx * n, sy + wy * n), (wx, wy))

rotate :: Int -> (Int, Int) -> (Int, Int)
rotate deg (x, y) =
  let rad = pi / 180 * fromIntegral deg
      sin' = round (sin rad)
      cos' = round (cos rad)
   in (x * cos' + y * sin', y * cos' - x * sin')

run2 :: Input -> Int
run2 input =
  let ((x, y), _) = foldl' (\st act -> step2 act st) ((0, 0), (-10, -1)) input
   in abs x + abs y

main :: IO ()
main = do
  bs <- BS.readFile "12.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
