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

type Pos = (Int, Int)

data Seat = Empty | Occupied
  deriving (Eq, Show)

data Board = Board
  { boardSeats :: Map Pos Seat,
    rows :: Int,
    cols :: Int
  }
  deriving (Eq)

getSeat :: Board -> Pos -> Maybe Seat
getSeat (Board m _ _) pos = Map.lookup pos m

mapBoard :: ((Pos, Seat) -> Seat) -> Board -> Board
mapBoard f (Board b r c) =
  Board
    (Map.mapWithKey (\p s -> f (p, s)) b)
    r
    c

parser :: Parser Board
parser =
  many1 (many1 seat <* endOfLine)
    <&> map (zip [0 ..])
    <&> zip [0 ..]
    <&> concatMap (\(r, xs) -> map (\(c, s) -> ((r, c), s)) xs)
    <&> mkBoard
  where
    seat :: Parser (Maybe Seat)
    seat =
      ( (char '.' $> Nothing)
          <|> (char 'L' $> Just Empty)
          <|> (char '#' $> Just Occupied)
      )
    mkBoard elems =
      Board
        (elems & mapMaybe (\(pos, s) -> Map.singleton pos <$> s) & mconcat)
        (elems & map (fst . fst) & maximum)
        (elems & map (snd . fst) & maximum)

occupiedNeighbours :: Board -> Pos -> Int
occupiedNeighbours board (r, c) =
  sum . map locToInt $ neighbours
  where
    neighbours = do
      dr <- [-1 .. 1]
      dc <- [-1 .. 1]
      guard $ (dr, dc) /= (0, 0)
      return ((r + dr, c + dc))
    locToInt pos = case getSeat board pos of
      Just Occupied -> 1
      _ -> 0

nextSeat :: Seat -> Int -> Seat
nextSeat Empty 0 = Occupied
nextSeat Occupied i | i >= 4 = Empty
nextSeat s _ = s

nextBoard :: Board -> Board
nextBoard board =
  board
    & mapBoard
      (\(pos, old) -> nextSeat old (occupiedNeighbours board pos))

run1 :: Board -> Int
run1 board =
  let next = nextBoard board
   in if board == next
        then
          boardSeats board
            & Map.elems
            & filter (== Occupied)
            & length
        else run1 next

-- Step 2

occupiedNeighbours2 :: Board -> Pos -> Int
occupiedNeighbours2 board pos =
  directions
    & map isOccupied
    & sum
  where
    directions = do
      dr <- [-1 .. 1]
      dc <- [-1 .. 1]
      let dir = (dr, dc)
      guard $ dir /= (0, 0)
      return dir

    isOccupied dir =
      move dir
        & mapMaybe (getSeat board)
        & \case
          Occupied : _ -> 1
          _ -> 0

    move (dr, dc) =
      pos
        & iterate (\(r, c) -> (r + dr, c + dc))
        & drop 1
        & takeWhile (\(r, c) -> r >= 0 && r <= rows board && c >= 0 && c <= cols board)

nextSeat2 :: Seat -> Int -> Seat
nextSeat2 Empty 0 = Occupied
nextSeat2 Occupied i | i >= 5 = Empty
nextSeat2 s _ = s

nextBoard2 :: Board -> Board
nextBoard2 board =
  board
    & mapBoard
      (\(pos, old) -> nextSeat2 old (occupiedNeighbours2 board pos))

run2 :: Board -> Int
run2 board =
  let next = nextBoard2 board
   in if board == next
        then
          boardSeats board
            & Map.elems
            & filter (== Occupied)
            & length
        else run2 next

main :: IO ()
main = do
  bs <- BS.readFile "11.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
