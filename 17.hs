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
import qualified Relude.Unsafe as Unsafe
import System.IO (hPutStrLn)

type D2 = (Int, Int)

type D3 = (Int, Int, Int)

type D4 = (Int, Int, Int, Int)

newtype Board a = Board
  { activeCells :: Set a
  }
  deriving (Eq, Show)

isActive :: Ord d => Board d -> d -> Bool
isActive (Board b) p = p `Set.member` b

parser :: Parser (Board D2)
parser =
  many1 (many1 ((char '.' $> False) <|> (char '#' $> True)) <* endOfLine)
    <&> map (zip [0 ..])
    <&> zip [0 ..]
    <&> concatMap (\(r, xs) -> mapMaybe (\(c, b) -> if b then Just (r, c) else Nothing) xs)
    <&> Set.fromList
    <&> Board

nextCell :: Bool -> Int -> Bool
nextCell True 2 = True
nextCell _ 3 = True
nextCell _ _ = False

neighboursD3 :: D3 -> Set D3
neighboursD3 (x, y, z) = Set.fromList $ do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  dz <- [-1 .. 1]
  guard $ (dx, dy, dz) /= (0, 0, 0)
  return ((x + dx, y + dy, z + dz))

neighboursD4 :: D4 -> Set D4
neighboursD4 (x, y, z, t) = Set.fromList $ do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  dz <- [-1 .. 1]
  dt <- [-1 .. 1]
  guard $ (dx, dy, dz, dt) /= (0, 0, 0, 0)
  return ((x + dx, y + dy, z + dz, t + dt))

occupiedNeighboursD3 :: Board D3 -> D3 -> Int
occupiedNeighboursD3 board pos =
  sum . map locToInt . Set.toList $ neighboursD3 pos
  where
    locToInt pos = bool 0 1 (isActive board pos)

occupiedNeighboursD4 :: Board D4 -> D4 -> Int
occupiedNeighboursD4 board pos =
  sum . map locToInt . Set.toList $ neighboursD4 pos
  where
    locToInt pos = bool 0 1 (isActive board pos)

stepD3 :: Board D3 -> Board D3
stepD3 board =
  let relevant = activeCells board <> mconcat (map neighboursD3 $ Set.toList (activeCells board))
   in relevant
        & Set.filter
          ( \pos ->
              nextCell
                (isActive board pos)
                (occupiedNeighboursD3 board pos)
          )
        & Board

stepD4 :: Board D4 -> Board D4
stepD4 board =
  let relevant = activeCells board <> mconcat (map neighboursD4 $ Set.toList (activeCells board))
   in relevant
        & Set.filter
          ( \pos ->
              nextCell
                (isActive board pos)
                (occupiedNeighboursD4 board pos)
          )
        & Board

run1 :: Board D2 -> Int
run1 board =
  Board (activeCells board & Set.map (\(x, y) -> (x, y, 0)))
    & iterate stepD3
    & (Unsafe.!! 6)
    & activeCells
    & Set.size

run2 :: Board D2 -> Int
run2 board =
  Board (activeCells board & Set.map (\(x, y) -> (x, y, 0, 0)))
    & iterate stepD4
    & (Unsafe.!! 6)
    & activeCells
    & Set.size

main :: IO ()
main = do
  bs <- BS.readFile "17.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print parsed
  print $ run1 parsed
  print $ run2 parsed
