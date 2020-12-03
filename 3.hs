{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (count)
import qualified Data.ByteString as BS
import Data.Vector as V
import Relude
import System.IO (hPutStrLn)
import Prelude ((!!))

data Board = Board
  { board :: Vector (Vector Bool),
    rows :: Int,
    cols :: Int
  }
  deriving (Show)

parser :: Parser Board
parser = do
  board <-
    many'
      (("." *> return False) <|> ("#" *> return True))
      <* endOfLine
      <&> V.fromList
      & many'
      <&> V.fromList
  return
    Board
      { board = board,
        rows = V.length board,
        cols = V.length (V.head board)
      }

type Pos = (Int, Int)

isOccupied :: Board -> Pos -> Bool
isOccupied Board {..} (row, col) =
  let col' = col `mod` cols
   in board V.! row V.! col'

step :: Pos -> Pos
step (x, y) = (x + 2, y + 1)

isDone :: Board -> Pos -> Bool
isDone Board {..} (y, _) = y >= rows - 1

run :: Board -> Int
run board = go (0, 0)
  where
    go :: Pos -> Int
    go pos =
      boolToInt (isOccupied board pos)
        + if isDone board pos
          then 0
          else go (step pos)

boolToInt :: Bool -> Int
boolToInt = bool 0 1

main :: IO ()
main = do
  bs <- BS.readFile "3.txt"
  parsed <- case parseOnly (parser <* endOfInput) bs of
    Right r -> return r
    Left err -> do
      hPutStrLn stderr $ "parse failed: " <> show err
      exitFailure
  print (rows parsed, cols parsed)
  let result = run parsed
  print result
