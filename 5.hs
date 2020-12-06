{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Relude
import System.IO (hPutStrLn)
import Prelude (maximum, minimum)

data Half = Lower | Upper

type Pos = ([Half], [Half])

parser :: Parser [Pos]
parser = many1 $ posParser <* endOfLine
  where
    posParser :: Parser Pos
    posParser =
      (,)
        <$> count 7 ((char 'F' $> Lower) <|> (char 'B' $> Upper))
        <*> count 3 ((char 'L' $> Lower) <|> (char 'R' $> Upper))

run1 :: [Pos] -> Int
run1 passengers =
  passengers
    & map (\(r, c) -> (fst $ locate r, fst $ locate c))
    & map toId
    & maximum

run2 :: [Pos] -> [Int]
run2 passengers =
  let ids =
        passengers
          & map (\(r, c) -> (fst $ locate r, fst $ locate c))
          & map toId
      l = minimum ids
      h = maximum ids
   in filter (not . (`elem` ids)) [l .. h]

locate :: [Half] -> (Int, Int)
locate [] = (0, 1)
locate (x : xs) =
  let (pos, range) = locate xs
   in ( case x of
          Lower -> pos
          Upper -> range + pos,
        range * 2
      )

toId :: (Int, Int) -> Int
toId (r, c) = r * 8 + c

main :: IO ()
main = do
  bs <- BS.readFile "5.txt"
  parsed <- case parseOnly (parser <* endOfInput) bs of
    Right r -> return r
    Left err -> do
      hPutStrLn stderr $ "parse failed: " <> show err
      exitFailure
  print $ run1 parsed
  print $ run2 parsed
