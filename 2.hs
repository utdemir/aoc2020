{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (count)
import qualified Data.ByteString as BS
import Relude
import System.IO (hPutStrLn)
import Prelude ((!!))

type Line = (Int, Int, Char, String)

type Input = [Line]

parser :: Parser Input
parser = many' lineParser

lineParser :: Parser Line
lineParser =
  (,,,)
    <$> decimal <* "-"
    <*> decimal <* " "
    <*> satisfy (inClass "a-z") <* ": "
    <*> many1 (satisfy (inClass "a-z")) <* endOfLine

run :: Input -> Int
run =
  foldl'
    ( \acc (lower, upper, ch, pwd) ->
        let c = count ch pwd
         in (acc +) . boolToInt $
              c >= lower && c <= upper
    )
    0
  where
    count x = length . filter (== x)

run2 :: Input -> Int
run2 =
  foldl'
    ( \acc (pos1, pos2, ch, pwd) ->
        (acc +) . boolToInt $
          (pwd !! (pos1 -1) == ch) `xor` (pwd !! (pos2 -1) == ch)
    )
    0

boolToInt :: Bool -> Int
boolToInt = bool 0 1

main :: IO ()
main = do
  bs <- BS.readFile "2.txt"
  parsed <- case parseOnly (parser <* endOfInput) bs of
    Right r -> return r
    Left err -> do
      hPutStrLn stderr $ "parse failed: " <> show err
      exitFailure
  let result = run2 parsed
  print result
