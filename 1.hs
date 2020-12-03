{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.Maybe
import Relude

type Input = [Int]

parser :: Parser Input
parser = do
  i <- sepBy decimal endOfLine
  return i

run :: Input -> Int
run input = fromJust . viaNonEmpty head $ do
  i <- input
  j <- input
  k <- input
  guard (i + j + k == 2020)
  return (i * j * k)

main :: IO ()
main = do
  bs <- decodeUtf8 @Text <$> BS.readFile "1.txt"
  let parsed = map (fromJust . readMaybe @Int . toString) $ lines bs
      result = run parsed
  print result
