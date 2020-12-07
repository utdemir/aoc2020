{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.List (foldl1)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Relude hiding (Constraint)
import System.IO (hPutStrLn)

newtype Bag = Bag Text
  deriving (Show, Eq, Ord)

data Content = Content Bag [(Int, Bag)]
  deriving (Show)

target :: Bag
target = Bag "shiny gold"

parser :: Parser [Content]
parser = many1 (content <* endOfLine)
  where
    color :: Parser Text
    color =
      (\i j k -> toText i <> toText [j] <> toText k)
        <$> many1 (satisfy $ inClass "a-z")
        <*> space
        <*> many1 (satisfy $ inClass "a-z")

    bag :: Parser Bag
    bag = Bag <$> (color <* space <* ("bags" <|> "bag"))

    numberedBag :: Parser (Int, Bag)
    numberedBag = (,) <$> (decimal <* space) <*> bag

    content :: Parser Content
    content = do
      b <- bag
      _ <- space <* "contain" <* space
      cs <-
        sepBy1 numberedBag ", "
          <|> ("no other bags" $> [])
      _ <- "."
      return $ Content b cs

run1 :: [Content] -> Int
run1 contents =
  let includes = Map.fromList [(x, map snd ys) | Content x ys <- contents]
      colors = Map.keys includes
      includesTarget color =
        let contents = fromJust $ Map.lookup color includes
         in or ((target `elem` contents) : map includesTarget contents)
   in length $ filter includesTarget colors

run2 :: [Content] -> Int
run2 contents =
  let includes = Map.fromList [(x, ys) | Content x ys <- contents]
      countBags color =
        let contents = fromJust $ Map.lookup color includes
         in 1 + sum [c * countBags b | (c, b) <- contents]
   in countBags target - 1

main :: IO ()
main = do
  bs <- BS.readFile "7.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
