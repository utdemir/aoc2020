{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.List (foldl1)
import qualified Data.Set as Set
import Relude
import System.IO (hPutStrLn)

newtype Question = Question Char
  deriving (Show, Eq, Ord)

newtype Person = Person {answers :: [Question]}
  deriving (Show)

newtype Group = Group {members :: [Person]}
  deriving (Show)

parser :: Parser [Group]
parser = sepBy1 group endOfLine
  where
    question :: Parser Question
    question = Question <$> satisfy (inClass "a-z")

    person :: Parser Person
    person = Person <$> many1 question <* endOfLine

    group :: Parser Group
    group = Group <$> many1 person

run1 :: [Group] -> Int
run1 groups =
  groups
    & map (length . groupAnswers)
    & sum
  where
    groupAnswers :: Group -> [Question]
    groupAnswers xs =
      xs
        & members
        & concatMap answers
        & ordNub

run2 :: [Group] -> Int
run2 groups =
  groups
    & map (length . groupAnswers)
    & sum
  where
    groupAnswers :: Group -> [Question]
    groupAnswers xs =
      xs
        & members
        & map (Set.fromList . answers)
        & foldl1 Set.intersection
        & Set.toList

main :: IO ()
main = do
  bs <- BS.readFile "6.txt"
  parsed <- case parseOnly (parser <* endOfInput) bs of
    Right r -> return r
    Left err -> do
      hPutStrLn stderr $ "parse failed: " <> show err
      exitFailure
  print parsed
  print $ run1 parsed
  print $ run2 parsed
