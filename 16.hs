{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done, takeWhile)
import qualified Data.ByteString as BS
import Data.List (findIndex, findIndices, foldl1, maximum, maximumBy, minimum, minimumBy, tail)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Show
import Relude hiding (Constraint, tail)
import qualified Relude.Unsafe as Unsafe
import System.IO (hPutStrLn)

data Rule = Rule {ruleName :: Text, ruleRanges :: [(Int, Int)]}
  deriving (Show, Eq)

data Ticket = Ticket {unTicket :: [Int]}
  deriving (Show)

data Input = Input
  { iRules :: [Rule],
    iMyTicket :: Ticket,
    iNearbyTickets :: [Ticket]
  }
  deriving (Show)

parser :: Parser Input
parser = do
  rules <- many1 (rule <* endOfLine)
  endOfLine
  _ <- string "your ticket:"
  endOfLine
  myTicket <- ticket <* endOfLine
  endOfLine
  _ <- string "nearby tickets:"
  endOfLine
  nearby <- many1 (ticket <* endOfLine)
  return $ Input rules myTicket nearby
  where
    rule = do
      text <- many' (satisfy $ inClass "a-z ")
      _ <- string ": "
      xs <- sepBy ((,) <$> (decimal <* string "-") <*> decimal) (string " or ")
      return $ Rule (toText text) xs

    ticket = Ticket <$> sepBy decimal (string ",")

compileRule :: Rule -> Int -> Bool
compileRule (Rule _ xs) = go xs
  where
    go [] _ = False
    go ((lo, hi) : xs) i =
      (lo <= i && i <= hi) || go xs i

run1 :: Input -> Int
run1 input =
  let matchesAnyRule :: Int -> Bool =
        iRules input
          & map compileRule
          & \xs i -> any ($i) xs
   in iNearbyTickets input
        & concatMap unTicket
        & filter (not . matchesAnyRule)
        & sum

-- Part 2

run2 :: Input -> _
run2 input =
  let matchesAnyRule :: Int -> Bool =
        iRules input
          & map compileRule
          & \xs i -> any ($i) xs
      validTickets :: [Ticket] =
        iNearbyTickets input
          & filter (all matchesAnyRule . unTicket)
      columns =
        validTickets
          & map unTicket
          & transpose
      possibleRules :: [[Rule]] =
        columns
          & map
            ( \c ->
                filter (\r -> all (compileRule r) c) (iRules input)
            )
   in possibleRules
        & map (map ruleName)
        & solve
        & fromJust
        & findIndices ("departure" `T.isPrefixOf`)
        & map (\i -> unTicket (iMyTicket input) Unsafe.!! i)
        & product

solve :: Eq a => [[a]] -> Maybe [a]
solve = go []
  where
    go :: Eq a => [a] -> [[a]] -> Maybe [a]
    go solved [] = Just $ reverse solved
    go solved ([] : unsolved) = Nothing
    go solved ([x] : unsolved) = go (x : solved) (map (filter (/= x)) unsolved)
    go solved ((x : rest) : unsolved) =
      go (x : solved) (map (filter (/= x)) unsolved)
        <|> go solved (rest : unsolved)

main :: IO ()
main = do
  bs <- BS.readFile "16.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
