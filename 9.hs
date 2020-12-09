{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done)
import qualified Data.ByteString as BS
import Data.List (findIndices, foldl1, maximum, minimum)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Relude hiding (Constraint)
import System.IO (hPutStrLn)

data Queue a = Queue [a] [a]

queuePush :: a -> Queue a -> Queue a
queuePush x (Queue f b) = Queue f (x:b)

queuePop :: Queue a -> Queue a
queuePop (Queue (_:xs) b) = Queue xs b
queuePop (Queue [] b) = queuePop (Queue (reverse b) [])

queueToList :: Queue a -> [a]
queueToList (Queue a b) = a ++ reverse b

queueFromList :: [a] -> Queue a
queueFromList xs = Queue xs []

parser :: Parser [Int]
parser = many1 (decimal <* endOfLine)

preambleSize :: Int
preambleSize = 25

checkPreamble :: [Int] -> Int -> Bool
checkPreamble xs x =
  xs
    & pairs
    & find (\(i, j) -> i+j == x)
    & isJust
  where
    pairs xs = do
      let ixs = zip @Int [0..] xs
      (ix, i) <- ixs
      (jx, j) <- ixs
      guard $ ix /= jx
      return (i, j)

run1 :: [Int] -> Int
run1 xs =
  let (pr, rest) = splitAt preambleSize xs
   in go (queueFromList pr) rest
  where
    go pr [] = error "end"
    go pr (x:xs) =
      if checkPreamble (queueToList pr) x
      then go (pr & queuePop & queuePush x) xs
      else x

run2 :: [Int] -> Int -> Int
run2 xs target = do
  case takeWhileTo target xs of
    Just xs -> case nonEmpty xs of
      Just xs' -> maximum xs' + minimum xs'
    Nothing ->
      let (_:rest) = xs
      in  run2 rest target
  where
    takeWhileTo :: Int -> [Int] -> Maybe [Int]
    takeWhileTo 0 _ = Just []
    takeWhileTo _ [] = Nothing
    takeWhileTo i (x:xs)
     | i < x = Nothing
     | otherwise =
       let r = i - x
        in case takeWhileTo r xs of
             Nothing -> Nothing
             Just xs' -> Just $ x:xs'

main :: IO ()
main = do
  bs <- BS.readFile "9.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  let r1 = run1 parsed
  print r1
  print (run2 parsed r1)
