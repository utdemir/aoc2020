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
import Data.List (findIndices, foldl1, maximum, maximumBy, minimum, minimumBy, tail)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Relude hiding (Constraint, tail)
import System.IO (hPutStrLn)

type Input = (Integer, [Maybe Integer])

parser :: Parser Input
parser = do
  (,)
    <$> decimal <* endOfLine
    <*> sepBy ((Just <$> decimal) <|> (char 'x' $> Nothing)) (char ',') <* endOfLine

run1 :: Input -> Integer
run1 (now, ids') =
  let ids = catMaybes ids'
      diffs = map (\i -> (i, id2diff i)) ids
      (m_id, m_diff) = minimumBy (comparing snd) diffs
   in m_id * m_diff
  where
    id2diff i = case now `mod` i of
      0 -> 0
      n -> i - n

run2 :: Input -> Integer
run2 (_, ids') =
  let n : ns =
        traceShowId $
          zip [(0 :: Integer) ..] ids'
            & mapMaybe (\(a, n) -> (a,) <$> n)
            & map (\(a, n) -> (n - (a `mod` n), n))
            & map (\(a, n) -> (a `mod` n, n))
            & sortOn (Down . snd)
   in foldl'
        ( \(a1, n1) (a2, n2) ->
            let r =
                  iterate (+ n1) a1
                    & find (\i -> i `mod` n2 == a2)
                    & fromJust
             in traceShow ((a1, n1), (a2, n2), r) $
                  (r, n1 * n2)
        )
        n
        ns
        & traceShowId
        & fst

main :: IO ()
main = do
  bs <- BS.readFile "13.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
