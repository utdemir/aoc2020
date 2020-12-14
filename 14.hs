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
import qualified GHC.Show
import Relude hiding (Constraint, tail)
import System.IO (hPutStrLn)

data MaskVal = Mask1 | Mask0 | MaskX
  deriving (Show)

newtype Mask = Mask [MaskVal]
  deriving (Show)

data Instr
  = InstrMask Mask
  | InstrSet Integer Integer
  deriving (Show)

type Input = [Instr]

parser :: Parser Input
parser = many1 $ (mask <|> set) <* endOfLine
  where
    mask =
      InstrMask . Mask . reverse
        <$> ( string "mask = "
                *> count
                  36
                  ( (char 'X' $> MaskX)
                      <|> (char '1' $> Mask1)
                      <|> (char '0' $> Mask0)
                  )
            )
    set =
      InstrSet
        <$> (string "mem[" *> decimal <* "] = ")
        <*> decimal

intToBits :: Integer -> [Bool]
intToBits 0 = []
intToBits n = (mod n 2 == 1) : intToBits (div n 2)

bitsToInt :: [Bool] -> Integer
bitsToInt [] = 0
bitsToInt (x : xs) = (if x then 1 else 0) + 2 * bitsToInt xs

mask1 :: Mask -> Integer -> Integer
mask1 (Mask m) v =
  bitsToInt $
    zipWith
      ( \case
          MaskX -> id
          Mask1 -> const True
          Mask0 -> const False
      )
      m
      (intToBits v ++ repeat False)

run1 :: Input -> Integer
run1 input =
  input
    & foldl' go (undefined, Map.empty)
    & snd
    & Map.elems
    & sum
  where
    go (currMask, mem) instr =
      case instr of
        InstrMask newMask -> (newMask, mem)
        InstrSet addr val ->
          ( currMask,
            Map.insert addr (mask1 currMask val) mem
          )

run2 :: Input -> Integer
run2 input =
  input
    & foldl' go (undefined, Map.empty)
    & snd
    & Map.elems
    & sum
  where
    go (currMask, mem) instr =
      case instr of
        InstrMask newMask -> (newMask, mem)
        InstrSet addr val ->
          ( currMask,
            mask2 currMask addr
              & foldl'
                (\m a -> Map.insert a val m)
                mem
          )

mask2 :: Mask -> Integer -> [Integer]
mask2 (Mask m) v = go m (intToBits v ++ repeat False) & map bitsToInt
  where
    go [] _ = [[]]
    go (Mask1 : xs) (_ : bs) = (True :) <$> go xs bs
    go (Mask0 : xs) (b : bs) = (b :) <$> go xs bs
    go (MaskX : xs) (_ : bs) =
      let rest = go xs bs
       in map (False :) rest ++ map (True :) rest

main :: IO ()
main = do
  bs <- BS.readFile "14.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
