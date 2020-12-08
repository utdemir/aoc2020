{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done)
import qualified Data.ByteString as BS
import Data.List (findIndices, foldl1)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Relude hiding (Constraint)
import System.IO (hPutStrLn)

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show, Eq)

type Program = V.Vector Instr

parser :: Parser Program
parser = V.fromList <$> many1 (instr <* endOfLine)
  where
    instr :: Parser Instr
    instr = acc <|> jmp <|> nop

    acc :: Parser Instr
    acc = Acc <$> ("acc " *> offset)
    jmp :: Parser Instr
    jmp = Jmp <$> ("jmp " *> offset)
    nop :: Parser Instr
    nop = Nop <$> ("nop " *> offset)

    offset :: Parser Int
    offset = do
      isNeg <- ("+" $> False) <|> ("-" $> True)
      num <- decimal
      return $ if isNeg then negate num else num

data St = St (Vector Instr) Int Int (Set Int)

data R
  = Loop St
  | Done St

initSt :: Program -> St
initSt xs = St xs 0 0 mempty

run1 :: Program -> Int
run1 pr =
  case eval pr of
    Loop (St _ _ acc _) -> acc

run2 :: Program -> Int
run2 pr =
  pr
    & V.findIndices
      ( \case
          Nop _ -> True
          Jmp _ -> True
          _ -> False
      )
    & toList
    & map (\i -> vecModify i flip pr)
    & mapMaybe
      ( \pr -> case eval pr of
          Loop _ -> Nothing
          Done (St _ _ acc _) -> Just acc
      )
    & viaNonEmpty head
    & fromJust
  where
    flip (Nop i) = Jmp i
    flip (Jmp i) = Nop i
    vecModify i f v =
      let old = v V.! i
          new = f old
       in v V.// [(i, new)]

eval :: Program -> R
eval pr = go (initSt pr)
  where
    go :: St -> R
    go st@(St program pc acc visited)
      | Set.member pc visited = Loop st
      | pc == V.length program = Done st
      | otherwise = case program V.! pc of
        Nop _ -> go $ St program (succ pc) acc (Set.insert pc visited)
        Acc i -> go $ St program (succ pc) (acc + i) (Set.insert pc visited)
        Jmp i -> go $ St program (pc + i) acc (Set.insert pc visited)

main :: IO ()
main = do
  bs <- BS.readFile "8.txt"
  parsed <-
    case parseOnly (parser) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
