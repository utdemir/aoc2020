{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done, takeWhile)
import qualified Data.ByteString as BS
import qualified GHC.Show
import Relude
import System.IO (hPutStrLn)

data Token
  = Plus
  | Times
  | Literal Integer
  | OpenParen
  | CloseParen
  deriving (Show, Eq)

parser :: Parser [[Token]]
parser = many1 (many1 (tok <* many' (char ' ')) <* endOfLine)
  where
    tok =
      (decimal <&> Literal)
        <|> (char '+' $> Plus)
        <|> (char '*' $> Times)
        <|> (char '(' $> OpenParen)
        <|> (char ')' $> CloseParen)

data Ops = Add | Mul
  deriving (Show)

data Expr
  = Num Integer
  | BinOp Ops Expr Expr
  | Group Expr

instance Show Expr where
  show (Num i) = show i
  show (Group e) = "( " <> show e <> " )"
  show (BinOp op lhs rhs) =
    ("[" <> show lhs <> " " <> (case op of Add -> "+"; Mul -> "*") <> " " <> show rhs <> "]")

mkExpr1 :: [Token] -> Expr
mkExpr1 ts = case parseExpr Nothing ts of
  (expr, []) -> expr
  (_, leftovers) -> error $ "leftovers: " <> show leftovers
  where
    parseArg :: [Token] -> (Expr, [Token])
    parseArg (Literal i : xs) =
      (Num i, xs)
    parseArg (OpenParen : xs) =
      case parseExpr Nothing xs of
        (expr, CloseParen : rest) -> (Group expr, rest)
        (_, rest) -> error $ "expected close paren, but got:" <> show rest
    parseArg xs =
      error $ "not an argument: " <> show xs

    parseExpr :: Maybe Expr -> [Token] -> (Expr, [Token])
    parseExpr Nothing xs =
      case parseArg xs of
        (expr, rest) -> parseExpr (Just expr) rest
    parseExpr (Just lhs) (Plus : xs) =
      case parseArg xs of
        (rhs, rest) -> parseExpr (Just $ BinOp Add lhs rhs) rest
    parseExpr (Just lhs) (Times : xs) =
      case parseArg xs of
        (rhs, rest) -> parseExpr (Just $ BinOp Mul lhs rhs) rest
    parseExpr (Just expr) xs = (expr, xs)

evalExpr :: Expr -> Integer
evalExpr (Num i) = i
evalExpr (Group e) = evalExpr e
evalExpr (BinOp op lhs rhs) =
  (case op of Add -> (+); Mul -> (*))
    (evalExpr lhs)
    (evalExpr rhs)

run1 :: [[Token]] -> Integer
run1 toks =
  toks
    & map mkExpr1
    & map evalExpr
    & sum

-- part 2

pattern PAdd, PMul :: Expr -> Expr -> Expr
pattern PAdd lhs rhs = BinOp Add lhs rhs
pattern PMul lhs rhs = BinOp Mul lhs rhs

shakePrecedences :: Expr -> Expr
shakePrecedences = \case
  Num a ->
    Num a
  Group e ->
    Group (go e)
  a `PAdd` b ->
    case go a of
      a' `PMul` a'' ->
        go a' `PMul` (go $ a'' `PAdd` go b)
      a' -> go a' `PAdd` go b
  a `PMul` b ->
    go a `PMul` go b
  where
    go = shakePrecedences

run2 :: [[Token]] -> Integer
run2 toks =
  toks
    & map mkExpr1
    & map shakePrecedences
    & map evalExpr
    & sum

main :: IO ()
main = do
  bs <- BS.readFile "18.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
