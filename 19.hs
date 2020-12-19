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
import qualified Data.Map as Map
import qualified GHC.Show
import Relude
import System.IO (hPutStrLn)

newtype RuleId = RuleId Int
  deriving (Show, Eq, Ord)

data Rule
  = RAlts [[RuleId]]
  | RChar Char
  deriving (Show)

type Message = String

type Input = (Map RuleId Rule, [Message])

parser :: Parser Input
parser =
  (,)
    <$> (Map.fromList <$> many1 (rulePair <* endOfLine)) <* endOfLine
    <*> many1 (many1 letter_ascii <* endOfLine)
  where
    ruleId :: Parser RuleId
    ruleId = RuleId <$> decimal

    rulePair :: Parser (RuleId, Rule)
    rulePair = do
      rid <- ruleId
      _ <- string ": "
      r <- rule
      return (rid, r)

    rule :: Parser Rule
    rule =
      (RChar <$> (char '"' *> letter_ascii <* char '"'))
        <|> (RAlts <$> sepBy (sepBy ruleId (char ' ')) (string " | "))

newtype P a = P (String -> [(a, String)])

runP :: Show a => P a -> String -> Either Text a
runP (P p) toks =
  case p toks of
    (res, []) : xs -> Right res
    rest -> Left $ "parse error:" <> show rest

instance Functor P where
  fmap f (P p) = P $ map (first f) . p

instance Applicative P where
  pure i = P (return . (i,))
  P f <*> P a = P $ \s -> do
    (f', s') <- f s
    (a', s'') <- a s'
    return (f' a', s'')

instance Monad P where
  P a >>= f = P $ \s -> do
    (a', s') <- a s
    let P f' = f a'
    f' s'

instance Alternative P where
  empty = P $ \_ -> []
  P a <|> P b = P $ \s -> a s ++ b s

charP :: Char -> P ()
charP t = P $ \case
  i : xs | i == t -> [((), xs)]
  _ -> []

compile :: Map RuleId Rule -> P ()
compile m =
  let Just r0 = Map.lookup (RuleId 0) m
   in go r0
  where
    go :: Rule -> P ()
    go (RChar c) = charP c
    go (RAlts xs) =
      asum
        ( map
            ( mapM_
                ( \rid ->
                    let Just r = Map.lookup rid m
                     in go r
                )
            )
            xs
        )

run1 :: Input -> Int
run1 (rules, messages) =
  let p = compile rules
   in messages
        & map (runP p)
        & filter isRight
        & length

run2 :: Input -> Int
run2 (rules, messages) =
  let rules' =
        rules
          & Map.insert (RuleId 8) (RAlts [[RuleId 42], [RuleId 42, RuleId 8]])
          & Map.insert (RuleId 11) (RAlts [[RuleId 42, RuleId 31], [RuleId 42, RuleId 11, RuleId 31]])
      p = compile rules'
   in messages
        & map (runP p)
        & filter isRight
        & length

main :: IO ()
main = do
  bs <- BS.readFile "19.txt"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
