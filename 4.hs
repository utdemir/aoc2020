{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Relude
import System.IO (hPutStrLn)

type Input = [[(String, String)]]

{-
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
-}
requiredFields :: [(String, String -> Bool)]
requiredFields =
  [ ("byr", \s -> length s == 4 && s >= "1920" && s <= "2002"),
    ("iyr", \s -> length s == 4 && s >= "2010" && s <= "2020"),
    ("eyr", \s -> length s == 4 && s >= "2020" && s <= "2030"),
    ( "hgt",
      \s ->
        if "cm" `isSuffixOf` s
          then s >= "150cm" && s <= "193cm"
          else
            if "in" `isSuffixOf` s
              then s >= "59in" && s <= "76in"
              else False
    ),
    ( "hcl",
      \case
        '#' : a : b : c : d : e : f : [] -> all (`elem` "0123456789abcdef") [a, b, c, d, e, f]
        _ -> False
    ),
    ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
    ("pid", \s -> length s == 9 && all isDigit s)
  ]

parser :: Parser Input
parser = do
  sepBy1 passport (endOfLine >> endOfLine)
  where
    pair :: Parser (String, String)
    pair = do
      k <- A.count 3 letter_ascii
      _ <- char ':'
      v <- many1 (satisfy (not . isSpace))
      return (k, v)

    passport :: Parser [(String, String)]
    passport = sepBy1 pair (void (char ' ') <|> endOfLine)

isValidWeak :: [(String, String)] -> Bool
isValidWeak actual' =
  let actual = map fst actual'
   in all (`elem` actual) (map fst requiredFields)

isValidStrong :: [(String, String)] -> Bool
isValidStrong actual =
  flip all requiredFields $ \(field, predicate) ->
    case find ((== field) . fst) actual of
      Nothing -> False
      Just (_, v) -> predicate v

run :: Input -> Int
run = length . filter isValidStrong

main :: IO ()
main = do
  bs <- BS.readFile "4.txt"
  parsed <- case parseOnly (parser) bs of
    Right r -> return r
    Left err -> do
      hPutStrLn stderr $ "parse failed: " <> show err
      exitFailure
  print $ run parsed

-- let result = run parsed
-- print result
