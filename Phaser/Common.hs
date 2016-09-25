module Phaser.Common (
  satisfy,
  match,
  char,
  iChar,
  string,
  iString,
  integerDecimal,
  decimal
 ) where

import Data.Char
import Control.Monad
import Control.Applicative

import Phaser.Core

satisfy :: (i -> Bool) -> Phase p i o i
satisfy p = get >>= \c -> if p c then return c else empty

match :: (Eq i) => i -> Phase p i o i
match t = satisfy (== t)

char :: Char -> Phase p Char o Char
char = match

iChar :: Char -> Phase p Char o Char
iChar t = satisfy (\i -> toLower t == toLower i)

string :: Eq i => [i] -> Phase p i o [i]
string = mapM match

iString :: String -> Phase p Char o String
iString = mapM iChar

integerDecimal :: Num a => Phase p Char o a
integerDecimal = go 0 where
  go acc = do
    d <- fmap (fromIntegral . digitToInt) $ satisfy isDigit
    let acc' = acc * 10 + d
    acc' `seq` go acc' <|> return acc'

decimal :: Fractional a => Phase p Char o a
decimal = do
  w <- integerDecimal
  (match '.' >> go True 0.1 w) <|> return w
 where
  go i s acc = do
    let
     p = if i
      then (("At least one digit required after decimal point" :) <??>)
      else id
    d <- p $ fmap (fromIntegral . digitToInt) $ satisfy isDigit
    let acc' = acc + d * s
    acc' `seq` go False (s / 10) acc' <|> return acc'

