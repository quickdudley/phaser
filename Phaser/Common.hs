module Phaser.Common (
  Position(..),
  satisfy,
  match,
  char,
  iChar,
  string,
  iString,
  integerDecimal,
  decimal,
  directHex,
  hex,
  integer,
  countChar,
  countLine,
  trackPosition,
  parse
 ) where

import Data.Char
import Control.Monad
import Control.Applicative

import Phaser.Core

-- | A data type for describing a position in a text file. 
data Position = Position
  {-# UNPACK #-}!Int -- ^ Line number
  {-# UNPACK #-}!Int -- ^ Column number

-- | Consume one input, return it if it matches the predicate, otherwise fail.
satisfy :: (i -> Bool) -> Phase p i o i
satisfy p = get >>= \c -> if p c then return c else empty

-- | Consume one input, if it's equal to the parameter then return it, otherwise
-- fail.
match :: (Eq i) => i -> Phase p i o i
match t = satisfy (== t)

-- | 'match' specialized to 'Char'
char :: Char -> Phase p Char o Char
char = match

-- | Case insensitive version of 'char'
iChar :: Char -> Phase p Char o Char
iChar t = satisfy (\i -> toLower t == toLower i)

-- | Match a list of input values
string :: Eq i => [i] -> Phase p i o [i]
string t = go t where
  go [] = return t
  go (a:r) = get >>= \c -> if c == a then go r else empty

-- | Match a string (case insensitive)
iString :: String -> Phase p Char o String
iString = mapM iChar

-- | Take some digits and parse a number
integerDecimal :: Num a => Phase p Char o a
integerDecimal = go 0 where
  go acc = do
    d <- fmap (fromIntegral . digitToInt) $ satisfy isDigit
    let acc' = acc * 10 + d
    acc' `seq` go acc' <|> return acc'

-- | Take some hexadecimal digits and parse a number from hexadecimal
directHex :: Num a => Phase p Char o a
directHex = go 0 where
  go acc = do
    d <- fmap (fromIntegral . digitToInt) $ satisfy isHexDigit
    let acc' = acc * 16 + d
    acc' `seq` go acc' <|> return acc'

-- | Parse a hexadecimal number prefixed with "Ox"
hex :: Num a => Phase p Char o a
hex = string "0x" >> directHex

-- | Parse a number either from decimal digits or from hexadecimal prefixed with
-- "0x"
integer :: Num a => Phase p Char o a
integer = integerDecimal <|> hex

-- | Parse a number from decimal digits and "."
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

-- | Move the position counter one character to the right
countChar :: Phase Position i o ()
{-# INLINE countChar #-}
countChar = count (\(Position r c) -> Position r (c + 1))

-- | Move the position counter to the next line
countLine :: Phase Position i o ()
{-# INLINE countLine #-}
countLine = count (\(Position r _) -> Position (r + 1) 1)

-- | Count the lines and characters from the input before yielding them again
trackPosition :: Phase Position Char Char ()
{-# INLINABLE trackPosition #-}
trackPosition = goR where
  goR = get >>= \c -> yield c >> case c of
    '\n' -> countLine >> goN
    '\r' -> countLine >> goR
    _ -> countChar >> goR
  goN = get >>= \c -> yield c >> case c of
    '\n' -> countLine >> goN
    '\r' -> goR
    _ -> countChar >> goR

-- | Use a 'Phase' as a parser. Note that unlike other parsers the reported
-- position in the input when the parser fails is the position reached when
-- all parsing options are exhausted, not the beginning of the failing token.
-- Since the characters may be counted nondeterministically: if multiple errors
-- are returned the reported error position may be different for each error
-- report.
parse :: Phase Position i o a -> [i] -> Either [(Position,[String])] [a]
parse = parse_ (Position 1 1)

