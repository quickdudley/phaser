{-|
Module:     Codec.Parser.Common
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

Common functions which do not need to be in 'Phaser.Core', mostly for using
'Phase's and 'Automaton's as parsers.
-}
module Codec.Phaser.Common (
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
  parse,
  sepBy,
  munch,
  munch1,
  parseFile,
  latin1
 ) where

import Data.Char
import Data.Word
import Control.Monad
import Control.Applicative

import Codec.Phaser.Core
import qualified Codec.Phaser.ByteString as BP

-- | A data type for describing a position in a text file. Constructor arguments
-- are row number and column number.
data Position = Position
  {-# UNPACK #-}!Int
  {-# UNPACK #-}!Int
 deriving (Eq,Ord)

instance Show Position where
  showsPrec p (Position r c) = b m where
    b a = if p > 0
      then ('(' :) . a . (')' :)
      else a
    m = ("Row " ++) . showsPrec 0 r . (", Column " ++) . showsPrec 0 c

instance Read Position where
  readsPrec p = toReadS (toAutomaton (go p)) where
    parenthes a = surround a
      (many (satisfy isSpace) >> char '(')
      (char ')' >> many (satisfy isSpace))
    go 0 = inner <|> parenthes (go 0)
    go _ = parenthes (go 0)
    inner = do
      many (satisfy isSpace)
      iString "row"
      some (satisfy isSpace)
      r <- integer
      many (satisfy isSpace)
      char ','
      many (satisfy isSpace)
      iString "column"
      some (satisfy isSpace)
      c <- integer
      return (Position r c)

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
      then ("At least one digit required after decimal point" <?>)
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

-- | Count the lines and characters from the input before yielding them again.
-- If the phase pipeline does not include this or similar: parsing errors will
-- not report the correct position.
trackPosition :: Phase Position Char Char ()
{-# INLINABLE trackPosition #-}
trackPosition = goR where
  goR = flip (<|>) (return ()) $ get >>= \c -> yield c >> case c of
    '\r' -> countLine >> goN
    '\n' -> countLine >> goR
    _ -> countChar >> goR
  goN = flip (<|>) (return ()) $ get >>= \c -> yield c >> case c of
    '\r' -> countLine >> goN
    '\n' -> goR
    _ -> countChar >> goR

-- | Use a 'Phase' as a parser. Note that unlike other parsers the reported
-- position in the input when the parser fails is the position reached when
-- all parsing options are exhausted, not the beginning of the failing token.
-- Since the characters may be counted nondeterministically: if multiple errors
-- are returned the reported error position may be different for each error
-- report.
parse :: Phase Position i o a -> [i] -> Either [(Position,[String])] [a]
parse = parse_ (Position 1 1)

-- | sepBy p sep parses zero or more occurrences of p, separated by sep. Returns
-- a list of values returned by p.
sepBy :: Phase p i o a -> Phase p i o s -> Phase p i o [a]
sepBy p sep = go id <|> return [] where
  go acc = do
    a <- p
    let acc' = acc . (a :)
    (sep >> go acc') <|> return (acc' [])

surround :: Phase p i o a -> Phase p i o b -> Phase p i o e -> Phase p i o a
surround m o c = (\_ r _ -> r) <$> o <*> m <*> c

-- | Parses the first zero or more values satisfying the predicate. Always
-- succeds, exactly once, having consumed all the characters Hence NOT the same
-- as (many (satisfy p))
munch :: (i -> Bool) -> Phase p i o [i]
munch p = munch1 p <|> (eof >> return [])

-- | Parses the first one or more values satisfying the predicate. Always
-- succeds, exactly once, having consumed all the characters Hence NOT the same
-- as (some (satisfy p))
munch1 :: (i -> Bool) -> Phase p i o [i]
munch1 p = go id where
  go acc = do
    c <- get
    if p c
      then go (acc . (c :)) <|> (eof >> return (acc [c]))
      else put1 c >> return (acc [])

-- | Run a parser on input from a file. Input is provided as bytes, if
-- characters are needed: a decoding phase such as
-- 'Codec.Phaser.UTF8.utf8_stream' or 'latin1' may be used
parseFile :: Phase Position Word8 o a -> FilePath ->
  IO (Either [(Position,[String])] [a])
parseFile = BP.parseFile_ (Position 1 1)

-- | Decode bytes to characters using the Latin1 (ISO8859-1) encoding
latin1 :: Phase p Word8 Char ()
latin1 = go where
  go = flip (<|>) (return ()) $ 
    fmap (toEnum . fromIntegral) get >>= yield >> go
    

