{-|
Module:     Codec.Parser.Common
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

Common functions which do not need to be in 'Phaser.Core', mostly for using
'Phase's and 'Automaton's as parsers.
-}
{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts #-}
module Codec.Phaser.Common (
  Position(..),
  PhaserType(..),
  Standardized(..),
  Trie,
  newTrie,
  fromTrie,
  satisfy,
  match,
  char,
  iChar,
  string,
  iString,
  (<#>),
  integerDecimal,
  positiveIntegerDecimal,
  decimal,
  scientificNotation,
  directHex,
  hex,
  positiveInteger,
  integer,
  countChar,
  countLine,
  trackPosition,
  normalizeNewlines,
  parse,
  sepBy,
  sepBy1,
  munch,
  munch1,
  parseFile,
  parseHandle,
  latin1
 ) where

import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import Data.Ratio
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import System.IO (Handle)

import Codec.Phaser.Core
import qualified Codec.Phaser.ByteString as BP

-- | Class for types which have standardized or otherwise unambiguous
-- representations. Implementations of 'regular' may be more permissive than
-- the corresponding 'Read' instance (if any).
class Standardized r a where
  regular :: Monoid p => Phase p r o a

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
    go :: Int -> Phase () Char o Position
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

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position r1 c1) (Position r2 c2)
    | r2 == 0   = Position r1 (c1 + c2)
    | otherwise = Position (r1 + r2) c2

-- | Tries in this module can be used for creating more efficient parsers
-- when several of the recognized strings begin with the same few characters
data Trie c a = Trie [a] (M.Map c (Trie c a))

instance Ord c => Monoid (Trie c a) where
  mempty = Trie [] M.empty
  mappend ~(Trie l1 m1) ~(Trie l2 m2) =
    Trie (l1 ++ l2) (M.unionWith mappend m1 m2)

-- | Consume one input, return it if it matches the predicate, otherwise fail.
satisfy :: (Monoid p) => (i -> Bool) -> Phase p i o i
satisfy p = get >>= \c -> if p c then return c else empty

-- | Consume one input, if it's equal to the parameter then return it, otherwise
-- fail.
match :: (Eq i, Monoid p) => i -> Phase p i o i
match t = satisfy (== t)

-- | 'match' specialized to 'Char'
char :: (Monoid p) => Char -> Phase p Char o Char
char = match

-- | Case insensitive version of 'char'
iChar :: (Monoid p) => Char -> Phase p Char o Char
iChar t = satisfy (\i -> toLower t == toLower i)

-- | Match a list of input values
string :: (Eq i, Monoid p) => [i] -> Phase p i o [i]
string t = go t where
  go [] = return t
  go (a:r) = get >>= \c -> if c == a then go r else empty

-- | Match a string (case insensitive)
iString :: (Monoid p) => String -> Phase p Char o String
iString = mapM iChar

infixl 5 <#>
(<#>) :: (PhaserType d, PhaserType s, Monoid p) =>
     s p b c (a -> z) -> d p c t a -> Automaton p b t z
(<#>) = chainWith ($)

-- | Parse a standard positive base 10 integer
positiveIntegerDecimal :: (Num a, Monoid p) => Phase p Char o a
positiveIntegerDecimal = go 0 where
  go acc = do
    d <- fmap (fromIntegral . digitToInt) $ satisfy isDigit
    let acc' = acc * 10 + d
    acc' `seq` go acc' <|> return acc'

-- | Parse a standard base 10 integer
integerDecimal :: (Num a, Monoid p) => Phase p Char o a
integerDecimal = (pure id <|> (char '-' *> munch isSpace *> pure negate)) <*>
  positiveIntegerDecimal

-- | Take some hexadecimal digits and parse a number from hexadecimal
directHex :: (Num a, Monoid p) => Phase p Char o a
directHex = go 0 where
  go acc = do
    d <- fmap (fromIntegral . digitToInt) $ satisfy isHexDigit
    let acc' = acc * 16 + d
    acc' `seq` go acc' <|> return acc'

-- | Parse a hexadecimal number prefixed with "Ox"
hex :: (Num a, Monoid p) => Phase p Char o a
hex = string "0x" >> directHex

-- | Parse a positive integer from either decimal or hexadecimal format
positiveInteger :: (Num a, Monoid p) => Phase p Char o a
positiveInteger = positiveIntegerDecimal <|> hex

-- | Parse a number either from decimal digits or from hexadecimal prefixed with
-- "0x"
integer :: (Num a, Monoid p) => Phase p Char o a
integer = integerDecimal <|> hex

-- | Parse a number from decimal digits, "-", and "."
decimal :: (Fractional a, Monoid p) => Phase p Char o a
decimal = (pure id <|> (negate <$ char '-' <* munch isSpace)) <*>
  positiveDecimal

-- | Parse a positive number from decimal digits and "."
positiveDecimal :: (Fractional a, Monoid p) => Phase p Char o a
positiveDecimal = fromRational <$> do
  w <- positiveIntegerDecimal
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

-- | Parse a number from standard decimal format or from scientific notation.
scientificNotation :: (Fractional a, Monoid p) => Phase p Char o a
scientificNotation = fmap fromRational $ flip id <$> decimal <*> (pure id <|> (
  (\o p n -> o n (10 ^ p)) <$> (iChar 'e' *>
    (pure (*) <|> ((*) <$ char '+') <|> ((/) <$ char '-'))) <*>
    positiveIntegerDecimal
 ))

-- | Move the position counter one character to the right
countChar :: Phase Position i o ()
{-# INLINE countChar #-}
countChar = count (Position 0 1)

-- | Move the position counter to the next line
countLine :: Phase Position i o ()
{-# INLINE countLine #-}
countLine = count (Position 1 1)

-- | Count the lines and characters from the input before yielding them again.
-- If the phase pipeline does not include this or similar: parsing errors will
-- not report the correct position. Unix, Windows, Mac-OS Classic, and Acorn
-- newline formats are all recognized.
trackPosition :: Phase Position Char Char ()
{-# INLINABLE[1] trackPosition #-}
trackPosition = go where
  go = flip (<|>) (return ()) $ get >>= \c -> yield c >> case c of
    '\n' -> countLine >> goN
    '\r' -> countLine >> goR
    _ -> countChar >> go
  goN = flip (<|>) (return ()) $ get >>= \c -> yield c >> case c of
    '\n' -> countLine >> goN
    '\r' -> go
    _ -> countChar >> go
  goR = flip (<|>) (return ()) $ get >>= \c -> yield c >> case c of
    '\n' -> go
    '\r' -> countLine >> goR
    _ -> countChar >> go

-- | Converts all line separators into Unix format.
normalizeNewlines :: Monoid p => Phase p Char Char ()
normalizeNewlines = go where
  go = flip (<|>) (return ()) $ get >>= \c -> case c of
    '\n' -> yield '\n' >> goN
    '\r' -> yield '\n' >> goR
    _ -> yield c >> go
  goN = flip (<|>) (return ()) $ get >>= \c -> case c of
    '\n' -> yield '\n' >> goN
    '\r' -> go
    _ -> yield c >> go
  goR = flip (<|>) (return ()) $ get >>= \c -> case c of
    '\n' -> go
    '\r' -> yield '\n' >> goR
    _ -> yield c >> go

-- | Use a 'Phase' as a parser. Note that unlike other parsers the reported
-- position in the input when the parser fails is the position reached when
-- all parsing options are exhausted, not the beginning of the failing token.
-- Since the characters may be counted nondeterministically: if multiple errors
-- are returned the reported error position may be different for each error
-- report.
parse :: (PhaserType s) => s Position i o a -> [i] -> Either [(Position,[String])] [a]
parse = parse_ (Position 1 1)

-- | sepBy p sep parses zero or more occurrences of p, separated by sep. Returns
-- a list of values returned by p.
sepBy :: Monoid p => Phase p i o a -> Phase p i o s -> Phase p i o [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | sepBy1 p sep parses one or more occurrences of p, separated by sep. Returns
-- a list of values returned by p.
sepBy1 :: Monoid p => Phase p i o a -> Phase p i o s -> Phase p i o [a]
sepBy1 p sep = ((:) <$> p <*> many (sep >> p))

surround :: Phase p i o a -> Phase p i o b -> Phase p i o e -> Phase p i o a
surround m o c = (\_ r _ -> r) <$> o <*> m <*> c

-- | Parses the first zero or more values satisfying the predicate. Always
-- succeds, exactly once, having consumed all the characters Hence NOT the same
-- as (many (satisfy p))
munch :: Monoid p => (i -> Bool) -> Phase p i o [i]
munch p = go id where
  go acc = flip (<|>) (eof >> return (acc [])) $ do
    c <- get
    if p c
      then go (acc . (c :))
      else put1 c >> return (acc [])

-- | Parses the first one or more values satisfying the predicate. Succeeds if
-- at least one value matches, having consumed all the characters Hence NOT the
-- same as (some (satisfy p))
munch1 :: Monoid p => (i -> Bool) -> Phase p i o [i]
munch1 p = go1 where
  go1 = do
    c <- get
    if p c
      then go (c :) <|> (eof >> return [c])
      else empty
  go acc = do
    c <- get
    if p c
      then go (acc . (c :)) <|> (eof >> return (acc [c]))
      else put1 c >> return (acc [])

-- | Run a parser on input from a file. Input is provided as bytes, if
-- characters are needed: a decoding phase such as
-- 'Codec.Phaser.UTF8.utf8_stream' or 'latin1' may be used.
parseFile :: (PhaserType s) => s Position Word8 o a -> FilePath ->
  IO (Either [(Position,[String])] [a])
parseFile = BP.parseFile_ (Position 1 1)

-- | Run a parser on input from a handle. Input is provided as bytes, if
-- characters are needed: a decoding phase such as
-- 'Codec.Phaser.UTF8.utf8_stream' may be used.
parseHandle :: (PhaserType s) => s Position Word8 o a -> Handle ->
  IO (Either [(Position,[String])] [a])
parseHandle = BP.parseHandle_ (Position 1 1)

-- | Decode bytes to characters using the Latin1 (ISO8859-1) encoding
latin1 :: Monoid p => Phase p Word8 Char ()
latin1 = go where
  go = flip (<|>) (return ()) $ 
    fmap (toEnum . fromIntegral) get >>= yield >> go

-- | Decode bytes to characters using the ASCII encoding, aborting if
-- any byte is outside the ASCII range.
ascii :: Monoid p => Phase p Word8 Char ()
ascii = go where
  go = flip (<|>) (return ()) $ get >>= \c -> if c .&. 0x80 == 0
    then yield (toEnum $ fromIntegral c) >> go
    else fail "Byte out of ASCII range"

instance Standardized Char Int where
  regular = integer

instance Standardized Char Integer where
  regular = integer

instance Standardized Char Word where
  regular = positiveInteger

instance Standardized Char Word8 where
  regular = positiveInteger

instance Standardized Char Word16 where
  regular = positiveInteger

instance Standardized Char Word32 where
  regular = positiveInteger

instance Standardized Char Word64 where
  regular = positiveInteger

instance Standardized Char Int8 where
  regular = integer

instance Standardized Char Int16 where
  regular = integer

instance Standardized Char Int32 where
  regular = integer

instance Standardized Char Int64 where
  regular = integer

instance Standardized Char Float where
  regular = scientificNotation

instance Standardized Char Double where
  regular = scientificNotation

instance (Integral a,Standardized Char a) => Standardized Char (Ratio a) where
  regular = scientificNotation <|> ((%) <$> regular <*> (
    munch isSpace *> char '%' *> munch isSpace *> regular
   ))

instance Standardized Char Bool where
  regular = (False <$ (void (char '0') <|> void (iString "false"))) <|>
    (True <$ (void (char '1') <|> void (iString "true")))

-- | Create a trie which maps a single string to an object. Analogous to
-- 'M.singleton'.
newTrie :: Ord c => [c] -> a -> Trie c a
newTrie l0 a = go l0 where
  go [] = Trie [a] M.empty
  go (c:r) = Trie [] $ M.singleton c $ go r

-- | Create a trie from a list of strings and corresponding objects. Analogous
-- to 'M.fromList'
listToTrie :: Ord c => [([c],a)] -> Trie c a
listToTrie = mconcat . map (uncurry newTrie)

-- | Create a 'Phase' or 'Automaton' from a 'Trie'
fromTrie :: (Monoid p, PhaserType s, Ord c) => Trie c a -> s p c o a
fromTrie = fromPhase . go where
  go ~(Trie l m) = let
    n = get >>= \c -> case M.lookup c m of
      Nothing -> empty
      Just r -> go r
    in foldr (<|>) n (map pure l)
