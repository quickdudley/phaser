{-|
Module:     Codec.Parser.UTF8
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

'Phase's for decoding bytes to characters using UTF-8
-}
module Codec.Phaser.UTF8 (
  utf8_char,
  utf8_stream,
  utf8_encode
 ) where

import Data.Bits
import Data.Word
import Data.List
import Control.Monad
import Control.Applicative

import Codec.Phaser.Core

-- | Consume a UTF-8 character from a stream of bytes and return it. Fail on
-- invalid UTF-8.
utf8_char :: (Monoid p) => Phase p Word8 o Char
utf8_char = do
  c1 <- fmap fromIntegral get
  case () of
   _ | c1 .&. 0x80 == 0 -> return $ toEnum c1
     | c1 .&. 0x40 == 0 -> fail "UTF-8 codepoint missing initial byte"
     | complement c1 .&. 0x38 == 0 ->
        fail "Invalid UTF-8 codepoint initial byte"
     | otherwise -> go 0x20 (c1 .&. 0x3F)
 where
  go z a = do
    c2 <- "Incomplete UTF-8 codepoint" <?> do
      c <- fmap fromIntegral get
      guard $ c .&. 0xc0 == 0x80
      return c
    if (a .&. z) == 0
      then return $ toEnum $ shiftL a 6 .|. (c2 .&. 0x3F)
      else go (shiftL z 5) (shiftL (a .&. complement z) 6 .|. (c2 .&. 0x3F))

-- | Consume any number of UTF-8 characters and yield them.
utf8_stream :: (Monoid p) => Phase p Word8 Char ()
utf8_stream = (utf8_char >>= yield >> utf8_stream) <|> return ()

-- | Consume any number of Characters and yield them as UTF-8 bytes
utf8_encode :: (Monoid p) => Phase p Char Word8 ()
utf8_encode = (fromEnum <$> get) >>= \c -> if c > 0 && c < 0x80
  then yield (fromIntegral c) >> nxt
  else go 0xC0 0x20 c []
 where
  nxt = (utf8_encode <|> return ())
  go pfb fzb c' o = let
    l = shiftR c' 6
    m = fromIntegral (c' .&. 0x3F)
    m' = m .|. 0x80
    in if l < fromIntegral fzb
      then foldr (\b r -> yield b >> r) nxt $
        (pfb .|. fromIntegral l) : m' : o
      else go (pfb .|. fzb) (shiftR fzb 1) l (m' : o)
