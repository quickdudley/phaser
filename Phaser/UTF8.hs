module Phaser.UTF8 (
  utf8_char,
  utf8_stream
 ) where

import Data.Bits
import Data.Word
import Data.List
import Control.Monad
import Control.Applicative

import Phaser.Core

utf8_char :: Phase p Word8 o Char
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
    c2 <- ("Incomplete UTF-8 codepoint":) <??> do
      c <- fmap fromIntegral get
      guard $ c .&. 0xc0 == 0x80
      return c
    if (a .&. z) == 0
      then return $ toEnum $ shiftL a 6 .|. (c2 .&. 0x3F)
      else go (shiftL z 5) (shiftL (a .&. complement z) 6 .|. (c2 .&. 0x3F))

utf8_stream :: Phase p Word8 Char ()
utf8_stream = (utf8_char >>= yield >> utf8_stream) <|> return ()

