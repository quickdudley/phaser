import Control.Applicative
import Data.Char
import Data.List
import Test.QuickCheck

import Codec.Phaser.Core
import Codec.Phaser.Common

main = do
  quickCheck testScientific
  quickCheck testOldChain
  quickCheck testOptions

testScientific :: Double -> Bool
testScientific v = case parse_ () scientificNotation (show v) of
  Right l@(_:_) -> all (== v) l
  _ -> False

testOldChain :: String -> Bool
testOldChain s = case parse_ () (go >># many get) s of
  Right [s'] -> s == s'
  _ -> False
 where
  go = (get >>= yield >> go) <|> return ()

testOptions :: Bool
testOptions = let
  a = yield 'a' <|> yield 'b'
  opts = map (>># many get) $ options $ toAutomaton a
  in map (\p -> parse p "") opts == [Right ["a"], Right ["b"]]

testGetCount :: [Int] -> Bool
testGetCount s' = let
  s = map ((+1) . abs) s'
  i = foldr (\n r -> replicate (n-1) ' ' ++ ('x':r)) [] s
  p = let
    go _ [] = []
    go n (a:r) = let b = a + n in b : go b r
    in go 0 s
  c = trackPosition >># (many (munch isSpace *> getCount <* char 'x'))
  in parse c i ==
     Right [map (Position 1) p]
