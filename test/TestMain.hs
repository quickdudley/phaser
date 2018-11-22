import Control.Applicative
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
