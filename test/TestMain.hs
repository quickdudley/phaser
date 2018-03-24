import Control.Applicative
import Data.List
import Test.QuickCheck

import Codec.Phaser.Core
import Codec.Phaser.Common

main = do
  quickCheck testScientific
  quickCheck testOldChain

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
