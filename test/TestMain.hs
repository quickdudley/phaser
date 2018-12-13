import Control.Applicative
import Data.Char
import Data.List
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Codec.Phaser.Core
import Codec.Phaser.Common

main = do
  results <- sequenceA $ [
    quickCheckResult testScientific,
    quickCheckResult testOldChain,
    quickCheckResult testOptions,
    quickCheckResult testGetCount
   ]
  if all isSuccess results
    then return ()
    else exitFailure

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

testGetCount :: [Bool] -> Bool
testGetCount s' = let
  i = map (\b -> if b
    then 'x'
    else ' '
   ) s'
  p = map snd $ filter fst $ zip s' [1 ..]
  -- We accept slightly erroneous behaviour from `getCount` where `munch` is
  -- involved.
  c = trackPosition >># (
    many (many (satisfy isSpace) *> getCount <* char 'x') <*
    munch isSpace
   )
  in parse c i ==
     Right [map (Position 1) p]
