{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
module Phaser.Core (
  Automaton,
  Phase,
  Link(..),
  Applicative(..),
  Alternative(..),
  get,
  count,
  yield,
  (<??>),
  (>#>),
  toAutomaton,
  fromAutomaton,
  beforeStep,
  step,
  extract,
  toReadS,
  parse_
 ) where

import Control.Applicative
import Control.Monad

data Automaton p i o a =
  Result a |
  Ready (i -> Automaton p i o a) ([String] -> [String]) |
  Failed ([String] -> [String]) |
  Automaton p i o a :+++ Automaton p i o a |
  Yield o (Automaton p i o a) |
  Count (p -> p) (Automaton p i o a)

newtype Phase p i o a =
  Phase (([String] -> [String]) ->
    forall b . (a -> Automaton p i o b) -> Automaton p i o b)

infixr 4 >>#
class Link s d l | s d -> l where
  (>>#) :: s p b c x -> d p c t a -> l p b t a

instance Functor (Phase p i o) where
  fmap f (Phase x) = Phase (\e c -> x e (c . f))

instance Applicative (Phase p i o) where
  pure a = Phase (\e c -> c a)
  Phase f <*> Phase a = Phase (\e c -> f e (\f' -> a e (c . f')))

instance Monad (Phase p i o) where
  return = pure
  fail s = Phase (\e _ -> Failed (e . (s:)))
  Phase a >>= f = Phase (\e c -> a e (\a' -> let Phase b = f a' in b e c))

instance Alternative (Phase p i o) where
  empty = Phase (\e _ -> Failed e)
  Phase a <|> Phase b = Phase (\e c -> prune1 (a e c :+++ b e c))

instance MonadPlus (Phase p i o) where
  mzero = empty
  mplus = (<|>)

instance Functor (Automaton p i o) where
  fmap f = go where
    go (Result a) = Result (f a)
    go (Ready n e) = Ready (fmap go n) e
    go (Failed e) = Failed e
    go (a :+++ b) = go a :+++ go b
    go (Yield o r) = Yield o (go r)
    go (Count p r) = Count p (go r)

instance Link Phase Automaton Phase where
  s >># d = fromAutomaton (toAutomaton s >># d)

instance Link Phase Phase Phase where
  s >># d = s >># toAutomaton d

instance Link Automaton Automaton Automaton where
  Yield o r >># d = case beforeStep d of
    Left e -> e
    Right d' -> r >># step d' o
  Failed e >># _ = Failed e
  _ >># Failed e = Failed e
  Result _ >># d = starve d
  (a :+++ b) >># d = prune1 ((a >># d) :+++ (b >># d))
  s >># (a :+++ b) = prune1 ((s >># a) :+++ (s >># b))
  Count p r >># d = prune1 (Count p (r >># d))
  s >># Count p r = prune1 (Count p (s >># r))
  Ready n e >># d = Ready (\t -> n t >># d) e

infixl 1 <??>
(<??>) :: ([String] -> [String]) -> Phase p i o a -> Phase p i o a
f <??> Phase s = Phase (\e -> s (f . e))

infixl 1 >#>
(>#>) :: ((p0 -> p0) -> p -> p) -> Phase p0 i o a -> Phase p i o a
f >#> p = fromAutomaton $ go $ toAutomaton p where
  go (Result a) = Result a
  go (Ready n e) = Ready (fmap go n) e
  go (Failed e) = Failed e
  go (a :+++ b) = go a :+++ go b
  go (Yield t r) = Yield t (go r)
  go (Count p r) = Count (f p) (go r)

get :: Phase p i o i
get = Phase (flip Ready)

count :: (p -> p) -> Phase p i o ()
count f = Phase (\_ c -> Count f (c ()))

yield :: o -> Phase p i o ()
yield o = Phase (\_ c -> Yield o (c ()))

prune1 (Failed e1 :+++ Failed e2) = Failed (e1 . e2)
prune1 (Failed e1 :+++ Ready n e2) = Ready n (e1 . e2)
prune1 (Ready n e1 :+++ Failed e2) = Ready n (e1 . e2)
prune1 (Ready n1 e1 :+++ Ready n2 e2) =
  Ready (\i -> prune1 $ n1 i :+++ n2 i) (e1 . e2)
prune1 (Count p (Count q r)) = prune1 $ Count (\w -> let
  w' = p w
  in w' `seq` q w') r
prune1 (Count p (a :+++ b)) =
  prune1 (prune1 (Count p a) :+++ prune1 (Count p b))
prune1 (Count p (Yield o r)) =
  prune1 (Yield o (prune1 (Count p r)))
prune1 (Yield _ f@(Failed _)) = f
prune1 (Yield _ f@(Count _ (Failed _))) = f
prune1 a = a

starve (Result a) = Result a
starve (Ready _ e) = Failed e
starve (Failed e) = Failed e
starve (a :+++ b) = prune1 (starve a :+++ starve b)
starve (Yield o r) = prune1 (Yield o (starve r))
starve (Count p r) = prune1 (Count p (starve r))

toAutomaton :: Phase p i o a -> Automaton p i o a
toAutomaton (Phase c) = c id Result

fromAutomaton :: Automaton p i o a -> Phase p i o a
fromAutomaton a = Phase (\e' c -> let
  continue (Result r) = c r
  continue (Ready n e) = Ready (fmap continue n) (e' . e)
  continue (Failed e) = Failed (e' . e)
  continue (l :+++ r) = prune1 (continue l :+++ continue r)
  continue (Count p r) = prune1 (Count p (continue r))
  continue (Yield o r) = prune1 (Yield o (continue r))
  in continue a
 )

beforeStep :: Automaton p i o a ->
  Either (Automaton p v o a) (Automaton p i o a)
beforeStep = go where
  go :: Automaton p i o a ->
    Either (Automaton p v o a) (Automaton p i o a)
  go (Result _) = Left (Failed id)
  go r@(Ready _ _) = Right r
  go (Failed f) = Left $ Failed f 
  go (a :+++ b) = case (go a, go b) of
    (Right a', Right b') -> Right $ prune1 $ a' :+++ b'
    (a'@(Right _), Left _) -> a'
    (Left _, b'@(Right _)) -> b'
    (Left a', Left b') -> Left $ prune1 $ a' :+++ b'
  go (Yield o r) = case go r of
    r'@(Left _) -> r'
    Right r' -> Right (prune1 $ Yield o r')
  go (Count p r) = case go r of
    Left r' -> Left $ prune1 $ Count p r'
    Right r' -> Right $ prune1 $ Count p r'

step :: Automaton p i o a -> i -> Automaton p i o a
step a' i = go a' where
  go (Result _) = Failed id
  go (Ready n _) = n i
  go (Failed e) = Failed e
  go (a :+++ b) = prune1 (go a :+++ go b)
  go (Yield o r) = prune1 (Yield o (go r))
  go (Count p r) = prune1 (Count p (go r))

extract :: p -> Automaton p i o a -> Either [(p,[String])] [a]
extract p' a = case go p' a of
  Left e -> Left $ map (\(p,e') -> (p, e' [])) (e [])
  Right r -> Right $ r []
 where
  go _ (Result z) = Right (z:)
  go p (Ready _ e) = Left ((p,e):)
  go p (Failed e) = Left ((p,e):)
  go p (a :+++ b) = case (go p a, go p b) of
    (Right a', Right b') -> Right (a' . b')
    (a'@(Right _), Left _) -> a'
    (Left _, b'@(Right _)) -> b'
    (Left a', Left b') -> Left (a' . b')
  go p (Yield _ r) = go p r
  go p (Count i r) = let
    p' = i p
    in p' `seq` go p' r

toReadS :: Automaton p i o a -> [i] -> [(a,[i])]
toReadS a i = go a i [] where
  go (Result r) i' = ((r,i'):)
  go (Ready _ _) [] = id
  go (Ready n _) (t:r) = go (n t) r
  go (Failed _) _ = id
  go (a :+++ b) i' = go a i' . go b i'
  go (Yield _ r) i' = go r i'
  go (Count _ r) i' = go r i'

run :: Automaton p i o a -> [i] -> Automaton p i o a
run = go where
  go a [] = a
  go a (i:r) = case beforeStep a of
    Left a' -> a'
    Right a' -> go (step a' i) r

parse_ :: p -> Phase p i o a -> [i] -> Either [(p,[String])] [a]
parse_ p a i = extract p $ run (toAutomaton a) i

