{-|
Module:     Codec.Parser.Core
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

Core functions and types.
-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
module Codec.Phaser.Core (
  Automaton,
  Phase,
  Link(..),
  get,
  put,
  put1,
  count,
  yield,
  eof,
  (<??>),
  (<?>),
  (>#>),
  starve,
  toAutomaton,
  fromAutomaton,
  beforeStep,
  step,
  extract,
  toReadS,
  run,
  parse_,
  parse1_,
  options,
  readCount,
  outputs
 ) where

import Control.Applicative
import Control.Monad

-- | Represents a nondeterministic computation in progress.
-- There are 4 type parameters: a counter type (may be used for tracking line
-- and column numbers), an input type, an incremental output type, and a final
-- output type.
data Automaton p i o a =
  Result a |
  Ready (i -> Automaton p i o a) ([String] -> [String]) |
  Failed ([String] -> [String]) |
  Automaton p i o a :+++ Automaton p i o a |
  Yield o (Automaton p i o a) |
  Count (p -> p) (Automaton p i o a)

-- | A type for building 'Automaton' values. 'Monad' and 'Applicative' instances
-- are defined for this type rather than for 'Automaton' in order to avoid
-- traversing the entire call stack for every input value.
newtype Phase p i o a =
  Phase (([String] -> [String]) ->
    forall b . (a -> Automaton p i o b) -> Automaton p i o b)

infixr 4 >>#
-- | Class for types which consume and produce incremental input and output.
class Link s d l | s d -> l where
  -- | Take the incremental output of the first argument and use it as input
  -- for the second argument. Discard the final output of the first argument.
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
  Phase a <|> Phase b = Phase (\e c -> prune1 (a e c :+++ b id c))

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
  (>>#) = link_p_a

link_p_a :: Phase p i t z -> Automaton p t o a -> Phase p i o a
{-# INLINE [1] link_p_a #-}
link_p_a s d = fromAutomaton (toAutomaton s >># d)

instance Link Phase Phase Phase where
  (>>#) = link_p_p

link_p_p :: Phase p i t z -> Phase p t o a -> Phase p i o a
{-# INLINE [1] link_p_p #-}
link_p_p s d = s >># toAutomaton d

{-# RULES
">>#/>>#/1.1"
  forall (a :: Phase p b c x) (b :: Phase p c t r) (c :: Phase p t g o) .
    link_p_p a (link_p_p b c) = link_p_a a (toAutomaton b >># toAutomaton c)
">>#/>>#/1.2"
  forall (a :: Phase p b c x) (b :: Phase p c t r) (c :: Automaton p t g o) .
    link_p_p a (link_p_a b c) = link_p_a a (toAutomaton b >># c)
">>#/>>#/2.1"
  forall
    (a :: Phase p b c x)
    (b :: Phase p c t y)
    (c :: Phase p t o z) .
     link_p_p (link_p_p a b) c = link_p_p a (link_p_p b c)
">>#/>>#/2.2"
  forall
    (a :: Phase p b c x)
    (b :: Phase p c t y)
    (c :: Automaton p t o z) .
     link_p_a (link_p_p a b) c = link_p_p a (link_p_a b c)
">>#/>>#/2.3"
  forall
    (a :: Phase p b c x)
    (b :: Automaton p c t y)
    (c :: Automaton p t o z) .
     link_p_a (link_p_a a b) c = link_p_a a (b >># c)
 #-}

instance Link Automaton Automaton Automaton where
  {-# INLINE [2] (>>#) #-}
  (>>#) = (!!!) where
    Yield o r !!! d = case beforeStep d of
      Left e -> e
      Right d' -> r !!! step d' o
    Failed e !!! _ = Failed e
    _ !!! Failed e = Failed e
    Result _ !!! d = starve d
    (a :+++ b) !!! d = prune1 ((a !!! d) :+++ (b !!! d))
    Count p r !!! d = prune1 (Count p (r !!! d))
    s !!! Count p r = prune1 (Count p (s !!! r))
    s !!! Yield o r = Yield o (s !!! r)
    Ready n e !!! d = Ready (\t -> n t !!! d) e

{- 
{-# RULES
"toAutomaton/fromAutomaton" forall a . toAutomaton (fromAutomaton a) = a
"fromAutomaton/toAutomaton" forall a . fromAutomaton (toAutomaton a) = a
 #-} -}

-- | When the right argument fails: apply the left argument to the list of
-- error messages. Depreciated because it doesn't work correctly for all
-- arguments and fixing it would break the 'Alternative' and 'MonadPlus'
-- instances.
infixr 1 <??>
{-# WARNING (<??>) "<??> is faulty and will be removed in future versions. \
  \Please use <?> instead" #-}
(<??>) :: ([String] -> [String]) -> Phase p i o a -> Phase p i o a
f <??> Phase s = Phase (\e -> s (f . e))

-- | If parsing fails in the right argument: prepend the left argument to the
-- errors
infixr 1 <?>
e <?> Phase s = Phase (\e1 -> s ((e :) . e1))

-- | Change the counter type of a Phase object.
infixr 1 >#>
(>#>) :: ((p0 -> p0) -> p -> p) -> Phase p0 i o a -> Phase p i o a
{-# INLINABLE [1] (>#>) #-}
f >#> p = fromAutomaton $ go $ toAutomaton p where
  go (Result a) = Result a
  go (Ready n e) = Ready (fmap go n) e
  go (Failed e) = Failed e
  go (a :+++ b) = go a :+++ go b
  go (Yield t r) = Yield t (go r)
  go (Count p r) = Count (f p) (go r)

{-# RULES
">#>/>#>" forall pt2 pt1 a . pt2 >#> (pt1 >#> a) = (pt2 . pt1) >#> a
  #-}

-- | Return one item of the input.
get :: Phase p i o i
get = Phase (flip Ready)

-- | Modify the counter
count :: (p -> p) -> Phase p i o ()
{-# INLINE [1] count #-}
count f = Phase (\_ c -> Count f (c ()))

-- | Yield one item for the incremental output
yield :: o -> Phase p i o ()
{-# INLINE [1] yield #-}
yield o = Phase (\_ c -> Yield o (c ()))

-- | Fail if any more input is provided.
eof :: Phase p i o ()
eof = Phase (\e c -> prune1 (Failed e :+++ starve (c ())))

-- | Insert one value back into the input. May be used for implementing lookahead
put1 :: i -> Phase p i o ()
{-# INLINE [1] put1 #-}
put1 i = Phase (\_ c -> case beforeStep (c ()) of
  Right n -> step n i
  Left e -> e
 )

-- | Put a list of values back into the input.
put :: [i] -> Phase p i o ()
{-# INLINE [1] put #-}
put i = Phase (\_ c -> run (c ()) i)

{-# RULES
"count/yield" forall p o . count p >> yield o = yield o >> count p
  #-}

{-# INLINABLE [1] prune1 #-}
prune1 (Failed e1 :+++ Failed e2) = Failed (e1 . e2)
prune1 (Failed e1 :+++ Ready n e2) = Ready n (e1 . e2)
prune1 (Ready n e1 :+++ Failed e2) = Ready n (e1 . e2)
prune1 (Ready n1 e1 :+++ Ready n2 e2) =
  Ready (\i -> prune1 $ n1 i :+++ n2 i) (e1 . e2)
prune1 (r@(Result _) :+++ Failed _) = r
prune1 (Failed _ :+++ r@(Result _)) = r
prune1 (Count p (Count q r)) = prune1 $ Count (\w -> let
  w' = p w
  in w' `seq` q w') r
prune1 (Count p (Yield o r)) =
  prune1 (Yield o (prune1 (Count p r)))
prune1 (Yield _ f@(Failed _)) = f
prune1 (Yield _ f@(Count _ (Failed _))) = f
prune1 a = a

{-# RULES
"prune1/prune1" forall a . prune1 (prune1 a) = prune1 a
  #-}

-- | Remove an 'Automaton''s ability to consume further input
starve :: Automaton p i o a -> Automaton p z o a
{-# INLINABLE [1] starve #-}
starve (Result a) = Result a
starve (Ready _ e) = Failed e
starve (Failed e) = Failed e
starve (a :+++ b) = prune1 (starve a :+++ starve b)
starve (Yield o r) = prune1 (Yield o (starve r))
starve (Count p r) = prune1 (Count p (starve r))

{-# RULES
"starve/starve" forall a . starve (starve a) = starve a
  #-}

-- | Convert a 'Phase' to an 'Automaton'. Subject to fusion.
toAutomaton :: Phase p i o a -> Automaton p i o a
{-# INLINE[2] toAutomaton #-}
toAutomaton (Phase c) = c id Result

-- | Convert an 'Automaton' back to a 'Phase' (somewhat inefficient). Subject
-- to fusion.
fromAutomaton :: Automaton p i o a -> Phase p i o a
{-# INLINE[2] fromAutomaton #-}
fromAutomaton a = Phase (\e' c -> let
  continue (Result r) = c r
  continue (Ready n e) = Ready (fmap continue n) (e' . e)
  continue (Failed e) = Failed (e' . e)
  continue (l :+++ r) = prune1 (continue l :+++ continue r)
  continue (Count p r) = prune1 (Count p (continue r))
  continue (Yield o r) = prune1 (Yield o (continue r))
  in continue a
 )

-- | Optional pre-processing of an automaton before passing it more input.
-- Produces 'Right' with all "final outputs" and errors stripped if the
-- automaton can accept more input, and 'Left' with everything except errors
-- stripped if it cannot accept more input.
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

-- | Pass one input to an automaton
step :: Automaton p i o a -> i -> Automaton p i o a
step a' i = go a' where
  go (Result _) = Failed id
  go (Ready n _) = n i
  go (Failed e) = Failed e
  go (a :+++ b) = prune1 (go a :+++ go b)
  go (Yield o r) = prune1 (Yield o (go r))
  go (Count p r) = prune1 (Count p (go r))

-- | Take either counters with errors or a list of possible results from an
-- automaton.
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

-- | Create a 'ReadS' like value from an 'Automaton'. If the Automaton's input
-- type is 'Char', the result will be 'ReadS'
toReadS :: Automaton p i o a -> [i] -> [(a,[i])]
toReadS a i = go a i [] where
  go (Result r) i' = ((r,i'):)
  go (Ready _ _) [] = id
  go (Ready n _) (t:r) = go (n t) r
  go (Failed _) _ = id
  go (a :+++ b) i' = go a i' . go b i'
  go (Yield _ r) i' = go r i'
  go (Count _ r) i' = go r i'

-- | Pass a list of input values to an 'Automaton'
run :: Automaton p i o a -> [i] -> Automaton p i o a
run = go where
  go a [] = a
  go a (i:r) = case beforeStep a of
    Left a' -> a'
    Right a' -> go (step a' i) r

-- | Use a 'Phase' value similarly to a parser.
parse_ :: p -> Phase p i o a -> [i] -> Either [(p,[String])] [a]
parse_ p a i = extract p $ run (toAutomaton a) i

-- | Use a 'Phase' as a parser, but consuming a single input instead of a list
parse1_ :: p -> Phase p i o a -> i -> Either [(p,[String])] [a]
parse1_ p a i = extract p $ step (toAutomaton a) i

-- | Decompose an 'Automaton' into its component options.
options :: Automaton p i o a -> [Automaton p i o a]
options = ($ []) . go where
  go (a :+++ b) = go a . go b
  go (Yield o r) = (fmap . fmap) (Yield o) $ go r
  go (Count p r) = (fmap . fmap) (Count p) $ go r
  go a = (a :)

-- | Separate unconditional counter modifiers from an automaton
readCount :: Automaton p i o a -> (p -> p, Automaton p i o a)
readCount = go where
  go (Count p0 r) = let
    (p1, r') = go r
    p' c = let
      c' = p0 c
      in c' `seq` p1 c'
    in (p', r')
  go (Yield o r) = let
    (p, r') = go r
    in (p, prune1 $ Yield o r')
  go a = (id, a)

-- | Separate the values unconditionally yielded by an automaton
outputs :: Automaton p i o a -> ([o], Automaton p i o a)
outputs = go where
  go (Yield o r) = let
    (o', r') = go r
    in (o:o', r')
  go (Count p r) = let
    (o, r') = go r
    in (o, prune1 $ Count p r')
  go a = ([], a)

