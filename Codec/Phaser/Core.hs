{-|
Module:     Codec.Parser.Core
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

Core functions and types.
-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}
module Codec.Phaser.Core (
  Automaton,
  Phase,
  get,
  put,
  put1,
  count,
  getCount,
  yield,
  eof,
  neof,
  (<?>),
  (>>#),
  (>#>),
  starve,
  PhaserType(..),
  fitYield,
  beforeStep,
  step,
  extract,
  toReadS,
  run,
  parse_,
  parse1_,
  options,
  readCount,
  outputs,
  stream
 ) where

import Control.Applicative
import Control.Monad
import Data.Void
import Unsafe.Coerce

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
  Count p (Automaton p i o a) |
  GetCount (p -> Automaton p i o a)

-- | A type for building 'Automaton' values. 'Monad' and 'Applicative' instances
-- are defined for this type rather than for 'Automaton' in order to avoid
-- traversing the entire call stack for every input value.
newtype Phase p i o a =
  Phase (([String] -> [String]) ->
    forall b . (a -> Automaton p i o b) -> Automaton p i o b)

infixr 4 >>#
infixl 5 $#$
-- | Class for types which consume and produce incremental input and output.
class PhaserType s where
  toAutomaton :: (Monoid p) => s p i o a -> Automaton p i o a
  fromAutomaton :: (Monoid p) => Automaton p i o a -> s p i o a
  toPhase :: (Monoid p) => s p i o a -> Phase p i o a
  fromPhase :: (Monoid p) => Phase p i o a -> s p i o a
  ($#$) :: (Monoid p) => s p b c x -> (c -> t) -> s p b t x

instance Functor (Phase p i o) where
  fmap f (Phase x) = Phase (\e c -> x e (c . f))

instance Applicative (Phase p i o) where
  pure a = Phase (\e c -> c a)
  Phase f <*> Phase a = Phase (\e c -> f e (\f' -> a e (c . f')))
  Phase a <* Phase b = Phase (\e c -> a e (\a' -> b e (\_ -> c a')))
  (*>) = (>>)

-- This is the first time I've ever created a separate '>>' method for a monad
-- instance, but turns out in this case it's a significant optimization.
instance Monad (Phase p i o) where
  return = pure
  fail s = Phase (\e _ -> Failed (e . (s:)))
  Phase a >>= f = Phase (\e c -> a e (\a' -> let Phase b = f a' in b e c))
  Phase a >> Phase b = Phase (\e c -> a e (const (b e c)))

instance (Monoid p) => Alternative (Phase p i o) where
  empty = Phase (\e _ -> Failed e)
  Phase a <|> Phase b = Phase (\e c -> prune1 (a e c :+++ b id c))
  many a = some a <|> pure []
  some (Phase a) = Phase (\e c -> let
    go acc = a e (\x -> let acc' = acc . (x:) in prune1 (go acc' :+++ c (acc' [])))
    in go id
   )

instance (Monoid p) => MonadPlus (Phase p i o) where
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
    go (GetCount n) = GetCount (fmap go n)

instance PhaserType Phase where
  toAutomaton (Phase c) = c id Result
  fromAutomaton a = Phase (\e' c -> let
    continue (Result r) = c r
    continue (Ready n e) = Ready (fmap continue n) (e' . e)
    continue (Failed e) = Failed (e' . e)
    continue (l :+++ r) = prune1 (continue l :+++ continue r)
    continue (Count p r) = prune1 (Count p (continue r))
    continue (Yield o r) = prune1 (Yield o (continue r))
    continue (GetCount n) = GetCount (fmap continue n)
    in continue a
   )
  toPhase = id
  fromPhase = id
  ($#$) = source_p

instance PhaserType Automaton where
  toAutomaton = id
  fromAutomaton = id
  toPhase = fromAutomaton
  fromPhase = toAutomaton
  ($#$) = source_a

-- | Take the incremental output of the first argument and use it as input
-- for the second argument. Discard the final output of the first argument.
(>>#) :: (Monoid p, PhaserType s, PhaserType d) => s p b c x -> d p c t a -> Automaton p b t a
a >># b = toAutomaton a !!! toAutomaton b where
    s@(Yield _ _) !!! Yield o r = prune1 (Yield o (s !!! r))
    Yield o r !!! d = case beforeStep' d of
      Left e -> e
      Right d' -> let s = step' d' o in s `seq` (r !!! s)
    Failed e !!! _ = Failed e
    _ !!! Failed e = Failed e
    Result _ !!! d = starve d
    Count p r !!! d = prune1 (Count p (r !!! d))
    s !!! Yield o r = prune1 (Yield o (s !!! r))
    s !!! Count p r = prune1 (Count p (s !!! r))
    s !!! GetCount n = GetCount (\p -> s !!! n p)
    (a :+++ b) !!! d = prune1 ((a !!! d) :+++ (b !!! d))
    Ready n e !!! d = Ready (\t -> n t !!! d) e
    GetCount n !!! d = GetCount (\p -> n p !!! d)

source_a :: Automaton p i c a -> (c -> t) -> Automaton p i t a
{-# INLINE[1] source_a #-}
source_a a f = go a where
  go (Result a) = Result a
  go (Ready p e) = Ready (fmap go p) e
  go (Failed e) = Failed e
  go (a :+++ b) = go a :+++ go b
  go (Yield o r) = Yield (f o) (go r)
  go (Count p r) = Count p (go r)
  go (GetCount n) = GetCount (fmap go n)

{-# INLINE[1] source_p #-}
source_p p f = fromAutomaton $ source_a (toAutomaton p) f

{-# RULES
"$#$/$#$/1" forall a f1 f2 . source_a (source_a a f1) f2 = source_a a (f2 . f1)
"$#$/$#$/1" forall a f1 f2 . source_p (source_p a f1) f2 = source_p a (f2 . f1)
"toAutomaton/$#$" forall p f . toAutomaton (source_p p f) = source_a (toAutomaton p) f
 #-}

-- | If parsing fails in the right argument: prepend the left argument to the
-- errors
infixr 1 <?>
e <?> Phase s = Phase (\e1 -> s ((e :) . e1))

-- | Change the counter type of a Phaser object. May cause 'getCount' to
-- behave differently from expected: counter increments inside the right hand
-- argument are visible outside but not vice versa.
infixr 1 >#>
(>#>) :: forall s p0 p i o a . (PhaserType s, Monoid p0, Monoid p) =>
  (p0 -> p) -> s p0 i o a -> s p i o a
{-# INLINABLE [1] (>#>) #-}
f >#> p = fromAutomaton $ go mempty $ toAutomaton p where
  go :: (PhaserType s, Monoid p0, Monoid p) => p0 -> Automaton p0 i o a -> Automaton p i o a
  go _ (Result a) = Result a
  go p (Ready n e) = Ready (fmap (go p) n) e
  go _ (Failed e) = Failed e
  go p (a :+++ b) = go p a :+++ go p b
  go p (Yield t r) = Yield t (go p r)
  go p0 (Count p r) = let
    p' = mappend p0 p
    in p' `seq` Count (f p) (go p' r)
  go p (GetCount n) = go p (n p)

-- | Return one item of the input.
get :: Phase p i o i
get = Phase (flip Ready)

-- | Increment the counter
count :: p -> Phase p i o ()
{-# INLINE [1] count #-}
count f = Phase (\_ c -> Count f (c ()))

-- | Retrieve the current counter. Counter values are shared between arguments
-- to '>>#' except when at least one argument is produced by an incompatible function.
-- All functions in this module are compatible unless noted in the corresponding
-- documentation.
getCount :: Phase p i o p
getCount = Phase (const GetCount)

-- | Yield one item for the incremental output
yield :: o -> Phase p i o ()
{-# INLINE [1] yield #-}
yield o = Phase (\_ c -> Yield o (c ()))

-- | Fail if any more input is provided.
eof :: (Monoid p) => Phase p i o ()
eof = Phase (\e c -> starve (c ()))

-- | Fail unless more input is provided.
neof :: (Monoid p) => Phase p i o ()
neof = Phase (\e c -> case beforeStep (c ()) of
  Right r -> r
  Left r -> r
 )

-- | Insert one value back into the input. May be used for implementing lookahead
put1 :: (Monoid p) => i -> Phase p i o ()
{-# INLINE [1] put1 #-}
put1 i = Phase (\_ c -> case beforeStep' (c ()) of
  Right n -> step' n i
  Left e -> e
 )

-- | Put a list of values back into the input.
put :: (Monoid p) => [i] -> Phase p i o ()
{-# INLINE [1] put #-}
put i = Phase (\_ c -> run' (c ()) i)

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
prune1 (f@(Failed _) :+++ (a :+++ b)) = prune1 (prune1 (f :+++ a) :+++ b)
prune1 ((a :+++ b) :+++ f@(Failed _)) = prune1 (a :+++ prune1 (b :+++ f))
prune1 (f@(Failed _) :+++ Yield o r) = prune1 (Yield o (prune1 (f :+++ r)))
prune1 (Yield o r :+++ f@(Failed _)) = prune1 (Yield o (prune1 (r :+++ f)))
prune1 (a@(Ready _ _) :+++ (b@(Ready _ _) :+++ c)) = prune1 (prune1 (a :+++ b) :+++ c)
prune1 ((a :+++ b@(Ready _ _)) :+++ c@(Ready _ _)) = prune1 (a :+++ prune1 (b :+++ c))
prune1 (Count p (Count q r)) = prune1 $ let t = mappend p q in t `seq` Count t r
prune1 (Count p (Yield o r)) =
  prune1 (Yield o (prune1 (Count p r)))
prune1 (Yield _ f@(Failed _)) = f
prune1 (Yield _ f@(Count _ (Failed _))) = f
prune1 a = a

{-# RULES
"prune1/prune1" forall a . prune1 (prune1 a) = prune1 a
  #-}

-- | Remove an 'Automaton''s ability to consume further input
starve :: (Monoid p) => Automaton p i o a -> Automaton p z o a
{-# INLINABLE [1] starve #-}
starve (Result a) = Result a
starve (Ready _ e) = Failed e
starve (Failed e) = Failed e
starve (a :+++ b) = prune1 (starve a :+++ starve b)
starve (Yield o r) = prune1 (Yield o (starve r))
starve (Count p r) = prune1 (Count p (starve r))
starve (GetCount n) = GetCount (fmap starve n)

{-# RULES
"starve/starve" forall a . starve (starve a) = starve a
  #-}

-- | Take a 'Phase' or 'Automaton' which doesn't 'yield' anything and allow
-- it to be used in a chain containing yield statements
fitYield :: PhaserType s => s p i Void a -> s p i o a
fitYield = unsafeCoerce

-- | Optional pre-processing of an automaton before passing it more input.
-- Produces 'Right' with all "final outputs" and errors stripped if the
-- automaton can accept more input, and 'Left' with everything except errors
-- stripped if it cannot accept more input.
beforeStep :: (Monoid p) => Automaton p i o a ->
  Either (Automaton p v o a) (Automaton p i o a)
beforeStep = go mempty where
  go _ (Result _) = Left (Failed id)
  go _ r@(Ready _ _) = Right r
  go _ (Failed f) = Left $ Failed f 
  go p (a :+++ b) = case (go p a, go p b) of
    (Right a', Right b') -> Right $ prune1 $ a' :+++ b'
    (a'@(Right _), Left _) -> a'
    (Left _, b'@(Right _)) -> b'
    (Left a', Left b') -> Left $ prune1 $ a' :+++ b'
  go p (Yield o r) = case go p r of
    r'@(Left _) -> r'
    Right r' -> Right (prune1 $ Yield o r')
  go p0 (Count p1 r) = let p = mappend p0 p1 in case go p r of
    Left r' -> Left $ prune1 $ Count p1 r'
    Right r' -> Right $ prune1 $ Count p1 r'
  go p (GetCount n) = go p (n p)

-- Apoligies for code duplication
beforeStep' :: (Monoid p) => Automaton p i o a ->
  Either (Automaton p v o a) (Automaton p i o a)
beforeStep' = fmap (\(s,a,b) -> if s then a else b) . go where
  go (Result _) = Left (Failed id)
  go r@(Ready _ _) = Right (True,r,r)
  go (Failed e) = Left (Failed e)
  -- When we reach 'GetCount': we don't know whether or not it's hiding a 'Ready'.
  -- It can't be safely removed, and unlike finding 'Ready' it doesn't
  -- indicate that errors be safely removed.
  go (a :+++ b) = case (go a, go b) of
    (Left a', Left b') -> Left (prune1 (a' :+++ b'))
    -- You could do without 'unsafeCoerce' here if you used ImpredicativeTypes:
    -- but it wouldn't be worth it.
    (Right (sa,a1,a2), Left b') -> Right (sa,a1,prune1 (a2 :+++ unsafeCoerce b'))
    (Left a', Right (sb,b1,b2)) -> Right (sb,b1,prune1 (unsafeCoerce a' :+++ b2))
    (Right (sa,a1,a2), Right (sb,b1,b2)) ->
      Right (sa || sb, prune1 (a1 :+++ b1), prune1 (a2 :+++ b2))
  go (Yield o r) = case go r of
    Left r' -> Left (Yield o r')
    Right (s,r1,r2) -> Right (s, Yield o r1, Yield o r2)
  go (Count p r) = case go r of
    Left r' -> Left (Count p r')
    Right (s,r1,r2) -> Right (s, Count p r1, Count p r2)
  go a@(GetCount _) = Right (False,a,a)

-- | Pass one input to an automaton
step :: (Monoid p) => Automaton p i o a -> i -> Automaton p i o a
step a' i = go mempty a' where
  go _ (Result _) = Failed id
  go _ (Ready n _) = n i
  go _ (Failed e) = Failed e
  go p (a :+++ b) = prune1 (go p a :+++ go p b)
  go p (Yield o r) = prune1 (Yield o (go p r))
  go p0 (Count p1 r) = let
    p = mappend p0 p1
    in p `seq` prune1 (Count p1 (go p r))
  go p (GetCount n) = go p (n p)

step' :: (Monoid p) => Automaton p i o a -> i -> Automaton p i o a
step' a' i = go a' where
  go (Result _) = Failed id
  go (Ready n _) = n i
  go (Failed e) = Failed e
  go (a :+++ b) = prune1 (go a :+++ go b)
  go (Yield o r) = prune1 (Yield o (go r))
  go (Count p r) = prune1 (Count p (go r))
  go (GetCount n) = GetCount (fmap go n)

-- | Take either counters with errors or a list of possible results from an
-- automaton.
extract :: (Monoid p) => p -> Automaton p i o a -> Either [(p,[String])] [a]
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
    p' = mappend p i
    in p' `seq` go p' r
  go p (GetCount n) = go p (n p)

-- | Create a 'ReadS' like value from a Phaser type. If the input
-- type is 'Char', the result will be 'ReadS'
toReadS :: (PhaserType s, Monoid p) =>
  s p i o a -> [i] -> [(a,[i])]
toReadS a i = case parse_ mempty ((,) <$> toPhase a <*> many get) i of
  Right r -> r
  Left _ -> []

-- | Pass a list of input values to an 'Automaton'
run :: (Monoid p) => Automaton p i o a -> [i] -> Automaton p i o a
run = go where
  go a [] = a
  go a (i:r) = case beforeStep a of
    Left a' -> a'
    Right a' -> go (step a' i) r

run' :: (Monoid p) => Automaton p i o a -> [i] -> Automaton p i o a
run' = go where
  go a [] = a
  go a (i:r) = case beforeStep' a of
    Left a' -> a'
    Right a' -> go (step' a' i) r

-- | Use a 'Phase' value similarly to a parser.
parse_ :: (Monoid p, PhaserType s) => p -> s p i o a -> [i] -> Either [(p,[String])] [a]
parse_ p a i = extract mempty $ run (Count p $ toAutomaton a) i

-- | Use a 'Phase' as a parser, but consuming a single input instead of a list
parse1_ :: (Monoid p, PhaserType s) => p -> s p i o a -> i -> Either [(p,[String])] [a]
parse1_ p a i = extract p $ step (toAutomaton a) i

-- | Decompose an 'Automaton' into its component options.
options :: Automaton p i o a -> [Automaton p i o a]
options = ($ []) . go where
  go (a :+++ b) = go a . go b
  go (Yield o r) = (fmap . fmap) (Yield o) $ go r
  go (Count p r) = (fmap . fmap) (Count p) $ go r
  go a = (a :)

-- | Separate unconditional counter modifiers from an automaton. The removal
-- is visible to 'getCount'
readCount :: (Monoid p) => Automaton p i o a -> (p, Automaton p i o a)
readCount = go where
  go (Count p0 r) = let
    (p1, r') = go r
    p' = p0 `mappend` p1
    in p' `seq` (p', r')
  go (Yield o r) = let
    (p, r') = go r
    in (p, prune1 $ Yield o r')
  go a = (mempty, a)

-- | Separate the values unconditionally yielded by an automaton
outputs :: (Monoid p) => Automaton p i o a -> ([o], Automaton p i o a)
outputs = go where
  go (Yield o r) = let
    (o', r') = go r
    in (o:o', r')
  go (Count p r) = let
    (o, r') = go r
    in (o, prune1 $ Count p r')
  go a = ([], a)

-- | Run a Phaser object on input values produced by a monadic action
-- and passing the output values to another monadic function. The input action
-- should return 'Nothing' when there is no more input. If there is more than
-- one final result: the left one is chosen, and all the outputs leading to it
-- are also output.
stream :: (Monoid p, PhaserType s, Monad m) =>
  p -> s p i o a -> m (Maybe [i]) -> ([o] -> m ()) ->
  m (Either [(p,[String])] a)
stream p0 a1 r w = go (toAutomaton a1) where
  go a = do
    let (o,a1) = outputs a
    case o of
      [] -> return ()
      _ -> w o
    i <- r
    case i of
      Nothing -> fin1 a1
      Just i' -> go $ run a1 i'
  fin1 = fin2 . fst . clean mempty
  clean _ r@(Result _) = (r,True)
  clean p0 (Count p r) = let
    p' = mappend p0 p
    (r',c) = clean p' r
    in (Count p r', c)
  clean p (Yield o r) = let
    (r',c) = clean p r
    in (Yield o r', c)
  clean p (a :+++ b) = case (clean p a, clean p b) of
    (a'@(_,True),_) -> a'
    (_,b'@(_,True)) -> b'
    ((a',False),(b',False)) -> (prune1 (a' :+++ b'), False)
  clean p (GetCount n) = clean p (n p)
  clean _ a = (a,False)
  fin2 a = do
    let (o,a1) = outputs a
    w o
    return $ case extract p0 a1 of
      Right (r:_) -> Right r
      Left e -> Left e
      _ -> Left [] -- I believe this case to be unreachable, but ghc unsure.
