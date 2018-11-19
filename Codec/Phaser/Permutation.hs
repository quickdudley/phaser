{-# Language GADTs, ScopedTypeVariables #-}
module Codec.Phaser.Permutation (
  Permutable,
  runPermutable,
  term
 ) where

import Codec.Phaser.Core
import Control.Applicative

data Permutable p c t a where
  Term :: Phase p c t a -> Permutable p c t a
  Filled :: a -> Permutable p c t a
  (:<*>) :: Permutable p c t (a -> b) -> Permutable p c t a -> Permutable p c t b

instance Functor (Permutable p c t) where
  fmap f (Term p) = Term (fmap f p)
  fmap f (Filled a) = Filled (f a)
  fmap f (l :<*> r) = (fmap . fmap) f l :<*> r

instance Applicative (Permutable p c t) where
  pure = Filled
  (<*>) = (:<*>)

-- | Create a 'Phase' which runs the constituent terms of the 'Permutable'
-- in every order in which they succeed, running a separator 'Phase' between
-- each term which consumes input.
runPermutable :: forall p c t a b . Monoid p => Phase p c t b -> Permutable p c t a -> Phase p c t a
runPermutable sep = go0 where
  go0 p = resolve p <|> (fill1 p >>= go)
  go p = resolve p <|> (sep >> fill1 p >>= go)
  resolve :: Permutable p c t x -> Phase p c t x
  resolve (Term p) = fromAutomaton $ starve $ toAutomaton p
  resolve (Filled a) = pure a
  resolve (l :<*> r) = resolve l <*> resolve r
  fill1 :: Permutable p c t x -> Phase p c t (Permutable p c t x)
  fill1 (Term p) = Filled <$> p
  fill1 (Filled _) = empty
  fill1 (l :<*> r) = (flip simplify r <$> fill1 l) <|> (simplify l <$> fill1 r)
  simplify (Filled l) r = fmap l r
  simplify l  (Filled r) = fmap ($ r) l
  simplify l r = l :<*> r

term :: Phase p c t a -> Permutable p c t a
term = Term
