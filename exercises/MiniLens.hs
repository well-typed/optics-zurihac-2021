{-# LANGUAGE RankNTypes #-}

{-

This is a miniature implementation of some of the very basics of lenses and
traversals.  Why not have a go at writing one yourself, without copying this
one?

-}

module MiniLens where

import Data.Functor.Const
import Data.Functor.Identity

type LensVL s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type TraversalVL s t a b = forall f . Applicative f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> LensVL s t a b
lens get set f s = set s <$> f (get s)


viewL :: LensVL s t a b -> s -> a
viewL l s = getConst (l Const s)

overL :: LensVL s t a b -> (a -> b) -> s -> t
overL l f s = runIdentity (l (Identity . f) s)

setL :: LensVL s t a b -> b -> s -> t
setL l b = overL l (const b)

viewT :: Monoid a => TraversalVL s t a b -> s -> a
viewT l s = getConst (l Const s)


view :: ((a1 -> Const a1 b1) -> s -> Const a b2) -> s -> a
view l s = getConst (l Const s)

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f s = runIdentity (l (Identity . f) s)


_1 :: LensVL (a,x) (b,x) a b
_1 = lens fst (\(_,x) b -> (b,x))

eg1 = view _1 ('x','y')
eg2 = setL _1 'c' ('a','b')
