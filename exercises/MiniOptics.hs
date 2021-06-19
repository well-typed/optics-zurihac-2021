{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-

This is a miniature implementation of the abstractions introduced by the
`optics` library, for lenses and traversals only.  It uses the van Laarhoven
representation for convenience (while the real library uses profunctors).

-}

module MiniOptics where

import Data.Kind

import qualified MiniLens

data A_Lens
data A_Traversal

newtype Optic k s t a b =
    MkOptic { unOptic :: forall f . Constraints k f => (a -> f b) -> s -> f t }

type family Constraints k :: (Type -> Type) -> Constraint where
  Constraints A_Lens      = Functor
  Constraints A_Traversal = Applicative

type Lens = Optic A_Lens
type Traversal = Optic A_Traversal


class Is k l where
  proof :: (Constraints k f => r) -> Constraints l f => r

instance Is k k where
  proof x = x

instance Is A_Lens A_Traversal where
  proof x = x

castOptic :: forall l k s t a b . Is k l => Optic k s t a b -> Optic l s t a b
castOptic (MkOptic o) = MkOptic (\ (x :: a -> f b) -> proof @k @l @f o x)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = MkOptic (MiniLens.lens get set)

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = MkOptic traverse

view :: forall k s t a b . Is k A_Lens => Optic k s t a b -> s -> a
view o = MiniLens.view x
  where
    MkOptic x = castOptic @A_Lens o

_1 :: Lens (a,x) (b,x) a b
_1 = lens fst (\(_,x) b -> (b,x))


class (Is k m, Is l m) => JoinKinds k l m | k l -> m

instance JoinKinds k k k

instance JoinKinds A_Lens A_Traversal A_Traversal
instance JoinKinds A_Traversal A_Lens A_Traversal


(%) :: forall k l m s t u v a b . JoinKinds k l m => Optic k s t u v -> Optic l u v a b -> Optic m s t a b
o1 % o2 = MkOptic (x1 . x2)
  where
    MkOptic x1 = castOptic @m o1
    MkOptic x2 = castOptic @m o2
