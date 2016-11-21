{-# language InstanceSigs, ExplicitForAll, TypeFamilies, FlexibleContexts #-}
-- | Hask monoidal category
module Lesson_7_2.Monoidal where

import GHC.Base

data Const a = Const deriving (Show,Eq)

instance Functor Const where
    fmap :: forall a b. (a -> b) -> Const a -> Const b
    fmap _  = coerce

newtype Identity a = Identity a deriving (Show,Eq)

instance Functor Identity where
    fmap :: forall a b. (a -> b) -> Identity a -> Identity b
    fmap = coerce

-- diagonal bifunctor (Hask functor)
data Sum f g a = LeftC (f a)  | RightC (g a) deriving (Show,Eq)

instance (Functor f, Functor g) => Functor (Sum f g)
  where
    fmap :: forall a b . (a -> b) -> Sum f g a -> Sum f g b
    fmap f (LeftC l) = LeftC $ fmap f l
    fmap f (RightC r) = RightC $ fmap f r

data Product f g a  = Product (f a)  (g a) deriving (Show,Eq)

instance (Functor f, Functor g) => Functor (Product f g) where
      fmap :: forall a b . (a -> b) -> Product f g a -> Product f g b
      fmap f (Product x y) = Product (f `fmap` x) $  f `fmap` y


