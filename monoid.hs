module Monoid where

import Data.Monoid ((<>))

newtype Product = Product Int deriving (Show)

data Mayb a = Nothin | Jst a deriving (Show)

instance Monoid Product where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x*y)

instance (Monoid a) => Monoid (Mayb a) where
  mempty = Nothin
  mappend Nothin x = x
  mappend x Nothin = x
  mappend (Jst x) (Jst y) = Jst (mappend x y)

newtype Max' a = Max' a deriving (Show)
newtype Min' a = Min' a deriving (Show)
newtype Any' = Any' Bool deriving (Show)
newtype All' = All' Bool deriving (Show)
newtype Last' a = Last' (Mayb a) deriving (Show)
newtype First' a = First' (Mayb a) deriving (Show)


-- instance (Bounded a, Ord a) => Monoid (Max' a) where
--   mempty = Max' (minBound :: a)
--   mappend (Max' x) (Max' y) = Max' (max x y)

instance Monoid Any' where
  mempty = Any' False
  mappend (Any' x) (Any' y) = Any' (x || y)

instance Monoid All' where
  mempty = All' True
  mappend (All' x) (All' y) = All' (x && y)

-- instance Monoid (Last' a) where
--   mempty =
