-- scratch.hs

module Scratch where

data List = Null' | Cons' Int List deriving (Show)

listLength :: List -> Int
listLength xs = case xs of
                 Null' -> 0
                 Cons' _ rest -> listLength rest + 1

listSum :: List -> Int
listSum xs = case xs of
               Null' -> 0
               Cons' x rest -> listSum rest + x

list = Cons' 1 (Cons' 2 (Cons' 3 Null'))
listL = listLength list
listS = listSum list

data GenericList a = Null | Cons a (GenericList a) deriving (Show)

-- HW : define a bst of integers with a find and insert function
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x
                      then x : filter' f xs
                      else filter' f xs


fold' :: (t -> a -> t) -> t -> [a] -> t
fold' _ seedVal []     = seedVal
fold' f seedVal (x:xs) = f (fold' f seedVal xs) x

sum' :: [Int] -> Int
sum' xs = fold' (+) 0 xs

product' :: [Int] -> Int
product' xs = fold' (*) 1 xs

length' :: [a] -> Int
length' xs = fold' giveOne 0 xs

giveOne :: Int -> a -> Int
giveOne x _ = 1 + x
