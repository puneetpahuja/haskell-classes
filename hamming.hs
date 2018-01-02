-- hamming.hs

module Hamming where

hamming :: String -> String -> Int
hamming [] []         = 0
hamming [] _          = error "strings have unequal length"
hamming _ []          = error "strings have unequal length"
hamming (a:as) (b:bs) = if a /= b
                          then 1 + hamming as bs
                          else hamming as bs
