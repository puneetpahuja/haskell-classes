-- hamming.hs

module Hamming where

hamming :: String -> String -> Int
hamming a b = if length a /= length b
                 then -1
                 else if a == ""
                      then 0
                      else if head a /= head b
                              then 1 + hamming (tail a) (tail b)
                              else hamming (tail a) (tail b)
