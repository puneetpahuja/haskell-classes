-- prime.hs

module Prime where

primes :: Int -> [Int]
primes n = take n (filter prime [1..])

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime n = take 2 (factors n (sqrtInt n)) == [1]
          where
            sqrtInt n = floor (sqrt (fromIntegral n))

factors :: Int -> Int -> [Int]
factors n x = filter dividesN [1..x]
              where
                dividesN x = n `mod` x == 0


data List a = Empty | Cons a (List a)

instance Show a => Show (List a) where
  show l = "[" ++ show' l ++ "]"
    where
      show' (Cons x Empty) = show x
      show' (Cons x y) = show x ++ ", " ++ show' y
      show' Empty = ""


instance Eq' Color where
  equal Red Red = True
  equal Blue Blue = True
  equal Green Green = True
  equal _ _ = False

instance Eq' a => Eq' (List a) where
  equal Empty Empty = True
  equal (Cons x xs) (Cons y ys) = equal x y && equal xs ys
  equal _ _ = False

class (Eq' a) => Ord' a where
  compare' :: a -> a -> Ordering

{-
> primes 100
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,
107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,
223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,
337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,
457,461,463,467,479,487,491,499,503,509,521,523,541]
-}
