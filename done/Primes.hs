module Primes
(sieveOfEratosthenes
,primesToG
,primeFactors
)where

import Data.Array.Unboxed

sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

-- p < m**0.5 < q, m = pq, p < q
primesToQ m = eratos [2..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])

primesToG m = 2 : sieve [3,5..m]  where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]] :: UArray Int Bool)
  where
    sieve p a
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a

-- ordered lists, difference and union
-- O(|x U y|)
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys


primeFactors n = factors n $ primesToG n
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps) -- Vital
      | otherwise = factors n ps
