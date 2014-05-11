module PEuler
(eachCons
,primeFactors
) where

import Primes

eachCons :: Int -> [a] -> [[a]]
eachCons n xs
  |n - 1 == length xs = []
  |otherwise = take n xs : eachCons n (tail xs)

primeFactors :: (Num a) => Integer -> [a]
primeFactors n = factors n sieveOfEratosthenes
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps) -- Vital
      | otherwise = factors n ps
