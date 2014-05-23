module Prime
(sieveOfEratosthenes
, primesToQ
, primesPE1
, primesPE
) where

sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

primesToQ :: Integer -> [Integer]
primesToQ m = eratos [2..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])

primesPE1 ::  [Integer]
primesPE1 = 2 : sieve [3..] primesPE1
  where
    sieve xs (p:ps) | q <- p*p , (h,t) <- span (< q) xs =
                   h ++ sieve (t `minus` [q, q+p..]) ps

primesPE ::  [Integer]
primesPE = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

