module PEuler
(primesToQ
,primesPE1
,prettyPrint
,rotate
,eachCons
,combination
,findKey
,triangleNumber
,fibonacci
,sieveOfEratosthenes
,primeFactors
,numDivisors
) where
import Data.List(group)

primesToQ m = eratos [2..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])

primesToG m = 2 : sieve [3,5..m]  where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primesPE1 = 2 : sieve [3..] primesPE1
  where
    sieve xs (p:ps) | q <- p*p , (h,t) <- span (< q) xs =
                   h ++ sieve (t `minus` [q, q+p..]) ps

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

prettyPrint :: Show a => [a] -> IO()
prettyPrint = mapM_ print

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

eachCons :: Int -> [a] -> [[a]]
eachCons n xs
  |n - 1 == length xs = []
  |otherwise = take n xs : eachCons n (tail xs)

combination:: Integer->Integer->Integer
combination n k =
  let n:d:xs = frac in n`div`d
    where frac = map product [take k' $ [n, n-1..1], [1..k]]
          k' = fromIntegral k

findKey key [] = Nothing
findKey key ((k, v):xs)
  |key == k = Just v
  |otherwise = findKey key xs

triangleNumber :: (Integral a) => a -> a
triangleNumber n = n * (n + 1) `div` 2

fibonacci :: [Integer]
fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

primeFactors :: Integer -> [Integer]
primeFactors n = factors n sieveOfEratosthenes
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps) -- Vital
      | otherwise = factors n ps

numDivisors :: Integer -> Int
numDivisors x | null factors = 1
              | otherwise = (product . (map $ (+1).length) . group) factors
                where factors = primeFactors x

main = print $ take 10 fibonacci
