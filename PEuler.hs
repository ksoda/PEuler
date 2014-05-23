module PEuler
where
import Data.List(group)
import Prime

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

primeFactors :: Integer -> [Integer]
primeFactors n = factors n sieveOfEratosthenes
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
      | otherwise = factors n ps

numDivisors :: Integer -> Int
numDivisors x | null factors = 1
              | otherwise = (product . (map $ (+1).length) . group) factors
                where factors = primeFactors x

