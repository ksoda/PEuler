import Data.List(group)

numDivisors x | null factors = 1
              | otherwise = (product . (map $ (+1).length) . group) factors
                where factors = primeFactors x

primeFactors n = factors n $ sieveOfEratosthenes
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
      | otherwise = factors n ps

sieveOfEratosthenes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

triangleNumber :: (Integral a) => a -> a
triangleNumber n = n * (n + 1) `div` 2

result = head $ filter ((>500).numDivisors) [y|x<-[1..], let y = triangleNumber x]

main = putStrLn $ show result
