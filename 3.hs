primeFactors :: Integer -> [Integer]
primeFactors n = factors n sieveOfEratosthenes
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) ps -- Vital
      | otherwise = factors n ps

sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

main = putStrLn . show . maximum $ primeFactors 600851475143
