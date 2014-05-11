result = sieveOfEratosthenes !! (10^4)
sieveOfEratosthenes :: [Integer]
sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

main = putStrLn $ show result
