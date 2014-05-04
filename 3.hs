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
    sieve (p:xs) = p : sieve (filter ((/=0) . flip mod p) xs)

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes)
nonprimes = foldr1 f $ map g $ tail primes
  where
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

main = do
  putStrLn "Tell me a number"
  input <- getLine
  putStrLn . show . maximum $ primeFactors ( read input :: Integer)
