import Data.List
-- 素数リスト保存

numDivisors x | null factors = 1
              | otherwise = (product . (map $ (+1).length) . group) factors
                where factors = primeFactors x

primeFactors n = factors n $ primesToG n
  where
    factors n (p:ps)
      | p > n = []
      | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
      | otherwise = factors n ps

primesToG m = 2 : sieve [3,5..m] where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

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

triangleNumber :: (Integral a) => a -> a
triangleNumber n = n * (n + 1) `div` 2

result = head $ filter ((>500).numDivisors) [y|x<-[1..], let y = triangleNumber x]

main = putStrLn $ show result
