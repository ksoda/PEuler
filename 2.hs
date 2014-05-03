fibonacci :: [Integer]
fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

result =  sum(filter even (takeWhile ( < 4 * (round (10 ** 6))) fibonacci))
main = putStrLn $ show result
