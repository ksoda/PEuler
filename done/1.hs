result = let n = 1000 in sum [x|x<-[1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]
main = putStrLn $ show result
