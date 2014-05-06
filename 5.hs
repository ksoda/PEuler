result = head [x|x<-[1 ..], (all (\d-> x`mod`d == 0) [1 .. 20])]
main = putStrLn $ show result
