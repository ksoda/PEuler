
collatz 1 = [1]
collatz n | even n = n: collatz (n `div` 2)
          | otherwise = n: collatz (3 * n + 1)

--[10^6-1, 10^6-3 ..1]
x = 10^6 - 1
ns = [x, x-2..1]
result = flip zip ns $ map (length.collatz) ns
main = print . snd . maximum $ result
