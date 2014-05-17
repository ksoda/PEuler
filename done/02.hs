import PEuler (fibonacci)

result =  sum(filter even (takeWhile (< 4 * 10^6) fibonacci))
main = putStrLn $ show result
