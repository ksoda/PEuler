import PEuler(numDivisors, triangleNumber)

result = head $ filter ((>500).numDivisors) [y|x<-[1..], let y = triangleNumber x]

main = putStrLn $ show result
