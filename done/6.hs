xs = [1..100]

sumOfSquares::[Int]->Int
sumOfSquares xs = foldl1 (+) $ map (^2) xs

squareOfSum::[Int]->Int
squareOfSum xs =  (^2) $ foldl1 (+) xs

result =  squareOfSum xs - sumOfSquares xs

main = putStrLn $ show result

