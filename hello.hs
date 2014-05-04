main = do
  putStrLn "Hello"
  putStrLn "Tell me a number"
  input <- getLine
  putStrLn . show $ succ (read input :: Int) -- not show . succ
