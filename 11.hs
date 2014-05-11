main = do
  contents <- getContents
  nss = (map ((map (read::String->Int)) . words)) $ lines contents
  putStrLn $ show nss
