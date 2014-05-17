import PEuler(prettyPrint)

readNums :: String -> [Integer]
readNums xs =  map (read::String->Integer) $ lines xs

main = do
  contents <- getContents
  putStr . take 10 . show . sum . readNums $ contents
