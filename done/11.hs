import Data.List

prettyPrint :: Show a => [a] -> IO()
prettyPrint = mapM_ print

readGrid :: String -> [[Int]]
readGrid xs = map (map (read::String->Int) . words) $ lines xs


fourAdjacencies :: Eq a => (Int, Int) -> [[a]] -> [[a]]
fourAdjacencies (x, y) mtx =
  map eliminate . map (adjs 4 (x, y)) $ diffs
    where
      adjs n (x, y) (dx, dy) =
        map (getElm mtx) $ take n $ iterate inc (x, y)
        where
          getElm xss (x, y) |x >= 0 && x < size && y >= 0 && y < size = Just $ xss!!y!!x
                            |otherwise = Nothing
          size = 20
          inc (x, y) = (x + dx, y + dy)
      diffs = [(dx, dy)|dx<-[-1..1], dy<-[-1..1], not(dx == 0 && dy == 0)]
      eliminate xs
        |Nothing `elem` xs = []
        |otherwise = map (\(Just a)-> a) xs


solve mtx =
  maximum $ map product $ concatMap (\pair-> fourAdjacencies pair mtx) [(x, y) |x <- [1..20], y <- [1..20]]



main = do
  contents <- getContents
  print . solve $ readGrid contents
