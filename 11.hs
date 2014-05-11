genMtx _ [] = []
genMtx n xs = take n xs:genMtx n (drop n xs)

mtx = genMtx 6 [1..6^2]

diffs = [(dx, dy)|dx<-[-1..1], dy<-[-1..1], not(dx == 0 && dy == 0)]


fourAdjacencies (x, y) (dx, dy) =
  map (getElm mtx) $ take 4 $ iterate f (x, y)
    where
      getElm xss (x, y) = xss!!y!!x
      f (x, y) = (x + dx, y + dy)

result = fourAdjacencies (0, 0) (1, 1)

main = do
  --contents <- getContents
  --nss = (map ((map (read::String->Int)) . words)) $ lines contents
  putStrLn $ show result
