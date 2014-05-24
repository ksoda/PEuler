import Data.Char
import Data.List
import qualified Data.Graph as G
import qualified Data.Array as A
import PEuler(eachCons)

main = do
  src <- getContents
  let pairs = concatMap f . lines $ src
      g = fmap nub . G.buildG (0, 9) $ pairs
  putStrLn . map intToDigit . filter (\x -> not $ x `elem` unrelated g) .
    reverse . G.topSort $ g
  where
    f = map (\(f:[d]) -> (d, f)) . eachCons 2 . map digitToInt
    unrelated x = (map fst . filter (\(k, v) ->  null v) . A.assocs $ x) \\
      (nub . snd . unzip . G.edges $ x)
