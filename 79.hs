import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import PEuler(eachCons)

type Digit = Int
type First = Int
dependencies :: [(Digit, First)] -> [(Digit, Digit, [First])]
dependencies pairs =
  let empties = M.fromList . map (\d -> (d, S.empty)) $ [0..9]
      firsts = foldr insert empties pairs
  in map (\(k, adj) -> (k, k, adj)) .  M.toList . M.map S.toList $ firsts
  where insert (d, f) = M.insertWith S.union d (S.singleton f)

main = do
  src <- getContents
  let pairs = concatMap f . lines $ src
      (g, _, _) = G.graphFromEdges . dependencies $ pairs
  print . map intToDigit . reverse . G.topSort $ g
  where
    f = map (\(f:[d]) -> (d, f)) . eachCons 2 . map digitToInt

