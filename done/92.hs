import PEuler(findKey)
import Data.Char (digitToInt)

chainType :: Int -> Int
chainType n
  |(let m = next n in m  == 1 || m == 89) = next n
  |otherwise = chainType (next n)
    where next = sum . map ((^2) .digitToInt) . show

result = length . filter (==89) . map chainType $ [1..10^7-1]
main = putStrLn . show $ result
