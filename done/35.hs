import PEuler(rotate)
import Prime(primesPE1)
import Data.Char

primes = takeWhile (<10^6) primesPE1
result = length . filter circular $ primes
  where
    circular x =
      and . map (flip elem primes) . map (read :: String->Integer) $ numStrs
      where seed = map digitToInt . show $ x
            numStrs = (map.map) intToDigit . take (length seed) . iterate rotate $ seed

main = putStrLn . show $ result
