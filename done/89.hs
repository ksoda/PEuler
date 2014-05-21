import Data.Char(digitToInt)

unit 'I' = 1
unit 'V' = 5
unit 'X' = 10
unit 'L' = 50
unit 'C' = 100
unit 'D' = 500
unit 'M' = 1000

fromRoman :: String -> Int
fromRoman [c] = unit c
fromRoman (c:d:cs) = sign*unit c + fromRoman(d:cs)
  where sign = if unit c < unit d then -1 else 1

lengthOfRoman :: Int -> Int
lengthOfRoman ns = q + (sum . map rsize . toDigits $ r)
  where (q, r) = ns `divMod` 1000
        toDigits = map digitToInt . show
        rsize d | d==9 || d==4 = 2
                | d>4 = d-4
                | otherwise = d

main = do
  contents <- getContents
  print . sum . map s . lines $ contents
    where s ns = length ns - (lengthOfRoman . fromRoman $ ns)
