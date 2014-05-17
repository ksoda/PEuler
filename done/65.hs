import Data.Ratio
import Data.Char

reduceFrac :: [Rational] -> [Rational]
reduceFrac (x:[]) = [x]
reduceFrac (x:y:xs) = reduceFrac $ 1/x + y : xs

eulerCnst = take 100 . (2:) . concatMap (\x-> [1,x,1]) $ [2,4..]
num = numerator . head . reduceFrac . reverse $ eulerCnst
result = sum . map digitToInt . show $ num
main = putStrLn . show $ result
