palindromes = filter isPalindrome $ map show mults
  where
    n = 10^3
    mults = [x * y|x <- [n-1, n-2 .. n`div`10], y <- [x, x-1 ..n`div`10]]
    isPalindrome s = s == reverse s

result = maximum $ map (read :: String->Int) palindromes
main = putStrLn $ show result
