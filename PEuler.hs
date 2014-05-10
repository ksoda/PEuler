module PEuler
(eachCons
) where

eachCons :: Int -> [a] -> [[a]]
eachCons n xs
  |n - 1 == length xs = []
  |otherwise = take n xs : eachCons n (tail xs)
