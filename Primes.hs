import Data.Array.Unboxed

primesTo m = eratos [2..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p, p+p..m])

primesToQ m = eratos [2..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])

primesToG m = 2 : sieve [3,5..m]  where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]] :: UArray Int Bool)
  where
    sieve p a
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a

-- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys
