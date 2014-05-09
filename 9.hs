result = head [a*b*c|c<-[1..998],b<-[1..(c-1)],let a=1000-(b+c),a^2+b^2==c^2]

main = putStrLn $ show result
