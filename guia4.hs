-- Ex 1
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

-- Ex 2
entirePart :: Float -> Integer
entirePart n 
    | n < 1 = 0
    | otherwise = 1 + entirePart (n - 1)

-- Ex 3
isDivisible :: Integer -> Integer -> Bool
isDivisible x y
    | x == 0 = True
    | x < 0 = False
    | otherwise = isDivisible (x - y) y

-- Ex 4 forma ez
sumOddsWeird :: Integer -> Integer
sumOddsWeird n = n*n

-- Ex 4 recursive
sumOdds :: Integer -> Integer
sumOdds 1 = 1
sumOdds n = 2*n - 1 + sumOdds(n - 1)

-- Ex 5 
halfFact :: Integer -> Integer
halfFact 0 = 1
halfFact 1 = 1
halfFact n = halfFact(n-2) * n
