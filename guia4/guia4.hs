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

-- Ex 6
allDigitsEqual :: Integer -> Bool
allDigitsEqual x 
    | div x 10 == 0 = True
    | mod x 10 == mod (div x 10) 10 = allDigitsEqual(div x 10)
    | otherwise = False
 
-- Ex 7
cantDigits :: Integer -> Integer
cantDigits x
    | div x 10 == 0 = 1
    | otherwise = 1 + cantDigits(div x 10)

xDigit :: Integer -> Integer -> Integer
xDigit n i = mod (div n (10^((cantDigits n) - i))) 10

-- Ex 8
sumDigits :: Integer -> Integer
sumDigits x | div x 10 == 0 = x
sumDigits x = mod x 10 + sumDigits(div x 10)

-- Ex 9
isPalindromic :: Integer -> Bool
isPalindromic x 
    | (cantDigits x) == 1  = True 
    | mod x 10 == div x (10^((cantDigits x) - 1)) = isPalindromic (div (mod x (10^((cantDigits x) - 1))) 10)
    | otherwise = False

-- Ex 10
f1 :: Integer -> Integer
f1 0 = 1
f1 n = 2^n + f1 (n - 1)

f2 :: Integer -> Float -> Float
f2 1 _ = 1
f2 n q = q^n + f2 (n - 1) q

f3 :: Integer -> Float -> Float
f3 0 _ = 0
f3 n q = q^(2*n) + q^(2*n - 1) + f3 (n-1) q

-- f4

-- Ex 11
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial(n-1)

eAprox :: Integer -> Float
eAprox 0 = 1.0
eAprox n = 1.0 / (fromIntegral (factorial n)) + eAprox (n-1)

e :: Float
e = eAprox 10

-- Ex 12
sqrtFrom2 :: Integer -> Float
sqrtFrom2 1 = 2.0
sqrtFrom2 n = 2.0 + (1.0 / sqrtFrom2 (n-1))

sqrtFrom2Aprox :: Integer -> Float
sqrtFrom2Aprox n = sqrtFrom2 (n) - 1

-- Ex 13
ex13 :: Integer -> Integer -> Integer
ex13 1 q = ex13aux 1 q
ex13 n q = ex13aux n q + ex13 (n-1) q

ex13aux :: Integer -> Integer -> Integer
ex13aux n 1 = n
ex13aux n q = n^q + ex13aux n (q-1)

{--

|||||   ||||||  ||      ||||||
||  || ||    || ||     ||    ||
|||||  ||    || ||     ||    ||
||     ||    || ||     ||    ||
||      ||||||  ||||||  ||||||
                            
                            --}
