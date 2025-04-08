-- Ex 1 A
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- Ex 1 B
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- Ex 1 C
h :: Integer -> Integer -- fog function
h x = f (g x)

k :: Integer -> Integer -- gof function
k x = g (f x)

-- Ex 2
absolute :: Integer -> Integer
absolute x
    | x >= 0 = x
    | otherwise = -x

maxAbsolute :: Integer -> Integer -> Integer
maxAbsolute x y
    | absolute(x) >= absolute(y) = absolute(x)
    | otherwise = absolute(y)

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | z >= x && z >= y = z

anyoneIsZero :: Float -> Float -> Bool
anyoneIsZero x y
    | x == 0 || y == 0 = True
    | otherwise = False

anyoneIsZeroPM :: Float -> Float -> Bool
anyoneIsZeroPM 0 _ = True
anyoneIsZeroPM _ 0 = True
anyoneIsZeroPM _ _ = False

bothAreZero :: Float -> Float -> Bool
bothAreZero x y
    | x == 0 && y == 0 = True
    | otherwise = False

bothAreZeroPM :: Float -> Float -> Bool
bothAreZeroPM 0 0 = True
bothAreZeroPM _ _ = False

-- (−∞, 3],(3, 7] y (7, ∞)
inSameInterval :: Float -> Float -> Bool 
inSameInterval x y
    | x <= 3 && y <= 3 = True
    | x > 3 && x <= 7 && y > 3 && y <= 7 = True
    | x > 7 && y > 7 = True
    | otherwise = False

sumDifferent :: Integer -> Integer -> Integer -> Integer
sumDifferent x y z
    | x == y && x == z = x
    | x == y && x /= z = x + z
    | x /= y && x == z = x + y
    | x /= y && y == z = x + y
    | otherwise = x + y + z 

isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf x y
    | mod x y == 0 = True
    | otherwise = False

digitUnits :: Integer -> Integer
digitUnits x = mod x 10

digitDozens :: Integer -> Integer
digitDozens x
    | div x 10 == 0 = x
    | otherwise = digitDozens(div x 10) 