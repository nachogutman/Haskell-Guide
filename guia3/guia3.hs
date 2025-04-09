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
anyoneIsZero x y = x == 0 || y == 0

anyoneIsZeroPM :: Float -> Float -> Bool
anyoneIsZeroPM 0 _ = True
anyoneIsZeroPM _ 0 = True
anyoneIsZeroPM _ _ = False

bothAreZero :: Float -> Float -> Bool
bothAreZero x y = x == 0 && y == 0

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
isMultipleOf x y = mod x y == 0

digitUnits :: Integer -> Integer
digitUnits x = mod x 10

digitDozens :: Integer -> Integer
digitDozens x
    | div x 10 == 0 = x
    | otherwise = digitDozens(div x 10) 


-- Ex 3
-- This is the same as the ecuation the exercise give, because k = -a/b
areRelated :: Integer -> Integer -> Bool
areRelated a b = mod a b == 0

-- Ex 4
type Point2d = (Float, Float)
internalProduct :: Point2d -> Point2d -> Float
internalProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

isTupleMinor :: Point2d -> Point2d -> Bool
isTupleMinor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

distance :: Point2d -> Point2d -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

sumTriad :: (Integer, Integer, Integer) -> Integer
sumTriad (x, y, z) = x + y + z

sumJustMultiples :: (Integer, Integer, Integer) -> Integer -> Integer
sumJustMultiples (x, y, z) n
    | isMultipleOf x n && isMultipleOf y n && isMultipleOf z n = x + y + z
    | isMultipleOf x n && isMultipleOf y n = x + y
    | isMultipleOf x n && isMultipleOf z n = x + z
    | isMultipleOf y n && isMultipleOf z n = y + z
    | isMultipleOf x n = x
    | isMultipleOf y n = y
    | isMultipleOf z n = z
    | otherwise = 0   

posFirstOdd :: (Integer, Integer, Integer) -> Integer
posFirstOdd (x, y, z)
    | isMultipleOf x 2 = 1
    | isMultipleOf y 2 = 2
    | isMultipleOf z 2 = 3
    | otherwise = 4

createTuple :: a -> b -> (a, b)
createTuple a b = (a, b)

invert :: (a, b) -> (b, a)
invert (a, b) = (b, a)
