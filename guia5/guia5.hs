-- Ex 1
long :: [t] -> Integer
long [] = 0
long (x:xs) = 1 + long xs

lastInList :: [t] -> t
lastInList [t] = t
lastInList (x:xs) = lastInList xs

start :: [t] -> [t]
start [x, y] = [x] 
start (x:xs) = x : start xs

reverseOfList :: [t] -> [t]
reverseOfList [x] = [x]
reverseOfList l = lastInList l : reverseOfList (start l)

-- Ex 2
belongs :: (Eq t) => t -> [t] -> Bool
belongs t [x] = x == t
belongs t (x:xs)
    | x == t = True
    | otherwise = belongs t xs

allEquals :: (Eq t) => [t] -> Bool
allEquals [x] = True
allEquals (x:xs)
    | x == head xs = allEquals xs
    | otherwise = False

allDifferents :: (Eq t) => [t] -> Bool
allDifferents [x] = True
allDifferents (x:xs)
    | not (belongs x xs) = allDifferents xs
    | otherwise = False

haveRepetitions :: (Eq t) => [t] -> Bool
haveRepetitions [x] = False
haveRepetitions (x:xs)
    | belongs x xs = True
    | otherwise = haveRepetitions xs

takeOut :: (Eq t) => t -> [t] -> [t]
takeOut n [] = []
takeOut n (x:xs)
    | x == n = xs
    | otherwise = x : takeOut n xs

takeOutAll :: (Eq t) => t -> [t] -> [t]
takeOutAll n [] = []
takeOutAll n (x:xs)
    | x == n = takeOutAll n xs
    | otherwise = x : takeOutAll n xs

takeOutRepetitions :: (Eq t) => [t] -> [t]
takeOutRepetitions [] = []
takeOutRepetitions (x:xs) = x : takeOutRepetitions (takeOutAll x xs)

sameElements :: (Eq t) => [t] -> [t] -> Bool
sameElements [] [] = True
sameElements x y
    | (long (takeOutRepetitions x) == long (takeOutRepetitions y)) && (belongs (head x) y) = (sameElements (tail (takeOutRepetitions x)) (takeOutAll (head x) (takeOutRepetitions y)))
    | otherwise = False

palindrome  :: (Eq t) => [t] -> Bool
palindrome l = l == reverseOfList l

-- Ex 3
summation :: (Num t) => [t] -> t
summation [] = 0
summation (x:xs) = x + summation xs

productOfListElements :: [Integer] -> Integer
productOfListElements [] = 1
productOfListElements (x:xs) = x * productOfListElements xs

maxOfList :: [Integer] -> Integer
maxOfList [x] = x
maxOfList (x:y:xs) 
    | x > y = maxOfList (x : xs)
    | otherwise = maxOfList (y : xs)

sumN :: Integer -> [Integer] -> [Integer]
sumN _ [] = []
sumN n (x:xs) = (n + x) : sumN n xs

sumFirst :: [Integer] -> [Integer]
sumFirst x = sumN (head x) x

sumLast :: [Integer] -> [Integer]
sumLast l = sumN (lastInList l) l

pairs :: [Integer] -> [Integer]
pairs [] = []
pairs (x:xs)
    | mod x 2 == 0 = x : pairs xs
    | otherwise = pairs xs

multipleOfN :: Integer -> [Integer] -> [Integer]
multipleOfN _ [] = []
multipleOfN n (x:xs)
    | mod x n == 0 = x : multipleOfN n xs
    | otherwise = multipleOfN n xs

order :: [Integer] -> [Integer]
order [] = []
order l = minOfList l : order (takeOut (minOfList l) l)

minOfList :: [Integer] -> Integer
minOfList [x] = x
minOfList (x:y:xs) 
    | x < y = minOfList (x : xs)
    | otherwise = minOfList (y : xs)

-- Ex 4
type Texto = [Char]

takeOutDuplicateSpaces :: Texto -> Texto
takeOutDuplicateSpaces [x] = [x]
takeOutDuplicateSpaces (x:xs)
    | x == ' ' && head xs == ' ' = takeOutDuplicateSpaces xs
    | otherwise = x : takeOutDuplicateSpaces xs

contWords :: Texto -> Integer
contWords [] = 0
contWords l = contSpaces(takeOutExtraSpaces l) + 1

-- AUXILIAR FOR CONTWORDS
takeOutExtraSpaces :: Texto -> Texto
takeOutExtraSpaces l = takeOutFirstSpace (takeOutLastSpace (takeOutDuplicateSpaces l))

takeOutFirstSpace :: Texto -> Texto
takeOutFirstSpace l
    | head l == ' ' = tail l
    | otherwise = l

takeOutLastSpace :: Texto -> Texto
takeOutLastSpace l
    | lastInList l == ' ' = start l
    | otherwise = l

contSpaces :: Texto -> Integer
contSpaces [] = 0
contSpaces (x:xs)
    | x == ' ' = 1 + contSpaces xs
    | otherwise = contSpaces xs
--

wordsToList :: Texto -> [Texto]
wordsToList [] = []
wordsToList t
    | contSpaces t > 0 = firstWord (takeOutExtraSpaces t) : wordsToList (takeOutFirstWord (takeOutExtraSpaces t))
    | otherwise = [t]

-- AUXILIAR FOR WORDSTOLIST
firstWord :: Texto -> Texto
firstWord [] = []
firstWord (x:xs)
    | x == ' ' = []
    | otherwise = x : firstWord xs

takeOutFirstWord :: Texto -> Texto
takeOutFirstWord [] = []
takeOutFirstWord (x:xs)
    | x /= ' ' = takeOutFirstWord xs
    | otherwise = xs
--

largerWord :: Texto -> Texto
largerWord l
    | contSpaces (takeOutExtraSpaces l) == 0 = l
    | contLetters (firstWord (takeOutExtraSpaces l)) >= contLetters(firstWord (takeOutFirstWord (takeOutExtraSpaces l))) = takeOutExtraSpaces ( largerWord (firstWord (takeOutExtraSpaces l) ++ [' '] ++  (takeOutFirstWord ((takeOutFirstWord (takeOutExtraSpaces l))))))
    | otherwise = takeOutExtraSpaces (largerWord (firstWord ((takeOutFirstWord (takeOutExtraSpaces l))) ++ [' '] ++  (takeOutFirstWord ((takeOutFirstWord (takeOutExtraSpaces l))))))

-- AUXILIAR FOR LARGER WORLD
contLetters :: Texto -> Int
contLetters [] = 0
contLetters (x:xs) = 1 + contLetters xs
--

flatten :: [Texto] -> Texto
flatten [x] = x
flatten (x:xs) =  x ++ flatten xs

flattenWithSpaces :: [Texto] -> Texto
flattenWithSpaces [x] = x
flattenWithSpaces (x:xs) =  x ++ [' '] ++ flattenWithSpaces xs

flattenWithNSpaces :: [Texto] -> Integer -> Texto
flattenWithNSpaces [x] _ = x
flattenWithNSpaces (x:xs) n =  x ++ generateNSpaces n ++ flattenWithNSpaces xs n

--
generateNSpaces :: Integer -> Texto
generateNSpaces 0 = []
generateNSpaces n = [' '] ++ generateNSpaces (n-1)
--

-- Ex 5
acumulatedSum :: (Num t) => [t] -> [t]
acumulatedSum [x] = [x]
acumulatedSum (x:xs) =  acumulatedSum (takeOutLastInList (x:xs)) ++ [sumUntilX (long (x:xs)) (x:xs)]

takeOutLastInList :: (Num t) => [t] -> [t]
takeOutLastInList [x] = []
takeOutLastInList (x:xs) = x : takeOutLastInList xs

sumUntilX :: (Num t) => Integer -> [t] -> t
sumUntilX x l
    | long l == x = summation l
    | otherwise = sumUntilX x (takeOutLastInList l)
