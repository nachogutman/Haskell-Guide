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

palindromic :: (Eq t) => [t] -> Bool
palindromic l = l == reverseOfList l
