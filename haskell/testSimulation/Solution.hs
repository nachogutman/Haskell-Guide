module Solution where

hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c (x:xs)
    | c == fst x = True
    | otherwise = hayQueCodificar c xs


cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar _ [] _ = 0
cuantasVecesHayQueCodificar c (x:xs) m
    | hayQueCodificar c m == False = 0
    | c == x = 1 + cuantasVecesHayQueCodificar c xs m
    | otherwise = cuantasVecesHayQueCodificar c xs m

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [x] _ = x
laQueMasHayQueCodificar f m
    | sonTodosIguales f = head f 
    | cuantasVecesHayQueCodificar (head f) f m >= cuantasVecesHayQueCodificar tomarProximo f m = laQueMasHayQueCodificar (borrarTodosX tomarProximo f)  m
    | otherwise = laQueMasHayQueCodificar (tail f) m
    where tomarProximo = head (borrarTodosX (head f) f) -- Toma el siguiente valor en el caso de que los primeros se repitan. Ej "aaaacde" -> "c"

sonTodosIguales :: [Char] -> Bool
sonTodosIguales [x] = True
sonTodosIguales (x:xs)
    | x == head xs = sonTodosIguales xs
    | otherwise = False

borrarTodosX :: Char -> [Char] -> [Char]
borrarTodosX _ [] = []
borrarTodosX c (x:xs)
    |  c == x = borrarTodosX c xs
    | otherwise = x : (borrarTodosX c xs)


codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (f:fs) m
    | hayQueCodificar f m = buscoEnMapeo f m : codificarFrase fs m
    | otherwise = f : codificarFrase fs m


buscoEnMapeo :: Char -> [(Char,Char)] -> Char
buscoEnMapeo _ [x] = snd x
buscoEnMapeo c (m:ms)
    | c == fst m = snd m
    | otherwise = buscoEnMapeo c ms
