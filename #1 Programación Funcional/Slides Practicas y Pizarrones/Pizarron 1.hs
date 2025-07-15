curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)
-- Alternativas:
-- curry f = \x -> \y -> f (x,y)
-- curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x,y) -> f x y


triple :: Float -> Float
triple = (*) 3

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<) 17

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
-- Alternativa:
-- (.) f g = \x -> f (g x)

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \x -> \y -> f y x
-- Alternativas:
-- flip f = \x y -> f y x
-- flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f = f

const :: a -> (b -> a)
const x = \_ -> x
-- Alternativas:
-- const x _ = x
-- const = \x -> \_ -> x

-- ((==0) . (flip mod 2)) 6
-- = (==0) ((flip mod 2) 6)
-- = (==0) (mod 6 2)
-- = (==0) 0
-- = True

-- maximo :: Ord a => [a] -> a
-- maximo [x] = x
-- maximo (x:xs) = if x > maximo xs then x else maximo xs

-- minimo :: Ord a => [a] -> a
-- minimo [x] = x
-- minimo (x:xs) = if x < minimo xs then x else minimo xs

-- listaMasCorta :: [[a]] -> [a]
-- listaMasCorta [xs] = xs
-- listaMasCorta (xs:xss) = if length xs < length (listaMasCorta xss)
--                          then
--                             xs
--                          else
--                             listaMasCorta xss

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun p (x:xs) = if p x rec then x else rec
    where rec = mejorSegun p xs

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>)

minimo :: Ord a => [a] -> a
maximo = mejorSegun (<)

listaMasCorta :: [[a]] -> [a]
listaMasCorta = mejorSegun (\x1 x2 -> length x1 < length x2)

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n = filter (\f -> f n == n)

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado = reverse . (map reverse)
-- Es lo mismo que:
-- reverseAnidado xs = reverse (map reverse xs)

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if even x then x*x else x)

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

suma' :: [Int] -> Int
suma' = foldr (+) 0
-- Es lo mismo que:
-- suma' = foldr (\x rec -> x + rec)  0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x rec -> if p x then x : rec else rec) []
