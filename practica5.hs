-- 1 Dada una lista devuelva su cantidad de elementos
--1
longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud [_] = 1
longitud (x:xs) = 1 + longitud xs 
--2
ultimo :: (Eq t) => [t] -> t 
ultimo [x] = x
ultimo (x:xs) = head [ultimo xs]  
--3 
{-principio :: (Eq t) => [t] -> [t]
principio xs | xs == x = xs     
-}
principio :: (Eq t) => [t] -> [t]
principio [x] = []
principio (x:xs) = [x] ++ principio xs
--4
reverso :: (Eq t) => [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = [ultimo xs] ++ reverso (principio xs)

