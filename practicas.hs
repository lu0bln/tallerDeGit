-- Practica III --
-- 1
-- a) 
f :: Integer -> Integer 
f n | n == 1 = 8
    | n == 4 = 131 
    | n == 16 = 16
-- b)
g :: Integer -> Integer
g n | n == 8 = 16 
    | n == 16 = 4 
    | n == 131 = 1
-- c) 
h :: Integer -> Integer
h n = f (g n) 

k :: Integer -> Integer
k n = g (f n)

-- 2 
-- a) 
absolutoInt :: Integer -> Integer
absolutoInt n | n > 0 = n
           | otherwise = -n
-- b) preguntar consigna ? 
maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto a b | absolutoInt (a) >= absolutoInt (b) = absolutoInt (a)
                   | absolutoInt (a) <= absolutoInt (b) = absolutoInt (b)
-- C) 
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z
-- d) 
algunoEs0 :: Float -> Float -> Bool 
algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise = False
-- e)
ambosSon0 :: Float -> Float -> Bool 
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False
-- f) 
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | (x <=3 && y <=3) || (((x>3) && (x <= 7)) && ((y>3) && (y<=7))) || (x>7 && y>7) = True
                   | otherwise = False
-- g) 
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x /= y && y /= z && z/= x = x+y+z
                    | x == y && y == z && x == z = 0
                    | x == y = z
                    | x == z = y
                    | y == z = x
-- h)
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe n m | mod (n) m == 0 = True
                 | otherwise = False
-- i)
digitoUnidades :: Integer -> Integer
digitoUnidades x = mod (absolutoInt x) 10 
-- j)
digitoDecenas :: Integer -> Integer 
digitoDecenas x = div(mod (absolutoInt x) 100) 10

-- 3 nota: al dividir a por b estoy haciendo que "exista" indirectamente un k; y al hacer mod a b hago que sea entero.
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b | (- (div a b) /= 0) && ((mod (a) b) == 0) = True
                      | otherwise = False

-- 4                       
-- a)
prodInt :: (Float,Float) -> (Float,Float) -> Float
prodInt (a,b) (c,d) = a*c+b*d 
-- b)
todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (a,b) (c,d) | a < c && b < d = True
                      | otherwise = False
-- c)
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float 
distanciaPuntos (a,b) (c,d) = absoluto ( (a-c) + (b-d) )
-- d)
sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (a,b,c) = a + b + c
-- e)
sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) n | (mod (a) n == 0) && (mod(b) n == 0) && (mod(c) n == 0) = a + b + c
                             | (mod (a) n == 0) && (mod(b) n == 0) && (mod(c) n /= 0) = a + b 
                             | (mod (a) n == 0) && (mod(b) n /= 0) && (mod(c) n == 0) = a + c
                             | (mod (a) n == 0) && (mod(b) n /= 0) && (mod(c) n /= 0) = a
                             | (mod (a) n /= 0) && (mod(b) n == 0) && (mod(c) n == 0) = b + c
                             | (mod (a) n /= 0) && (mod(b) n == 0) && (mod(c) n /= 0) = b
                             | (mod (a) n /= 0) && (mod(b) n /= 0) && (mod(c) n == 0) = c
                             | otherwise = 0
-- f)
posPrimerPar :: (Integer,Integer,Integer) -> Integer
posPrimerPar (a,b,c) | mod (a) 2 == 0 = 0
                     | mod (b) 2 == 0 = 1
                     | mod (c) 2 == 0 = 2
                     | otherwise = 4
-- g)
crearPar ::  a -> b -> (a,b)
crearPar x y = (x,y)
-- h) preguntar que tipo de dato se debe usar para permitir numeros y letras (g)
invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x) 

-- 5
f1 :: Integer -> Integer
f1 n | n <= 7 = n*n 
    | n > 7 = 2*n - 1 

g1 :: Integer -> Integer
g1 n | (mod (n) 2) == 0 = (div n 2)
    | otherwise = 3*n + 1

todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (x,y,z) | (f1(x) > g1(x)) && (f1(y) > g1(y)) && (f1(z) > g1(z)) = True
                     | otherwise = False

-- 6
bisiesto :: Int -> Bool
bisiesto a | ((mod (a) 4 ) /= 0) || ((mod (a) 100 == 0) && (mod (a) 400 /=0) ) = False
           | otherwise = True

-- 7
absoluto :: Float -> Float 
absoluto n | n > 0 = n
           | otherwise = -n
distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (p0,p1,p2) (q0,q1,q2) = absoluto ( (p0-q0) + (p1-q1) + (p2-q2) )

-- 8
sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = (mod (absolutoInt x ) 10) + (mod (div (absolutoInt x) 10) 10)

comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos (a) < sumaUltimosDosDigitos (b) = 1
             | sumaUltimosDosDigitos (a) > sumaUltimosDosDigitos (b) = -1 
             | sumaUltimosDosDigitos (a) == sumaUltimosDosDigitos (b) = 0 

-- Practica IV -- 
-- repaso teorica factorial
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

-- 1 
fibonacci :: Integer ->Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci (n-1) + fibonacci (n-2)

-- 2
parteEntera :: Float -> Integer
parteEntera x | 0 <= x && x < 1 = 0
              | x >= 1 = parteEntera (x-1)+1
              | otherwise = parteEntera (x+1)-1

-- 3
esDivisible :: Integer -> Integer -> Bool
esDivisible n m | n-m < 0 = False
                | n-m == 0 = True
                | otherwise = esDivisible (n-m) m

-- 4
sumaImpares :: Integer -> Integer  
sumaImpares n | n == 1 = 1
              | n > 1 = 2*n-1 + sumaImpares (n-1)

 -- 5 
medioFactorial :: Integer -> Integer
medioFactorial n | n == 0 = 1
                 | n == 1 = 1
                 | n >= 2 = n* medioFactorial (n-2)

-- 6 
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n 
              | otherwise = sumaDigitos (div n 10) + sumaDigitos (mod n 10)

-- 7 (uso ejercicio 2i digitoUnidades y creo otro para sacarUnidades)
sacarUnidades :: Integer -> Integer
sacarUnidades n = div n 10

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = ( digitoUnidades n == digitoUnidades (sacarUnidades n) && todosDigitosIguales (sacarUnidades n))

-- 8
cantDigitos :: Integer -> Integer 
cantDigitos n | n < 10 = 1 
              | n >= 10 = 1 + cantDigitos (div n 10) 
{- iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n -i) ) )10   
-}
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | cantDigitos n == i = mod n 10 
                 | otherwise =  iesimoDigito (div n 10) i
-- 9 
esCapicua :: Integer -> Bool
esCapicua n | n >= 0 && n <10 = True 
            | iesimoDigito n 1 == iesimoDigito n (cantDigitos n) && esCapicua (sacarUnidades (sacarPrimero n)) =True 
            | otherwise = False   
            where sacarPrimero n = mod n (10^(cantDigitos n -1)) 

-- 10 
-- a) 
sumF1 :: Integer -> Integer 
sumF1 n | n == 0 = 1
        | otherwise = 2*(sumF1 (n-1)) + 1

sumF2 :: (Integer,Float) -> Float
sumF2 (n,q) |  n == 1 = q
            | otherwise = q + q* (sumF2 (n-1,q))

sumF3 :: (Integer,Float) -> Float 
sumF3 (n,q) | n == 1 = q + q^2
            | otherwise = q + q^2 + q^2*(sumF3 (n-1,q)) 

sumF4 :: (Integer,Float) -> Float
sumF4 (n,q) | n == 0 = 1
            | n == 1 = q^n + q^(n+1)
            | otherwise = q^n + q^2*(sumF4 (n-1,q))
-- 11 
-- a) fromIntegral convierte un numero de tipo int a float 
eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = (1/(fromIntegral (factorial n)))+ eAprox (n-1) 
-- b)
e :: Float 
e = eAprox 10

-- 12 revisar
raizDe2Aprox :: Integer -> Float 
raizDe2Aprox n | n == 1 = 1
               | otherwise = (2 + 1/ (raizDe2Aprox (n-1))) -1  
-- 13 
sumDei :: (Integer,Integer) -> Integer
sumDei (i,m) | m == 1 = i 
             | otherwise = i + i* (sumDei (i,m-1))

sumDoble :: (Integer,Integer) -> Integer
sumDoble (n,m) | n == 1 = m 
               | otherwise = sumDei (n,m) + sumDoble (n-1,m)

-- 14
sumaPotn :: Integer -> Integer -> Integer 
sumaPotn q n | n == 1 = q
             | otherwise = q^n + sumaPotn q (n-1)
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | q == 1 = n*m
                    | otherwise = sumaPotn q m * sumaPotn q n

-- 15 
sumaRacionalesAux :: Integer -> Integer -> Float
sumaRacionalesAux n q | q == 1 = fromIntegral n
                      | otherwise = fromIntegral n / fromIntegral q + sumaRacionalesAux n (q-1)
                
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m | n == 1 = sumaRacionalesAux n m  
                   | otherwise = sumaRacionalesAux n m + sumaRacionales (n-1) m  

-- 16 
-- a) como me pide el MENOR, lo mejor es arrancar desde ABAJO, tipo del 2 hacia arriba, y el 
-- primer natural que lo divida sera el menor divisor 
menorDivisor :: Integer -> Integer 
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorHasta n 2
menorDivisorHasta :: Integer -> Integer -> Integer
menorDivisorHasta n i |mod n i == 0 = i
                      |otherwise = menorDivisorHasta n (i+1)    
-- b) misma idea que el anterior... pero teniendo en cuenta que un primo solo es divisible por 
-- si mismo y por 1
esPrimo :: Integer -> Bool 
esPrimo n | menorDivisor n == n = True
          | otherwise = False 
-- c) 
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n /= m && esPrimo n && esPrimo m = True
                | (mod n (menorDivisor m) == 0) || (mod m (menorDivisor n) == 0) = False   
                | otherwise = sonCoprimos  (div n (menorDivisor n)) (div m (menorDivisor m)) 
-- d)
nEsimoPrimo :: Integer -> Integer 
nEsimoPrimo n | n == 1 = 2 

-- 17) res es true <-> n es algun valor de la secuencia de Fibonacci 
-- idea : si un n es algun valor de la secuencia de fibonacci entonces fibonacci n = n, sino la
-- fibonacci n-1 = n, ... , fibonacci 0 = n sino False 
esFibonacci :: Integer -> Bool
esFibonacci n | fibonacci n == n = True
              | otherwise = fibonacci (n-1) == n 
   
--                               PRACTICA V (LISTAS)
-- EJERCICIO 1 
--1)Dada una lista devuelva su cantidad de elementos
longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud [_] = 1
longitud (x:xs) = 1 + longitud xs 
--2)
ultimo :: (Eq t) => [t] -> t 
ultimo [x] = x
ultimo (x:xs) = head [ultimo xs]  
--3) 'quita el ultimo elemento de la lista' 
{-principio :: (Eq t) => [t] -> [t]
principio xs | xs == x = xs     
-}
principio :: (Eq t) => [t] -> [t]
principio [x] = []
principio (x:xs) = [x] ++ principio xs
--4)
reverso :: (Eq t) => [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso xs = [ultimo xs] ++ reverso (principio xs)
-- EJERCICIO 2 
--1) 
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (a:b) = e == a || pertenece e b  
--2) 
todosIguales :: (Eq t) => [t] -> Bool
todosIguales xs | longitud xs <= 1 = True
                | head xs /= head (tail xs) = False        
                | otherwise = todosIguales (tail xs)  
--3)
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos xs | longitud xs <= 1 = True
                  | pertenece (head xs) (tail xs)  = False
                  | otherwise = todosDistintos (tail xs)
--4)
hayRepetidos :: (Eq t) => [t] -> Bool 
hayRepetidos xs | not (todosDistintos xs) = True
                | otherwise = False 
--5) en la especificaion el requiere deberia ser que al menos contenga un elemento 
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | pertenece x xs == False = xs 
            | head xs == x = tail xs 
            | otherwise = [head xs] ++ (quitar x (tail xs))
--6)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | pertenece x xs == False = xs 
                 | otherwise = quitarTodos x (quitar x xs) 
--7) 
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos xs | hayRepetidos xs == False = xs
                     | otherwise = [head xs] ++ eliminarRepetidos (quitarTodos (head xs) xs) 
--8) dadas dos listas devuelve true <-> ambas listas contienen los mismosElementos sin tener en cuenta
-- repeticiones. NOTA: lo que hace la funcion es que evalua los elementos de una lista en otra en la cual si el primer elemento de la primer lista pertenece a la segunda
-- lista entonces hago recursion pero sacando a ese elemento de ambas listas ya que como pertenece a ambas no la necesito, asi hasta llegar a mi caso base que o bien al
-- sacar elementos me termina quedando ambas listas vacias lo cual afirma que tenian los mismos elementos o bien donde quede una lista vacia y en otra algun/os elementos
-- lo cual afirma que hay un elemento de mas en una lista, osea no tenian los mismos elementos.
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] ys = False 
mismosElementos xs ys | pertenece (head xs) ys == True = mismosElementos (quitarTodos (head xs) xs) (quitarTodos (head xs) ys)
                      | otherwise = False
--9) 
capicua :: (Eq t) => [t] -> Bool
capicua xs = reverso xs == xs

 -- EJERCICIO 3 
 --1) sumar todos los elementos enteros de una lista
sumatoria :: [Integer] -> Integer
sumatoria xs | xs == [] = 0 
             | otherwise = head xs + sumatoria (tail xs)
--2) multiplicar todos los elementos enteros de una lista
productoria :: [Integer] -> Integer
productoria xs | xs == [] = 1
               | otherwise = head xs * productoria (tail xs)
--3) devolver el maximo elemento (el numero mas grande) de una lista de numeros enteros
maximo :: [Integer] -> Integer
maximo xs | longitud (eliminarRepetidos xs) == 1 = head xs
          | head xs >= head (tail xs) = maximo (quitarTodos (head (tail xs)) xs) 
          | otherwise = maximo (quitarTodos (head xs) xs)
--4) 
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n xs | longitud xs == 1 = [n + (head xs)]
            | otherwise = [n + (head xs)] ++ sumarN n (tail xs)
--5)
sumarElPrimero :: [Integer] -> [Integer] 
sumarElPrimero xs = sumarN (head xs) xs
--6)
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (head (reverso xs)) xs
--7)
pares :: [Integer] -> [Integer] 
pares [] = []
pares xs | mod (head xs) 2 == 0 = [head xs] ++ pares (tail xs)
         | otherwise = [] ++ pares (tail xs)
--8) tengo la funcion esMultiploDe n m (?n esMultiploDe m ?) q me delveulve true si lo es y viceversa
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n xs | esMultiploDe (head xs) n == True = [head xs] ++ multiplosDeN n (tail xs)
                  | otherwise = [] ++ multiplosDeN n (tail xs)
--9) ordenar los elementos de una lista en forma CRECIENTE (Menor a Mayor). ordenarAux = DECRECIENTE
-- NOTA : si no hacia un auxiliar y mandaba reverso en la recursion de ordenarAux no daria lo mismo.
ordenarAux :: [Integer] -> [Integer]
ordenarAux [] = []
ordenarAux xs = [maximo xs] ++ ordenarAux (quitar (maximo xs) xs) 
ordenar :: [Integer] -> [Integer]
ordenar xs = reverso (ordenarAux xs)
-- fijarse de la p4 desde n esimo primo hasta ej 21
-- 4
--a) Lo que estaba fallando era que en la 2da condicion al haber dos ' ' igual solo me devolvia una
-- y la sumaba a la nueva lista lo cual esta demas pues cuando hay 2 ' ' lo q tengo q hacer es recursion
-- sin sumar ninguna ' ', pues por la 3ra condicion cuando solo haya '' y '_' entonces ya se agrega el 
-- unico ' '. 
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [letra] = [letra]
sacarBlancosRepetidos xs | head xs == ' ' &&  head (tail xs) == ' ' = sacarBlancosRepetidos (tail xs)
                         | otherwise = [head xs] ++ sacarBlancosRepetidos (tail xs)

--b) dada una lista de caracteres devuelva la cantidad de PALABRAS que tiene (lo que hice cuenta la cantidad de letras o caracteres)
contarPalabras :: [Char] -> Integer 
contarPalabras [] = 0
contarPalabras [' '] = 0
contarPalabras [letra] = 1
contarPalabras xs = contarPalabras [head xs] + contarPalabras(tail xs)

--c) dada una lista arma una nueva lista con las PALABRAS de la lista orginal
-- NOTA: necesite sacarEspacioFin y el de inicio pues el sacarBlancosRepetidos solo me eliminaba las repeticiones de mas de 2 ' ' y no contemplaba los casos donde el ' '
-- aparecia en el primer y/o ultimo elemento

sacarEspacioInicioFin :: [Char] -> [Char]
sacarEspacioInicioFin xs | head xs == ' ' = sacarEspacioInicioFin (tail xs)
                         | head (reverso xs) == ' ' = reverso (tail (reverso xs))
                         | otherwise = xs 
primerPalabra :: [Char] -> [Char]
primerPalabra [] = []
primerPalabra xs | head xs == ' ' = []
                 | otherwise = [head xs] ++ primerPalabra (tail xs)  

quitarPrimerPalabra :: [Char] -> [Char] 
quitarPrimerPalabra [] = []
quitarPrimerPalabra xs | head (sacarEspacioInicioFin (sacarBlancosRepetidos xs)) == ' ' = tail xs 
                       | otherwise = quitarPrimerPalabra (tail xs)             

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras xs = [primerPalabra (sacarEspacioInicioFin (sacarBlancosRepetidos xs))] ++ palabras (quitarPrimerPalabra xs)



