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

-- 9 
