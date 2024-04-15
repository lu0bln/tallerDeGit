-- repaso teorica factorial
factorial :: Int -> Int
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


