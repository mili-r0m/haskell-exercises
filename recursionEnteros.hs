{--
NOTAS IMPORTANTEESSS!!!!!!

-)  div n 10 -> le saca el último digito a n
    div 1234 10 => 123
-)  mod n 10 -> muestra el últmo dígito de n 
    mod 1234 10 => 4
--}


----- ejercicio 1 ---------

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci(n - 1) + fibonacci(n - 2)

----- ejercicio 3 ---------

esDivisible :: Integer -> Integer -> Bool
esDivisible a b | a `mod` b == 0 = True
                | otherwise = False

----- ejercicio 4 ---------

nEsimoImpar :: Integer -> Integer
nEsimoImpar n = 2 * n - 1
 
sumaImpares :: Integer -> Integer
sumaImpares n | n == 0 = 0
              | n > 0 = nEsimoImpar n + (sumaImpares (n-1))

----- ejercicio 6 ---------

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = (n `mod` 10 == mod (n `div` 10) 10) && todosDigitosIguales (n `div` 10) 

----- ejercicio 7 ---------

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (n `div` 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | i == cantDigitos n = n `mod` 10
                 | i < cantDigitos n = iesimoDigito (div n 10) i

----- ejercicio 8 ---------
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = sumaDigitos(n `mod` 10) + sumaDigitos(div n 10)

----- ejercicio 16 ---------
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n i | mod n i == 0 = i
                      | otherwise = menorDivisorDesde n (i+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && (menorDivisor n == n)


----- ejercicio 17 ---------
esFibo :: Integer -> Integer -> Bool
esFibo n i | fibonacci i == n = True
           | fibonacci i > n = False
           | otherwise = esFibo n (i+1)
-- i = indice ;;; n = n 

esFibonacci :: Integer -> Bool
esFibonacci n = esFibo n 0

----- ejercicio 18 ---------
esPar :: Integer -> Bool
esPar n = esDivisible n 2


--------------------------------------------------------------------------------------------
------------------------- EJERCICIOS DE PARCIAL --------------------------------------------

---EJERCICIO 1.0---

generarSecuencia :: Integer -> Integer -> Integer
generarSecuencia d h 