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
{---
Problema: hayPrimosGemelos Dos números p1 y p2 son primos gemelos si 
    -ambos son primos
    -|p_2 - p_1| = 2
    
    hayPrimosGemelos :: Integer -> Integer -> Bool
        -- requiere: { 0 < d <= h }
            -- asegura: { res = True <-> existen dos números p1 y p2 en el rango [d..h] que son primos gemelos }
---}
menorDivisorDesde2 :: Integer -> Integer -> Integer
menorDivisorDesde2 n i | mod n i == 0 = i 
                       | otherwise = menorDivisorDesde2 n (i+1)

menorDivisor2 :: Integer -> Integer
menorDivisor2 n = menorDivisorDesde2 n 2

esPrimo2 :: Integer -> Bool
esPrimo2 n = (n > 1) && (menorDivisor2 n == n)

absoluto :: Integer -> Integer
absoluto n | n < 0 = -n
           | otherwise = n

sonPrimosGemelos :: Integer -> Integer -> Bool
sonPrimosGemelos p1 p2 = (esPrimo2 p1 == True) && (esPrimo2 p2 == True) && (absoluto(p1 - p2) == 2)


hayPrimosGemelos :: Integer -> Integer -> Bool
hayPrimosGemelos d h | (d+2) > h = False -- si se pasa del rango 
                     | sonPrimosGemelos d (d+2)= True
                     | otherwise = hayPrimosGemelos (d+1) h


---EJERCICIO 2---

saturarUmbralHastaNegativo :: [Integer] -> Integer -> [Integer]
saturarUmbralHastaNegativo [] _ = []
saturarUmbralHastaNegativo (x:xs) n
    | x < 0  = []
    | (x >= 0) && (x <= n) = x : saturarUmbralHastaNegativo xs n 
    | (x >= 0) && (x > n) = n : saturarUmbralHastaNegativo xs n 
    | otherwise = saturarUmbralHastaNegativo xs n



----EJERCICIO 3------

{--

```
problema materiasTurnoTarde (cursadas: seq⟨String x String x Z x Z⟩) : seq⟨String⟩ {
  requiere: { Para toda tupla (materia, dia, inicio, fin) en cursadas: 0 <= inicio < fin <= 24 }
  asegura: { res no contiene elementos repetidos }
  asegura: { Un String m pertenece a res <=> 
    -existe alguna tupla (materia, dia, inicio, fin) en cursadas tal que: 
        -materia == m y 
        -el intervalo de cursada [inicio, fin] se superpone con el rango de la tarde [14, 17] 
            (es decir, inicio < 17 y fin > 14) }
}

materiasTurnoTarde :: [(String, String, Integer, Integer)] -&gt; [String]

--}

esHorarioTarde :: Integer -> Integer -> Bool
esHorarioTarde inicio fin = (inicio < 17) && (fin > 14)

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece m (x:xs)
    | m == x = True
    | otherwise = pertenece m xs

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) 
    | not (pertenece x xs) = x : eliminarRepetidos xs
    | otherwise = eliminarRepetidos xs


--------------------USAR ESTA, ES MEJOR, EL ALGORITMO ES MEJOR DE ENTENDER----------------------------
esPrimoAux :: Integer -> Integer -> Integer -> Bool
esPrimoAux n d h 
    | d == h = True
    | n `mod` d == 0 = False
    | otherwise = esPrimoAux n (d+1) h

esPrimo3 :: Integer -> Bool
esPrimo3 n = esPrimoAux n 2 n
----------------------------------------------------------------------------------------------------
-- ejercicios de Teórica en grupo--

-- EJERCICIO 1 -- 

mismaTemporada :: [([Char], Integer)] -> Integer -> [[Char]]
mismaTemporada [] _ = []
mismaTemporada((s,t):xs) n
    | t == n = s : mismaTemporada xs n
    | otherwise = mismaTemporada xs n

quitarTemporada :: Integer ->[([Char], Integer)] -> [([Char], Integer)]
quitarTemporada _ [] = []
quitarTemporada n ((s,t):xs)
    | t == n = quitarTemporada n xs
    | otherwise = (s,t) : quitarTemporada n xs

agruparPorCantidadDeTemporadas :: [([Char], Integer)] -> [(Integer, [[Char]])]
agruparPorCantidadDeTemporadas [] = []
agruparPorCantidadDeTemporadas((s,t):xs) = (t, mismaTemporada ((s,t):xs) t) : agruparPorCantidadDeTemporadas (quitarTemporada t xs)

-- EJERCICIO 2 -- 
sumaCuadradosPares :: Integer -> Integer
sumaCuadradosPares n
    | n == 1 = 0
    | (n `mod` 2 == 0) = (n^2) + sumaCuadradosPares (n-1)
    | otherwise = sumaCuadradosPares (n-1)

----------------------------------------------------------------------------------------------------


-----------RECUPERATORIO 2C- 2025-------------------------
-- ejercicio 4--

perteneceCaracter :: Char -> [Char] -> Bool
perteneceCaracter _ [] = False
perteneceCaracter c (x:xs)
    | c == x = True
    | otherwise = perteneceCaracter c xs

eliminarCaracteresRepetidos :: [Char] -> [Char]
eliminarCaracteresRepetidos [] = []
eliminarCaracteresRepetidos (x:xs)
    | (perteneceCaracter x xs) = eliminarCaracteresRepetidos xs
    | otherwise                = x : eliminarCaracteresRepetidos xs


contarCoincidencias :: [Char] -> [Char] -> [Char]
contarCoincidencias [] _ = []
contarCoincidencias (x:xs) s2
    | perteneceCaracter x s2 = x : contarCoincidencias xs s2
    | otherwise              = contarCoincidencias xs s2


caracteresEnComun :: [Char] -> [Char] -> [Char]
caracteresEnComun [] _ = []
caracteresEnComun (x:xs) p2 
    | perteneceCaracter x p2  =  eliminarCaracteresRepetidos(x : caracteresEnComun xs p2)
    | otherwise               = caracteresEnComun xs p2


-------------------------------------MATRICES----------------------------------------------------------------------------------------------------

maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (x:y:xs) 
    | x >= y = maximo (x:xs)
    | otherwise = maximo (y:xs)

posicion :: Integer -> [Integer] -> Integer
posicion _ [] = 0
posicion 0 (x:_) = x
posicion n ((x:xs)x:xs) = posicion (n-1) xs

maximoEnLaFila :: [[Integer]] -> Integer -> Integer
maximoEnLaFila [] _ = 0