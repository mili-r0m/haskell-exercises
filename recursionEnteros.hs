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

maximoEnLaFila :: [[Integer]] -> Integer -> Integer
maximoEnLaFila [] _ = 0












-----------------------------------------------------ultimos ejercicios pre parcial---------------------------------------------------------------------------------------------

-------------------------- recursion sobre enteros ------------------------------------

---- ejercicio 1 ----

{--
NOTAS IMPORTANTEESSS!!!!!!

-)  div n 10 -> le saca el último digito a n
    div 1234 10 => 123
-)  mod n 10 -> muestra el últmo dígito de n 
    mod 1234 10 => 4
--}


sumaDigitosPares :: Integer -> Integer
sumaDigitosPares 0 = 0
sumaDigitosPares n 
    | (n < 10) && (mod n 2 == 0) = n
    | n < 10  = 0
    | otherwise = sumaDigitosPares(mod n 10) + sumaDigitosPares(div n 10)

---- ejercicio 2 ----

divisoresDeNdesde :: Integer -> Integer -> Integer -> [Integer]
divisoresDeNdesde 0 _ _ = []
divisoresDeNdesde n d h 
    | d > h = []
    | mod n d == 0 = d : divisoresDeNdesde n (d+1) h
    | otherwise = divisoresDeNdesde n (d+1) h

divisoresPropiosDeN :: Integer -> [Integer]
divisoresPropiosDeN n = divisoresDeNdesde n 1 (n-1)

suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs

sumaDivisoresPropios :: Integer -> Integer
sumaDivisoresPropios n = suma (divisoresPropiosDeN n)

esPar3 :: Integer -> Bool
esPar3 x = mod x 2 == 0

hayNumeroConSumaDivisoresPar :: Integer -> Integer -> Bool
hayNumeroConSumaDivisoresPar d h 
    | d > h = False    ------si se va del rango corta la recursión
    | esPar3 (sumaDivisoresPropios d) = True
    | otherwise = hayNumeroConSumaDivisoresPar (d+1) h



-------------------------- cadena de char o strings ------------------------------------

---- ejercicio 1 ----

esVocal :: Char -> Bool
esVocal x
    | x == 'a' = True
    | x == 'e' = True
    | x == 'i' = True
    | x == 'o' = True
    | x == 'u' = True
    | otherwise = False

contarConsonantes :: [Char] -> Integer
contarConsonantes [] = 0
contarConsonantes (x:xs)
    | esVocal x == True = 0 + contarConsonantes xs
    | otherwise = 1 + contarConsonantes xs

---- ejercicio 2 ----
longitud :: [Char] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


quitarBlancos :: [Char] -> [Char]
quitarBlancos [] = []
quitarBlancos (x:xs)
    | x /= ' ' = x : quitarBlancos xs
    | otherwise = quitarBlancos xs


contarApariciones :: Char -> [Char] -> Integer
contarApariciones _ [] = 0
contarApariciones c (x:xs)
    | c == x = 1 + contarApariciones c xs
    | otherwise = contarApariciones c xs

mismaFrecuenciaSinEspacios :: [Char] -> [Char] -> Bool
mismaFrecuenciaSinEspacios [] [] = False
mismaFrecuenciaSinEspacios (x:xs) (y:ys)
    | (contarApariciones x xs) == (contarApariciones y ys) = True
    | otherwise = mismaFrecuenciaSinEspacios xs ys

------------ corregir, está mal





-------------------------- matrices uwu ------------------------------------

---- ejercicio 1 ----


todosPositivos :: [Integer] -> Bool
todosPositivos [] = True
todosPositivos (x:xs)
    | x < 0 = False
    | x > 0 = todosPositivos xs
    | otherwise = todosPositivos xs


posicion2 :: Integer -> [Integer] -> Integer
posicion2 _ [] = 0
posicion2 n (x:xs) 
    | n == 1 = x
    | otherwise = posicion2 (n-1) xs
    

iesimaColumna :: Integer -> [[Integer]] -> [Integer]
iesimaColumna _ [] = []
iesimaColumna col (x:xs) = posicion2 col x : iesimaColumna col xs

esColumnaPositiva :: [[Integer]] -> Integer -> Bool
esColumnaPositiva [] _ = True
esColumnaPositiva (x:xs) col = todosPositivos(iesimaColumna col (x:xs))


---- ejercicio 2 ---- 


-------------------------- Listas y tuplas------------------------------------

---- ejercicio 1 ----

longitud2 :: [Integer] -> Integer
longitud2 [] = 0
longitud2 (x:xs) = 1 + longitud2 xs

promedio :: [Integer] -> Float
promedio [] = 0.0
promedio (x:xs) = fromIntegral(suma (x:xs)) / fromIntegral(longitud2 (x:xs))


sonMayoresAn :: [Integer] -> Integer -> Bool
sonMayoresAn [] _ = True
sonMayoresAn (x:xs) n
    | x >= n = sonMayoresAn xs n
    | otherwise = False


esBuenAlumno :: [Integer] -> Bool 
esBuenAlumno [] = False
esBuenAlumno notas = sonMayoresAn notas 4 && promedio notas >= 7.0 


buenosAlumnos :: [(String, [Integer] ) ] -> [String]
buenosAlumnos [] = []
buenosAlumnos ((alum,notas):xs) 
    |esBuenAlumno notas = alum : buenosAlumnos xs
    | otherwise = buenosAlumnos xs


-------------------------------------------------------------------------------------------------------------------------

--- recu 2C 2025 ---

--- ejercicio 1 ---

esDivisor :: Integer -> Integer -> Bool
esDivisor 0 0 = False
esDivisor a b   
    | a `mod` b == 0 = True
    | otherwise = False

divisoresPropiosAux :: Integer -> Integer -> Integer -> [Integer]
divisoresPropiosAux 0 _ _ = []
divisoresPropiosAux n d h 
    | d > h = []
    | d == h && (esDivisor n h == True) = d : divisoresPropiosAux n d h
    | d < h && (esDivisor n d == True) = d : divisoresPropiosAux n (d+1) h
    | otherwise = divisoresPropiosAux n (d+1) h

divisoresPropios :: Integer -> [Integer]
divisoresPropios n = divisoresPropiosAux n 1 (n-1)



---- ejercicio 2 ----

perteneceMateria :: String -> [String] -> Bool
perteneceMateria _ [] = False
perteneceMateria materia (x:xs)
    | materia == x = True
    | otherwise = perteneceMateria materia xs

eliminarMateriaRepe :: [String] -> [String]
eliminarMateriaRepe [] = []
eliminarMateriaRepe (x:xs) 
    | not (perteneceMateria x xs) = x : eliminarMateriaRepe xs
    | otherwise = eliminarMateriaRepe xs

reverso :: [String] -> [String]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

materiasComisionN :: [(String, String, Integer, Integer)] -> Integer -> Integer -> [String]
materiasComisionN [] _ _ = []
materiasComisionN((m,d,i,f):xs) inicio fin
    | inicio > fin = []
    | (inicio >= i) &&  (fin <= f) = eliminarMateriaRepe(m : materiasComisionN xs inicio fin)
    | otherwise = materiasComisionN xs inicio fin



---- ejercicio 3 -----

sumaNotas :: [Integer] -> Integer
sumaNotas [] = 0
sumaNotas (x:xs) = x + sumaNotas xs


longitudNotas :: [Integer] -> Integer
longitudNotas [] = 0
longitudNotas (x:xs) = 1 + longitudNotas xs

promedioNotas :: [Integer] -> Float
promedioNotas [] = 0.0
promedioNotas (x:xs) = fromIntegral(sumaNotas (x:xs)) / fromIntegral(longitudNotas (x:xs))


mejorPromedio :: [([Char], [Integer])] -> [Char]
mejorPromedio [] = []
mejorPromedio [(a,n)] = a
mejorPromedio ((a1,n1):(a2,n2):xs) 
    | promedioNotas n1 > promedioNotas n2 = mejorPromedio((a1,n1):xs)
    | promedioNotas n1 < promedioNotas n2 = mejorPromedio((a2,n2):xs)
    | promedioNotas n1 ==promedioNotas n2  = mejorPromedio((a1,n1):xs)