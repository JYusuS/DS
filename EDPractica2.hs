-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- PRACTICA 2ª (Características de la Programación Funcional)
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega:  DIA | MES | AÑO
--
-- Ejercicios resueltos de la Relación : ..........
--
-------------------------------------------------------------------------------
module Practica2 where

import Test.QuickCheck



-------------------------------------------------------------------------------
-- Ejercicio 4
-------------------------------------------------------------------------------
distintos :: Ord a => [a] -> Bool
distintos [] = True
distintos [x] = True
distintos (x:y:xs) 
  | x == y = False
  | otherwise = distintos(x:xs) && distintos (y:xs)

-------------------------------------------------------------------------------
-- Ejercicio 10
-------------------------------------------------------------------------------
esPerfecto :: Integer -> Bool
esPerfecto x = (sumalist (divisores2 x)) == x

sumalist :: [Integer] -> Integer
sumalist [] = 0
sumalist [x] = x
sumalist (x:xs) = x + sumalist xs

divisores2 :: Integer -> [Integer]
divisores2 x 
  | x< 0 = [ z | z <- [-x..x-1], z/=0 && mod x z == 0 ]
  | otherwise = [ z | z <- [1..x-1], mod x z == 0 ]

perfectoMenorq :: Integer -> [Integer]
perfectoMenorq x = [z | z <- [1..x], esPerfecto z]
-------------------------------------------------------------------------------
-- Ejercicio 11
-------------------------------------------------------------------------------
take' :: Int -> [a] -> [a]
take' n xs = [ snd (p,x) | (p,x) <- zip [0..n-1] xs]

drop' :: Int -> [a] -> [a]
drop' n xs = [ snd (p,x) | (p,x) <- zip [0..] xs, p > n-1] 

-------------------------------------------------------------------------------
-- Ejercicio 12
-------------------------------------------------------------------------------

concat' :: [[a]] -> [a]
concat' x = foldr (++) [] x 

-------------------------------------------------------------------------------
-- Ejercicio 13
-------------------------------------------------------------------------------
desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]
-- Qué hace?
-- genera una lista de Bool que tendrán valor True si la lista introducida
-- tiene los elementos ordenados de forma creciente 
-------------------------------------------------------------------------------
-- Ejercicio 14
-------------------------------------------------------------------------------
-- apartados a, b, e y f
-- a)
inserta :: (Ord a) => a -> [a] -> [a]
inserta x s = takeWhile (<x) s ++ [x] ++ dropWhile (<x) s


-- b)
inserta' :: (Ord a ) => a -> [a] -> [a]
inserta' x [] = [x]
inserta' x (y:ys) 
  | y<x = y : inserta' x ys
  | y>=x = x : y: ys

-- e)

ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs

-- f)  Utiliza para ello la función sorted definida en las transarencias



-------------------------------------------------------------------------------
-- Ejercicio 22
-------------------------------------------------------------------------------
binarios ::Integer -> [String]
binarios 0 = [""]
binarios x 
  | x > 0 = [ "0" ++ z | z <- binarios (x-1)] ++ [ "1" ++ z | z <- binarios (x-1)]

-------------------------------------------------------------------------------
-- Ejercicio 37
-------------------------------------------------------------------------------

type Izdo = Double
type Dcho = Double
type Epsilon = Double
type Función = Double -> Double
biparticion :: Función -> Izdo -> Dcho -> Epsilon -> Double

biparticion f a b epsilon
  | (f a) * (f b) > 0 = error "No hay cambio de signo" 
  | long <= epsilon    = c
  | f c == 0 = c
  | (f a) * (f c) < 0 = biparticion f a c epsilon
  | otherwise = biparticion f c b epsilon
    where
        long = b - a
        c = (b+a)/2

-------------------------------------------------------------

empareja :: [a] -> [b] -> [(a,b)]
empareja (x:xs) (y:ys)
  | null (x:xs) || null (y:ys) = []
  | null xs || null ys = [(x,y)]
  | otherwise = (x,y) : empareja xs ys

p_empareja x y = empareja x y == zip x y

emparejaCon :: (a->b->c) -> [a] -> [b] -> [c]
emparejaCon f (x:xs) (y:ys) 
  | null xs || null ys = f x y : []
  | otherwise = (f x y) : emparejaCon f xs ys 

p_emparejaCon f x y = emparejaCon f x y == zipWith f x y


separaRec :: (a->Bool) -> [a] -> ([a],[a])
separaRec f (x:xs)
  | f x = ( x : u, v)
  | otherwise = (u, x: v)
    where (u,v) = separaRec f xs
separaRec f [] = ([],[])


--josecsvallejo@uma.es

{-
separaRecB :: (a->Bool) -> [a] -> ([a],[a])
separaRecB f (x:xs) 
  | null xs && f x == True = ([x],[])
  | null xs && f x == False = ([],[x])
  | f x == True = ((x : separaRecB f xs ), separaRecB f xs)
  | f x == False = (separaRecB f xs,( x : separaRecB f xs))
-}

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]


buscarRec :: Eq a => a-> [(a,b)] -> [b]
buscarRec x (y:ys) 
  | x /= getClave y && null ys = []
  | x /= getClave y = buscarRec x ys
  | otherwise = getValor y
  where
    getValor (a,b) = [b] 
    getClave (a,b) = a


buscarC :: Eq a => a -> [(a,b)] -> [b]
buscarC x y = [snd (k,j) | (k,j)<-y, k==x]


valorCartera :: [(String, Double)] -> [(String, Double)] -> Double
valorCartera (x:xs) y
  | null xs && null (buscarC (getClave x) y) == False = head (buscarRec (getClave x) y) * getValor x
  | null xs && null (buscarRec (getClave x) y) = 0
  | null (buscarRec (getClave x) y) = valorCartera xs y
  | null (buscarRec (getClave x) y) == False = head (buscarRec (getClave x) y) * getValor x + (valorCartera xs y)
   where 
      getClave (a,b) = a
      getValor (a,b) = b


mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys)
  | x < y = x :  mezcla xs (y:ys)
  | x >= y = y : mezcla (x:xs) ys 

mezclaD :: Ord a => [a] -> [a] -> [a]
mezclaD [] [] = []
mezclaD [] ys = ys
mezclaD xs [] = xs
mezclaD (x:xs) (y:ys)
  | x < y = x : mezclaD xs (y:ys)
  | x >= y = y : mezclaD xs ys



takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
  | f x = []
  | f x == False = x : takeUntil f xs


digitosDe :: Integer -> [Integer]
digitosDe x
  | div x 10 /= 0 =  digitosDe (div x 10) ++ [(mod x 10)]
  | div x 10 == 0 = [x]

sumaCuadradoDigitos :: Integer -> Integer
sumaCuadradoDigitos x = cuad (digitosDe x)
  where
    cuad (x:xs)
      | null xs = x^2
      | otherwise = x^2 + cuad xs

esFeliz :: Integer -> Bool
esFeliz x = esF x x
  where
   esF x y
    | sumaCuadradoDigitos x == y = False
    | sumaCuadradoDigitos x == 1 = True
    | otherwise = esF (sumaCuadradoDigitos x) y

borrarRec :: Eq a => a -> [a] -> [a]
borrarRec x (y:ys)
  | null ys && x==y = []
  | null ys && x/=y = [y]
  | x == y = borrarRec x ys 
  | x /= y = y : (borrarRec x ys)


agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:y:xs)
  | x == y = agrupar 


aplicaRec :: a -> [(a->b)] -> [b]
aplicaRec x (y:ys)
  | null ys = [y x]
  | otherwise = y x : aplicaRec x ys
