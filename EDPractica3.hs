-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- PRACTICA 3ª (Características de la Programación Funcional)
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega:  DIA | MES | AÑO
-------------------------------------------------------------------------------
module Practica3 where

import Test.QuickCheck

-------------------------------------------------------------------------------
-- Ejercicio 1. Ejercicio Racional  
-------------------------------------------------------------------------------
data Racional = R Integer Integer
instance Show Racional where
    show (R x y) = show x' ++ "/" ++ show y'
        where R x' y' = normaliza (R x y)

-- Normaliza un racional. Devuelve su expresión irrdeducible y 
-- con signo en el numerador  (gcd x y es el mínimo común múltiplo de x e y) 
normaliza :: Racional -> Racional
normaliza (R x y) = (R (div x m) (div y m))
    where m = gcd x y

-- Suma dos racionales. Devuelve la expresion canónica del resultado 
sumaRac :: Racional -> Racional -> Racional
sumaRac (R x y) (R z t) = normaliza (R (x*t+y*z) (z*t))

-- resta. Resta dos racionales. Devuelve la expresión canónica del resultado
restaRac :: Racional -> Racional -> Racional
restaRac (R x y) (R z t) = normaliza (R (x*t-y*z) (z*t))

-- iguales. Completa la definición de ==
instance Eq Racional where
    R x y == R x' y' = normaliza (R x y) == normaliza (R x' y')

-- menor o igual. Completa la definición de <=
instance Ord Racional where
    R x y <= R x' y' = normaliza (R x y) <= normaliza (R x' y')

-------------------------------------------------------------------------------
-- Ejercicio 2. Ejercicio Dinero
-------------------------------------------------------------------------------
data Dinero = Euros Double | Dolares Double deriving Show

dolaresPorEuro ::Double
dolaresPorEuro = 1.17

-- aEuros. Concierte un dinero en euros
aEuros:: Dinero -> Dinero
aEuros (Euros x)   = (Euros x)
aEuros (Dolares x) = Euros (x / dolaresPorEuro)

-- aDolares. Convierte un dinero en dolares 
aDolares:: Dinero -> Dinero
aDolares (Dolares x) = (Dolares x)
aDolares (Euros x) = Dolares (x * dolaresPorEuro)

-- sumaDinero. Suma dos cantidades de dinero
sumaDinero :: Dinero -> Dinero -> Dinero
sumaDinero (Dolares x) (Dolares y) =aEuros( Dolares (x+y))
sumaDinero (Euros x) (Euros y) = Euros (x+y)
sumaDinero (Euros x) (Dolares y) = sumaDinero (Euros x) (aEuros (Dolares y))
sumaDinero (Dolares x) (Euros y) = sumaDinero (aEuros (Dolares x)) (Euros y)

-- sumaListaDinero. Suma una lista con dinero
sumaListaDinero :: [Dinero] -> Dinero
sumaListaDinero xs = foldr sumaDinero (Euros 0.0) xs

-------------------------------------------------------------------------------
-- Ejercicio 3. Ejercicio Complejo 
-- atan2 y x devuelve el arco cuya tangente es y/x
-------------------------------------------------------------------------------
data Complejo = C Double Double | P Double Double 

instance Show Complejo where
    show (C x y)  
        | y == 0 = show x
        | y < 0  = show x ++ show y ++ "i"
        | otherwise = show x ++ "+" ++ show y ++ "i"
    show (P m r) = show (aCartesiana (P m r))

aCartesiana :: Complejo -> Complejo
aCartesiana (C x y) = (C x y)
aCartesiana (P m r) = (C j s)
    where
        j = m * (cos r)
        s = m * (sin r)

aPolar :: Complejo -> Complejo  
aPolar (P m r) = (P m r)
aPolar (C x y) = (C a b)
    where
        a = sqrt(x*x+y*y)
        b = atan2 y x

sumaComp :: Complejo -> Complejo -> Complejo
sumaComp (C x y) (C x2 y2) = (C (x+x2) (y+y2))
sumaComp (P m r) (P m2 r2) = sumaComp (aCartesiana (P m r)) (aCartesiana (P m2 r2))
sumaComp (C x y) (P m r) = sumaComp (C x y) (aCartesiana(P m r))
sumaComp (P m r) (C x y) = sumaComp (aCartesiana(P m r)) (C x y)


prodComp :: Complejo -> Complejo -> Complejo
prodComp (C a b) (C c d) = (C (a*c - b*d) (a*d - b*c))
prodComp (P m r) (P m2 r2) = prodComp (aCartesiana (P m r)) (aCartesiana (P m2 r2))
prodComp (C a b) (P m2 r2) = prodComp (C a b) (aCartesiana (P m2 r2))
prodComp (P m r) (C a b)   = prodComp (aCartesiana (P m r)) (C a b) 

-------------------------------------------------------------------------------
-- Ejercicio 4. Ejercicio Lista 
-------------------------------------------------------------------------------
data List  a = Nil | Cons a (List a) deriving Show

ejemplo:: List Integer
ejemplo = Cons 3 (Cons 5 (Cons 1 (Cons 9 Nil)))

infixr 5 ++>
-- ++> Concatena dos listas
(++>):: List a -> List a -> List a
Nil ++> Cons ys Nil = Cons ys Nil
Cons xs Nil ++> Nil = Cons xs Nil
Cons s xs ++>  ys = Cons s (xs ++> ys)

-- filterList. Se queda con los elementos que cumplan el predicado
filterList :: (a -> Bool) -> List a -> List a 
filterList p Nil = Nil
filterList p (Cons x Nil)
    | p x == True = Cons x Nil
filterList p (Cons x xs)
    | p x == True = (Cons x Nil) ++> filterList p xs 
    | otherwise = filterList p xs

-- mapList. Transforma una lista según una función dada
mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons x Nil) = (Cons (f x) Nil) 
mapList f (Cons x xs) = (Cons (f x) Nil) ++> mapList f xs

-- aHaskell. Transforma una List a en una [a]
aHaskell :: List a -> [a]
--aHaskell xs = foldr : [] xs
aHaskell Nil = []
aHaskell (Cons x Nil) = [x]
aHaskell (Cons x xs) = x : (aHaskell xs)


-- aList. Transfurma una [a] en una List a
aList:: [a] -> List a
aList [] = Nil
aList [x] = (Cons x Nil)
aList (x:xs) = (Cons x (aList xs))

-------------------------------------------------------------------------------
-- Ejercicio 5. Ejercicio Arbol
-------------------------------------------------------------------------------
data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a) deriving Show

ejemploArbol :: Arbol Integer
ejemploArbol = Nodo (Nodo (Nodo Vacio 7 Vacio) 4 (Nodo Vacio 2 Vacio)) 6 (Nodo Vacio 9 Vacio)

-- nodos Cuenta cuántos nodos hay en el árbol
nodos :: Arbol a -> Int
nodos Vacio = 0
nodos (Nodo Vacio x Vacio) = 1
nodos (Nodo x y z) = 1 + nodos x + nodos z

-- prof. Determina la profundidad del árbol
prof :: Arbol a -> Int
prof Vacio = 0
prof (Nodo Vacio x Vacio) = 1
prof (Nodo x y z) 
    | prof x > prof z = 1 + prof x
    | otherwise = 1 + prof z 

-- espejo. Crea un arbol que es imagen especular del dado
espejo :: Arbol a -> Arbol a
espejo Vacio = Vacio
espejo (Nodo Vacio x Vacio) = (Nodo Vacio x Vacio)
espejo (Nodo x y z) = (Nodo (espejo x) y (espejo z))

-- mapArbol. Aplica la función dada a cada elemento del árbol
mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f Vacio = Vacio
mapArbol f (Nodo Vacio x Vacio) = (Nodo Vacio (f x) Vacio) 
mapArbol f (Nodo x y z) = (Nodo (mapArbol f x) (f y) (mapArbol f z))
-------------------------------------------------------------------------------
-- Ejercicio 6. Ejercicio ArbolH
-------------------------------------------------------------------------------
data ArbolH a = Hoja a | NodoH (ArbolH a) (ArbolH a) deriving Show

ejemploArbolH :: ArbolH Integer
ejemploArbolH = NodoH (NodoH (Hoja 4) (NodoH (Hoja 3) (Hoja 5))) (NodoH (NodoH (Hoja 9) (Hoja 4)) (Hoja 2))

-- sumaArbolH. Suma las hojas de un árbolh
sumaArbolH :: Num a => ArbolH a -> a
sumaArbolH (Hoja x) = 1
sumaArbolH (NodoH x y) = 1 + sumaArbolH x + sumaArbolH y

-- mapoArbolH. Aplica la función dada a cada dato del árbolH
mapArbolH :: (a -> b) -> ArbolH a  -> ArbolH b
mapArbolH f (Hoja x) = (Hoja (f x))
mapArbolH f (NodoH x y) = (NodoH (mapArbolH f x) (mapArbolH f y))

-- arbolHALista. Transforma un ArbolH a en una lista [a]
arbolHALista :: ArbolH a -> [a] 
arbolHALista (Hoja x) = [x]
arbolHALista (NodoH x y) = arbolHALista x ++ arbolHALista y
