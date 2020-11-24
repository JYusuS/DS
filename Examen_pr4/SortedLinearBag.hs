-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 4 - Implementación y Especificación del TAD Bag
--
-- Alumno: APELLIDOS, NOMBRE
--
-------------------------------------------------------------------------------

module DataStructures.Bag.SortedLinearBag
  ( Bag         -- :: Bag a
  , empty       -- :: Bag a
  , isEmpty     -- :: Bag a -> Bool
  , insert      -- :: Ord a => a -> Bag a -> Bag a
  , delete      -- :: Ord a => a -> Bag a -> Bag a
  , occurrences -- :: Ord a => a -> Bag a -> Int
  , foldBag     -- :: Ord a => ((a,Int) -> b -> b) -> b -> Bag a -> b
  , list2Bag    -- :: Ord a => [a] -> Bag a
  , keys        -- :: Ord a => Bag a -> [a]
  , cardinal    -- :: Ord a => Bag a -> Int
  ) where

import Test.QuickCheck
import Data.List(sort, (\\))

-------------------------------------------------------------------------------
-- Implementación del TAD Bag
-------------------------------------------------------------------------------

data Bag a = Empty | Node (a, Int) (Bag a) deriving Eq

-- mantenemos los objetos de los nodos ordenados
bolsa1 :: Bag Char
bolsa1 = Node ('a', 2) (Node ('b', 3) (Node ('c', 2) (Node ('d',  1) Empty)))

{-
Se ha definido una instancia de Show de manera que la bolsa1 se verá como:
SortedLinearBag { 'a' 'a' 'b' 'b' 'b' 'c' 'c' 'd' }

Consideraremos el siguiente INVARIANTE de la representación:

   I1 : la información de los nodos estará ordenada sin duplicidades:
                   Nodo (x, ox) (Nodo (y, oy) s) ==>> x < y

   I2 : no deben aparecer nodos con el contador de ocurrencias igual <= 0
                     Nodo (x, ox) s   ==>> ox > 0

Esto permitirá generar una igualdad estructural (deriving Eq).

Todas las operaciones del TAD Bag reciben una bolsa que satisface el
invariante y, si devuelven una bolsa, ésta debe satisfacer el invariante.

-}

--------------------------------------------------------
-- EJERCICIO 1. COMPLETAR LA ESPECIFICACION
--------------------------------------------------------
------------
-- 0.10 ptos. 
------------
-- Devuelve una bolsa vacía
empty :: Bag a
empty = Empty

------------
-- 0.15 ptos. 
------------
-- Comprueba si una bolsa está vacía
isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _     = False

------------
-- 1 pto. 
------------
-- Inserta un nuevo dato en una bolsa
insert :: Ord a => a -> Bag a -> Bag a
insert x Empty = (Node (x,1) Empty )
insert x bg@(Node (y, oy) s) 
   | x == y = Node (y, oy+1) s
   | x < y = Node (x,1) bg
   | x > y = Node (y, oy) (insert x s)

------------
-- 1 pto. 
------------
-- Devuelve el número de ocurrencias de un elemento en una bolsa
-- (0 si el elemento no está)
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty = 0
occurrences x (Node (y, oy) s)
   | x == y = oy
   | x < y = 0
   | otherwise = occurrences x s

------------
-- 1 pto. 
------------
-- Borra una ocurrencia de un dato de una bolsa
-- (devuelve la bolsa original si el dato no estaba en la bolsa)
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty = Empty
delete x ny@(Node (y, oy) s)
   | x == y && oy>1 = Node (y, oy-1) s
   | x == y && oy==1 = s
   | x < y = ny
   | otherwise = Node (y,oy) (delete x s)
--------------------------------------------------------
-- EJERCICIO 2. COMPLETAR LA FUNCION
--------------------------------------------------------
--------
-- 1 pto. con plegado. 0.25 recursivo 
--------
-- Convertir una lista en una bolsa
list2Bag:: Ord a => [a] -> Bag a
list2Bag xs = foldr insert Empty xs

{-

La función list2Bag permite convertir una lista en una bolsa.
Por ejemplo:

   list2Bag "abracadabra"

devuelve la bolsa:

   Node ('a', 5) (Node ('b', 2) (Node ('c', 1) (Node ('d', 1) (Node ('r', 2) Empty))))

que se representa mediante show como:

   SortedLinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

Puedes usar list2Bag para construir bolsas para comprobar las
funciones de bolsas:

   *SortedLinearBag> insert 'a' (list2Bag "haskell")
   SortedLinearBag {'a' 'a' 'e' 'h' 'k' 'l' 'l' 's' }

   *SortedLinearBag> delete 'a' (list2Bag "haskell")
   SortedLinearBag { 'e' 'h' 'k' 'l' 'l' 's' }

   *SortedLinearBag> delete 'l' (list2Bag "haskell")
   SortedLinearBag { 'a' 'e' 'h' 'k' 'l' 's' }

   *SortedLinearBag> delete 'x' (list2Bag "haskell")
   SortedLinearBag { 'a' 'e' 'h' 'k' 'l' 'l' 's' }

   *SortedLinearBag> delete 'a' (list2Bag "haskell") == list2Bag "hskell"
   True

-}

--------------------------------------------------------
-- EJERCICIO 3. COMPLETAR LOS AXIOMAS PARA QUICKCHECK
--------------------------------------------------------
------------
-- 1 pto. 
------------
-- Selectores
isEmpty_empty = isEmpty empty == True
isEmpty_insert x s = isEmpty (insert x s) == False

occurrences_empty x = occurrences x empty == 0
occurrences_insert_1 x s = occurrences x (insert x s) == (occurrences x s) +1
occurrences_insert_2 x y s = x /= y ==> occurrences x (insert y s) == occurrences x s

------------
-- 1 pto. 
------------
-- Transformadores
delete_empty x = delete x empty == empty
delete_insert_1 x s = delete x (insert x s) == s
delete_insert_2 x y s  = x /= y ==> delete x (insert y s) == insert y (delete x s)

--------------------------------------------------------
---------------- PROBAR LOS AXIOMAS CON checkBag -------
--------------------------------------------------------

type T = Char -- Integer, etc.

checkBag = do
               putStrLn "isEmpty_empty"
               quickCheck (isEmpty_empty :: Bool)
               putStrLn "isEmpty_insert"
               quickCheck (isEmpty_insert :: T -> Bag T -> Bool)
               putStrLn "occurrences_empty"
               quickCheck (occurrences_empty :: T -> Bool)
               putStrLn "occurrences_insert_1"
               quickCheck (occurrences_insert_1 :: T -> Bag T -> Bool)
               putStrLn "occurrences_insert_2"
               quickCheck (occurrences_insert_2 :: T -> T -> Bag T -> Property)
               putStrLn "delete_empty"
               quickCheck (delete_empty :: T -> Bool)
               putStrLn "delete_insert_1"
               quickCheck (delete_insert_1 :: T -> Bag T -> Bool)
               putStrLn "delete_insert_2"
               quickCheck (delete_insert_2 :: T -> T -> Bag T -> Property)


-------------------------------------------------------------------------------
-- Plegado del TAD Bag
-------------------------------------------------------------------------------

-- función de plegado para bolsas

foldBag :: Ord a => ((a,Int) -> b -> b) -> b -> Bag a -> b
foldBag f z Empty = z
foldBag f z (Node (x,ox) s) = f (x,ox) (foldBag f z s)

--------------------------------------------------------
-- EJERCICIO 4. COMPLETAR LAS FUNCIONES
--------------------------------------------------------

------------
-- 1.25 ptos. con plegado. 0.50 recursivo
------------
-- Dada una bolsa 'bag' devuelve una lista con las
-- claves que aparecen en la bolsa
keys :: Ord a => Bag a -> [a]
keys Empty = []
keys (Node (x,ox) s) = foldBag (\(x,ox) xs -> x:xs) [] (Node (x,ox) s)

------------
-- 1.25 ptos. con plegado. 0.50 recursivo
------------
-- Determina el número de elementos totales de una bolsa
cardinal :: Ord a => Bag a -> Int
cardinal Empty = 0
cardinal (Node (x,ox) s) = foldBag (\(x,ox) n -> n+ox) 0 (Node (x,ox) s)

--------------------------------------------------------
-- EJERCICIO 5. COMPLETAR LA TABLA DE COMPLEJIDAD
--------------------------------------------------------

-------------------------------------------------------------------------------
-- Eficiencia de la implementación del TAD Bag
-------------------------------------------------------------------------------

{-

Responde a las dos siguientes preguntas sobre la eficiencia de nuestra
representación del TAD Bag.

------------
1) 1 pto. Completa la siguiente tabla indicando mediante la notación O el número
------------
de pasos que realiza cada operación del TAD Bag

    operación        números de pasos
    ---------------------------------
    empty                O(1)
    isEmpty              O(1)
    insert               O(N)
    delete               O(N)
    occurrences          O(N)


------------
2) 0.25 ptos. Nuestra representación del TAD Bag mantiene la secuencia de nodos ordenados:
------------

        Node ('a', 2) (Node ('b', 3) (Node ('c', 2) (Node ('d', 1) Empty)))

Obviamente, también es posible representar el TAD Bag por una secuencia desordenada:

        Node ('c', 2) (Node ('d', 1) (Node ('a', 2) (Node ('b', 3) Empty)))

¿Cuál de las dos representaciones es más eficiente? ¿Por qué?

La primera es más eficiente y sobretodo más útil a la hora de buscar nodos concretos, 
ya que si queremos crear funciones que modifiquen/utilicen nodos concretos, tener los nodos ordenados
simplifica mucho la búsqueda.
Un ejemplo sencillo es cuando en una bolsa (d,x,y,z,z,z,z,z,z,z,z) si buscamos 'c' sabemos a la primera iteración 
que no está ya que 'c' < 'd' y tendría complejidad 1. Sin embargo si no tuviese orden tendría complejidad N siempre



-}


--------------------------------------------------------
--------------- NO TOCAR A PARTIR DE AQUÍ --------------
--------------------------------------------------------

-- instancia de la clase Show para imprimir las bolsas
instance (Show a) => Show (Bag a) where
   show s = "SortedLinearBag { " ++ show' s
    where
       show' Empty = "}"
       show' (Node (x, ox) s) = muestra x ox ++  show' s
       muestra x 0  = ""
       muestra x ox = show x ++ ' ' : muestra x (ox-1)

       
-- generación de bolsas aleatorias para QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary =  do
                    xs <- listOf arbitrary
                    return (foldr insert empty xs)
  
  