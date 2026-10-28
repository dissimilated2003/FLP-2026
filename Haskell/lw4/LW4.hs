module LW4 
    ( myTolower
    , mySize
    , mySetNull
    , myElem
    , myReplicate
    ) where

import Data.Char (chr, ord)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List


-- 1. toLower
-------------
-- модуль:          Data.Char
-- тип:             toLower :: Char -> Char
-- что делает:      переводит заглавную букву ASCII в строчную, для не-букв возвращает исходный символ
-------------
-- примеры использования:
-------------
-- ghci> toLower 'A'
-- 'a'
-- ghci> toLower '5'
-- '5'
-- ghci> toLower '!'
-- '!'
-------------
-- самописная реализация:
-------------
myTolower :: Char -> Char
myTolower c
    | c >= 'A' && c <= 'Z' = chr (ord c + 32)
    | otherwise = c


-- 2. Map.size
-------------
-- модуль:          Data.Map
-- тип:             Map k a -> Int
-- что делает:      вычисляет размер отображения
-------------
-- примеры использования:
-------------
-- ghci> Map.size (Map.fromList [("a",1),("b",2)])
-- 2
-- ghci> Map.size Map.empty
-- 0
-- ghci> Map.size (Map.fromList [(1,'x')])
-- 1
-------------
-- самописная реализация (список пар):
-------------
mySize :: Map.Map k a -> Int
mySize m = go (Map.toList m)
  where
    go []       = 0
    go (_:rest) = 1 + go rest


-- 3. Set.null
-------------
-- модуль:          Data.Set
-- тип:             Set a -> Bool
-- что делает:      проверяет, пустое ли множество
-------------
-- примеры использования:
-------------
-- ghci> Set.null (Set.fromList [])
-- True
-- ghci> Set.null (Set.fromList [[]])
-- False
-------------
-- самописная реализация:
-------------
mySetNull :: Set.Set a -> Bool
mySetNull s = go (Set.toList s)
  where
    go [] = True
    go _  = False


-- 4. elem
-------------
-- модуль:          Data.List, но работает и через prelude
-- тип:             Eq a => a -> [a] -> Bool
-- что делает:      проверяет, есть ли элемент в списке
-------------
-- примеры использования:
-------------
-- ghci> elem 3 [1,2,3]
-- True
-- ghci> elem 5 [1,2,3]
-- False
-- ghci> elem 'a' "abc"
-- True
-- ghci> elem 99 []
-- False
-- ghci> elem 5 (12, 5)
-- True
-------------
-- самописная реализация:
-------------
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys


-- 5. replicate
-------------
-- модуль:          Data.List, но работает и через Prelude
-- тип:             Int -> a -> [a]
-- что делает:      создает список из N копий элемента
-------------
-- примеры использования:
-------------
-- ghci> myReplicate 4 []
-- [[],[],[],[]]
-- ghci> myReplicate 4 "a"
-- ["a","a","a","a"]
-- ghci> myReplicate 4 'a'
-- "aaaa"
-------------
-- самописная реализация:
-------------
myReplicate :: Int -> a -> [a]
myReplicate n _
    | n <= 0 = []
myReplicate n x = x : myReplicate (n - 1) x


-- тесты:

-- вопрос 1: что вернет myReplicate 3 'a'?
-------
-- а) "3a"
-- б) "aaa"
-- в) ['a','a','a']
-- г) ["aaa"]


-- вопрос 2: что вернет mySetNull (Set.fromList [1,2,3])?
-------
-- а) True
-- б) False
-- в) Ошибка
-- г) [1,2,3]
