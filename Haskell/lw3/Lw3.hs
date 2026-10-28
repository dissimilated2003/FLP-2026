module Lw3 where

-- задание 1 
-- Определите функцию listnums. 
-- Она берет численный аргумент n  и возвращает список всех чисел от n до 1, включительно. #2
listnums :: Int -> [Int]
listnums n
    | n < 0 = error "Number must't be negative"
    | n == 0 = []
    | otherwise = n : listnums (n - 1)

-- задание 2
-- Определите функцию secondlastlist.
-- Эта функция берет список  списков и возвращает последние элементы каждого, объединенные  в список. #2
secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (x : xs) 
    | null x = secondlastlist xs
    | otherwise = last x : secondlastlist xs

-- задание 3
-- Определите функцию myunion, которая находит объединение двух  списков. 
-- Объединением двух списков будет список содержащий элементы,  
-- которые есть по крайней мере в одном из списков. #2
myunion :: Eq a => [a] -> [a] -> [a]
myunion xs ys = myunique' (xs ++ ys)
myunique' :: Eq a => [a] -> [a]
myunique' [] = []
myunique' (x : xs)
    | x `elem` xs = myunique' xs
    | otherwise = x : myunique' xs


-- задание 4
-- Определите функцию mysubst. Получив два списка, она возвращает их разность. 
-- Разность двух списков называется список, состоящий из элементов  первого списка, 
-- которые не принадлежат второму списку. #2
mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = [] 
mysubst (x : xs) ys
    | x `elem` ys = mysubst xs ys
    | otherwise = x : mysubst xs ys

-- задание 5
-- Напишите функцию, берущую список списков и возвращающую список из N -х элементов 
-- подсписков с помощью функций map и (!!) #2
nposlist :: Int -> [[a]] -> [a]
-- nposlist n xss = map (!! n) xss
nposlist n xss
    | n < 0 = error "Index must't be negative"
    | otherwise = map (!! n) (filter (\xs -> length xs > n) xss)
