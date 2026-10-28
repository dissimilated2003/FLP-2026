module Testpass where

--1. Определите функцию listnums. Она берет численный аргумент n  
--и возвращает список всех чисел от n до 1, включительно. #2
listnums :: Int -> [Int]
listnums n | n <= 0 = []
           | otherwise = n : listnums (n - 1)

--2. Определите функцию secondlastlist.
--Эта функция берет список  списков и возвращает последние элементы каждого,
--объединенные  в список. #2

secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (x : xs)
    | null x = secondlastlist xs
    | otherwise = last x : secondlastlist xs

--3. Определите функцию myunion, которая находит объединение двух  списков.
--Объединением двух списков будет список содержащий элементы,  
--которые есть по крайней мере в одном из списков.

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] ys = ys
myunion (x : xs) ys
    | x `elem` ys = myunion xs ys
    | otherwise = x : myunion xs ys

--4. Определите функцию mysubst. Получив два списка, она возвращает их разность.
--Разность двух списков называется список,
--состоящий из элементов  первого списка, которые не принадлежат второму списку.

mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst (x : xs) ys
    | x `elem` ys = mysubst xs ys
    | otherwise = x : mysubst xs ys

--5. Напишите функцию, берущую список списков и возвращающую список из N -х элементов
--подсписков с помощью функций map и (!!)

getNthListElements :: Int -> [[a]] -> [a]
getNthListElements n xss
    | n <= 0 = []
    | any (\xs -> length xs <= n) xss = []
    | otherwise = map (!! n) xss
