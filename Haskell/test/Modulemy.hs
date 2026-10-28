module Modulemy (
    ex1,
    ex2,
    ex3,
    ex4
) where


import Data.List (groupBy)
import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Char (toLower)
import Data.Char (isSpace)


--Напишите функцию, которая разделяет исходный список чисел на подсписки так, 
--чтобы внутри каждого подсписка шли подряд только элементы одинаковой 
--четности. Как только четность меняется (например, после четного идет 
--нечетное), текущий подсписок закрывается и начинается новый.

--ex1 [1, 2, 4, 3, 5, 7, 6, 8, 10, 1] = [[1], [2, 4], [3, 5, 7], [6, 8, 10], [1]]
--ex1 [2, 4, 6, 8] = [[2, 4, 6, 8]]
--ex1 [1, 2, 3, 4, 5] = [[1], [2], [3], [4], [5]]

ex1 :: [Int] -> [[Int]]
ex1 [] = error "error"
ex1 xs = groupBy (\a b -> even a == even b) xs



--Кирпич Эйлера — это три натуральных числа (a,b,c), для которых три величины 
--(a^2+b^2), (a^2+c^2) и (b^2+c^2) являются квадратами целых чисел. 
--Найдите кирпичи Эйлера в диапазоне длин ребер от 1 до 600, имеющие сумму 
--ребер (a+b+c), не менее заданной  суммы N.

--Например [44, 117, 240] имеют сумму 401, если заданная сумма N 
--будет 300, то такой ответ подойдет.

--В выводе указывать кирпичи Эйлера и сумму ребер (последним параметром 
--в списке). Числа должны быть упорядочены по сумме ребер. 
--Для предложенной ситуации вывод выглядит следующим образом:

--[[44, 117, 240,401 ],...]

--Ваша функция должна находить все возможные варианты согласно заданию 
--по заданному параметру N - сумме ребер.

ex2 :: Int -> [[Int]]
ex2 n
    | n < 0     = error "error"
    | otherwise = sortBy (comparing last)
                  [ [a, b, c, a + b + c]
                  | a <- [1..600]
                  , b <- [a..600]
                  , isSquare (a * a + b * b)
                  , c <- [b..600]
                  , isSquare (a * a + c * c)
                  , isSquare (b * b + c * c)
                  , a + b + c >= n
                  ]
  where
    isSquare x = let r = round (sqrt (fromIntegral x)) :: Int
                 in r * r == x


--Напишите функцию, которая принимает два списка чисел одинаковой длины и 
--возвращает новый список, содержащий индексы, на которых элемент из 
--первого списка нацело делится на соответствующий элемент из второго списка. 
--Если таких позиций нет, верните пустой список.

--ex3 [10,15,3,8,21] [2,4,3,5,7] = [0, 2, 4]
--ex3 [2, 4, 6]  [8, 4, 12] = [1]
ex3 :: [Int] -> [Int] -> [Int]
ex3 xs ys
    | length xs /= length ys = error "error"
    | otherwise = go xs ys 0
        where
            go [] _ _ = []
            go (x : xs) (y : ys) it
                | y == 0 = error "error"
                | x `mod` y == 0 = it : go xs ys (it + 1)
                | otherwise = go xs ys (it + 1)


--Напишите функцию, которая находит неуникальные строки, игнорируя регистр 
--символов (Аа) и начальные/конечные пробелы. Например, строки "Привет " и 
--"привет" должны считаться дубликатами. В выходной файл нужно записать такие 
--строки в нижнем регистре.

trimBlanksBorderline :: String -> String
trimBlanksBorderline = reverse . dropWhile isSpace . reverse . dropWhile isSpace

processString :: String -> String
processString = map toLower . trimBlanksBorderline

ex4executor :: [String] -> [String]
ex4executor xs = go (map processString xs) [] []
  where
    go [] _ _ = []
    go (str:ss) seen emitted
        | str `elem` emitted = go ss seen emitted
        | str `elem` seen    = str : go ss seen (str:emitted)
        | str `elem` ss      = str : go ss (str:seen) (str:emitted)
        | otherwise          = go ss (str:seen) emitted

ex4 :: FilePath -> FilePath -> IO()
ex4 inFile outFile = do
    content <- readFile inFile
    let result = ex4executor (lines content)
    writeFile outFile (unlines result)
