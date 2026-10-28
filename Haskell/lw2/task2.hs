-- 0 задание
doMyList :: Int -> [Int]
doMyList n = if n <= 0 then [] else [n .. (n * 2 - 1)]

-- 1 задание
oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven (x:y:xs) = y : x : oddEven xs

-- 2 задание
insertElem :: [a] -> a -> Int -> [a]
insertElem [] a _ = [a]
insertElem (x:xs) a n
    | n <= 0 = x : a : xs
    | otherwise = x : insertElem xs a (n - 1)

-- 3 задание
listSumm :: Num a => [a] -> [a] -> [a]
listSumm [] [] = []
listSumm [] xs = xs
listSumm ys [] = ys
listSumm (x:xs) (y:ys) = (x + y) : listSumm xs ys

-- 4 задание 
position :: Eq a => [a] -> a -> Int
position [] _ = 0
position (x:xs) a 
    | x == a = 1 -- позиция с 0 или 1?
    | otherwise = let pos = position xs a
    in if pos == 0 then 0 else pos + 1

-- 5 задание
f :: Integer -> Integer
f n
    | n == 0 = 0
    | n < 0 = -1
    | otherwise = n + f (n-1)

-- 6 задание 
f' :: Integer -> Integer
f' n
    | n == 0 = 0
    | n < 0 = -1
    | otherwise = f n - n

main :: IO()
main = print(f' 4)
