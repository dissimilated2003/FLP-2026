doubleMe x = x ++ x
doubleUs x y = doubleMe x + doubleMe y

b = [[1,2,3], [4,5,6], [7,8,9]]

smallNumber x = if x > 100 then x else x + x

-- задание 1
ch1 = snd (fst ((1, 'a'), "abc"))

-- задание 2
b1 = head (tail ['a', 'b', 'c'])
b2 = head (tail (head [['a', 'b'], ['c','d']]))
b3 = head (tail (head (tail [['a', 'c', 'd'], ['a','b']])))
b4 = head (head (tail [['a','d'], ['b', 'c']]))

-- задание 3
seq1 = [1, 3 .. 39]
seq2 = [x | x <- [1 .. 40], odd x]
seq3 = [x | x <- [1 .. 40], x `mod` 2 /= 0]

-- задание 4
tr n = n * (n + 1) `div` 2
t4 = map tr [1 .. 50]

-- задание 5
pyr n = n * (n + 1) * (n + 2) `div` 6
t5 = map pyr [1 .. 50]


main :: IO()
main = print(ch1)
