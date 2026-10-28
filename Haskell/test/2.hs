import System.IO
import Modulemy

ex_1  = do
   print(ex1 [1,2,4,3,5,7,6,8,10,1])
   print(ex1 [1,2,3,4,5])
   print(ex1 [1,1,1,1,1])
   print(ex1 [0, -2, 4, -6, 8])
ex_2  = do
    print(ex2 0)
    print(ex2 300)
    print(ex2 500)
    print(ex2 800)
    print(ex2 1500)
    print(ex2 2000)
ex_3  = do 
    print(ex3 [10,15,3,8,21] [2,4,3,5,7])
    print(ex3 [1,1,1] [1,1,1])
    print(ex3 [] [0,2,6,4,8])
    print(ex3 [5,5,5] [5,5])
    print(ex3 [] [])
    print(ex3 [10,20,30] [])
ex_4  = do     
      ex4 "input.txt" "out.txt"
      contents <- readFile "out.txt"
      putStr contents

   
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    startIO <- getLine
    case startIO of 
        "ex1" -> ex_1 
        "ex2" -> ex_2
        "ex3" -> ex_3
        "ex4" -> ex_4
