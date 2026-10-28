import System.IO
import System.Environment (getArgs)
import Data.Char (isPunctuation)


--2. Скопировать информацию из одного файла в другой, заменив знаки  
--пунктуации заданным с клавиатуры  символом. Имена файлов указываются в командной строке. #5
replacePunc :: Char -> String -> String
replacePunc repl = map (\c -> if isPunctuation c then repl else c)

firstCharFromStr :: String -> Char
firstCharFromStr "" = error "Пустая строка"
firstCharFromStr (c:_) = c


--запуск:  runghc task2.hs input.txt output.txt
--символ для замены поступает с клавы, не из args
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            putStr "Введите символ для замены: "
            hFlush stdout
            replStr <- getLine
            let repl = firstCharFromStr replStr
            content <- readFile inputFile
            let result = replacePunc repl content
            writeFile outputFile result
            putStr ("Знаки пунктуации заменены в файле " ++ outputFile)

        _ -> putStrLn "Использование: task2.hs <inputFile.txt> <outputFile.txt> <символ для замены>"  
