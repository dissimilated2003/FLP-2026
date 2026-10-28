import System.IO
import System.Environment (getArgs)
import Data.Char (isPunctuation)
import Control.Exception (evaluate)


readFileStrict :: FilePath -> IO String
readFileStrict path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    content <- hGetContents h
    evaluate (length content)
    hClose h
    return content

writeFileStrict :: FilePath -> String -> IO ()
writeFileStrict path content = do
    h <- openFile path WriteMode
    hSetEncoding h utf8
    hPutStr h content
    hClose h


--1) просмотреть содержимое файла
showInputFileContent :: FilePath -> IO ()
showInputFileContent file = do
    content <- readFileStrict file
    putStr content

--2) добавить напечатанное содержимое в конец файла
addNewText :: FilePath -> String -> IO ()
addNewText _ "" = return()
addNewText file text = do
    content <- readFileStrict file
    let newContent = content ++ (if null content || last content == '\n' then "" else "\n") ++ text ++ "\n"
    writeFileStrict file newContent --если файл не заканчивается на '\n' - добавить '\n'

--3) удаление строки с позиции N
deleteLine :: FilePath -> Int -> IO ()
deleteLine file pos = do
    content <- readFileStrict file
    let ls = lines content --разбить строку на список строк через '\n'
        newLs = take pos ls ++ drop (pos + 1) ls --склеить всё до pos и всё после
    writeFileStrict file (unlines newLs) --склеить обратно в строку через '\n'

--4) копирование с фильтрацией
--фильтр только знаки препинания (0):
filter0 :: String -> String
filter0 = filter (\c -> isPunctuation c || c == '\n')
--фильтр слова длиной <= 3 (1):
filter1 :: String -> String
filter1 content = unwords (filter (\w -> length (filter (not . isPunctuation) w) <= 3) (words content))
                                                --чистим от знаков пунктуации, чтобы 
                                        --words разбила строку на голые слова (через пробелы и \n)
copyWithFilter :: FilePath -> FilePath -> Int -> IO ()
copyWithFilter inFile outFile fMode = do
    content <- readFileStrict inFile
    let result = case fMode of
                            0 -> filter0 content
                            1 -> filter1 content
                            _ -> content
    writeFileStrict outFile result
    putStrLn ("скопировано в " ++ outFile)                    


loop :: FilePath -> IO ()
loop file = do
    putStrLn "\nМеню:"
    putStrLn "1 - просмотр файла"
    putStrLn "2 - добавить содержимое в файл"
    putStrLn "3 - удалить строку на позиции"
    putStrLn "4 - копировать файл с фильтром"
    putStrLn "0 - выход"
    putStr "Выбор: "
    hFlush stdout
    choice <- getLine
    case choice of 
        "1" -> do
            showInputFileContent file
            loop file

        "2" -> do
            putStr "Введите текст: "
            hFlush stdout
            txt <- getLine
            addNewText file txt
            loop file

        "3" -> do
            putStr "Позиция (с 0): "
            hFlush stdout
            posStr <- getLine
            let pos = read posStr :: Int
            deleteLine file pos
            loop file

        "4" -> do
            putStr "Режим фильтрации (0 / 1): "
            hFlush stdout
            filterModeStr <- getLine
            putStr "Новый файл (example.txt): "
            hFlush stdout
            newTxtFile <- getLine
            let filterMode = read filterModeStr :: Int
            copyWithFilter file newTxtFile filterMode
            loop file

        "0" -> putStrLn "Работа программы завершена"
        _ -> do
            putStrLn "Неверный выбор"
            loop file


--запуск программы:
--chcp 65001
--runghc task3.hs input.txt

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> loop file
        _ -> putStrLn "Использование: task3.hs <input.txt>"
