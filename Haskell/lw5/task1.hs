
-- start val -> count -> step
makeList :: Int -> Int -> Int -> [Int]
makeList start count step
    | count <= 0 = error "Count must be 1 or greater"
    | step <= 0 = error "Step must be 1 or greater"
    | otherwise = take count [alignedStart, alignedStart + step..]
    where
        alignedStart = start + (step - start `mod` step) `mod` step


main :: IO ()
main = do
    putStr "Start value: "
    startStr <- getLine
    putStr "Count of elements: "
    countStr <- getLine
    putStr "Step: "
    stepStr <- getLine
    let start = read startStr :: Int
        count = read countStr :: Int
        step = read stepStr :: Int

    print (makeList start count step)
