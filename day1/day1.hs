import System.Environment (getArgs)

parseInt :: String -> Int
parseInt ('+':ls) = read(ls) :: Int
parseInt (s) = read(s) :: Int

calculateFrequency :: String -> Int
calculateFrequency = sum . (map parseInt) . lines 

findDuplicateFrequencyFromText :: String -> Int
findDuplicateFrequencyFromText = findDuplicateFrequency . (map parseInt) . lines

findDuplicateFrequency :: [Int] -> Int
findDuplicateFrequency input = loop 0 [0] repeatedInput where
  repeatedInput = input ++ repeatedInput
  loop f cache (x:xs) = let new = f + x in if (new `elem` cache) 
    then new
    else loop new (new:cache) xs
    
processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show (calculateFrequency input))
  putStrLn (show (findDuplicateFrequencyFromText input))

main = mainWith myFunction
  where 
    myFunction = id;

    mainWith function = do
      args <- getArgs
      case args of 
        [input] -> processFile input 
        _ -> putStrLn "error: specify input file"
