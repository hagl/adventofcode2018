import System.Environment (getArgs)

calculateFrequency :: String -> Int
calculateFrequency = sum . (map parseInt) . lines where
  parseInt ('+':ls) = read(ls) :: Int
  parseInt (s) = read(s) :: Int

processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show (calculateFrequency input))

main = mainWith myFunction
  where 
    myFunction = id;

    mainWith function = do
      args <- getArgs
      case args of 
        [input] -> processFile input 
        _ -> putStrLn "error: specify input file"
