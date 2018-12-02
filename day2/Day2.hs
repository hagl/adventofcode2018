import System.Environment (getArgs)
import Data.List (sort, group, find)

duplicate :: Int -> [Char] -> Bool
duplicate c s = c `elem` (map length (group (sort s)))

checksum :: [String] -> Int
checksum list = twos * threes where
  twos = length (filter (duplicate 2) list)
  threes = length (filter (duplicate 3) list)

match :: String -> String -> Bool
match x = one . (filter id) . zipWith (/=) x where
  one l = (length l) == 1

findMatch :: [String] -> Maybe (String, String)
findMatch [] = Nothing
findMatch (x:xs) = case (find (match x) xs) of
  Nothing -> findMatch xs
  Just y -> Just (x, y)

printSame :: String -> String -> String
printSame a b = map fst (filter same (zip a b)) where
  same (a,b) = a == b


processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show (checksum (lines input)))
  putStrLn (show (fmap (uncurry printSame) (findMatch (lines input))))

main = do
  args <- getArgs
  case args of 
    [input] -> processFile input 
    _ -> putStrLn "error: specify input file"


test = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
test2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]