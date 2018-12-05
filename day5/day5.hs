import System.Environment (getArgs)
import Data.List (sort, sortBy, group, groupBy, find)
import Data.Char (toUpper, toLower)

data Time = Time {
  year :: Integer,
  month :: Integer,
  day :: Integer,
  hour :: Integer,
  minute :: Integer
} deriving  (Eq, Ord, Show)

data Action = Guard Integer | FallsAsleep | WakesUp  deriving Show

match :: Char -> Char -> Bool
match a b = (a /= toUpper a) && (b == toUpper a) || (a /= toLower a) && (b == toLower a)

process :: String -> String
process input = helper input [] where
  helper [] start = reverse start
  helper (a : []) start = reverse (a : start)
  helper (a : t@(b : as)) start
    | (match a b) = 
      case start of 
        [] -> helper as []
        s:ss -> helper (s:as) ss
    | otherwise = helper t (a : start)

processWithout :: Char -> String -> Int
processWithout c = length . process . filter (\x -> (toLower x) /= (toLower c))
    
process2 :: String -> [(Char, Int)]
process2 input = map (\c -> (c, processWithout c input)) ['a' .. 'z'] 

processFile inputFile = do
  input <- readFile inputFile
  let result = process input
  putStrLn result
  putStrLn (show (length result))
  let res2 = sortWith snd (process2 input)
  putStrLn (unlines (map show res2))

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = (sortBy (\e1 -> \e2 -> compare (f e1) (f e2)))

mostCommon :: [Integer] -> (Integer, Int)
mostCommon [] = (0, -1)
mostCommon list = (last (sortBy  (\e1 -> \e2 -> compare (snd e1) (snd e2))  (map (\l -> (head l, length l)) (groupBy (==) (sort list)))))

  -- x = [12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,53,54,55,56,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,51,52,53,54,55,42,43,44,45,30,31,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,53,54,55,56,57,58,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,34,35,36,37,43,44,45,46,47,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,51,52,53,54,55,56,57,58,59,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,11,12,13,14,57,58,59,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]
  -- y = sortBy  (\e1 -> \e2 -> compare (snd e1) (snd e2))  (map (\l -> (head l, length l)) (groupBy (==) (sort x)))
  

test = do
  processFile "input.txt"
  
main = do
  args <- getArgs
  case args of 
    [input] -> processFile input 
    _ -> putStrLn "error: specify input file"
 