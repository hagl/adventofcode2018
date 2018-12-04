import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.List (sort, sortBy, group, groupBy, find)
import Data.Char (digitToInt)

-- [1518-05-05 23:56] Guard #1873 begins shift
-- [1518-04-14 00:11] falls asleep
-- [1518-08-15 00:58] wakes up

data Time = Time {
  year :: Integer,
  month :: Integer,
  day :: Integer,
  hour :: Integer,
  minute :: Integer
} deriving  (Eq, Ord, Show)

data Action = Guard Integer | FallsAsleep | WakesUp  deriving Show

data Event = Event {
  time :: Time,
  action :: Action
} deriving Show

p_events :: CharParser () [Event]
p_events = endBy p_line eol

p_line = do
  time <- p_time
  char ' '
  action <- p_sleep <|> p_wake <|> p_guard
  return (Event time action)

p_sleep = do
  string "falls asleep"
  return FallsAsleep

p_wake = do
  string "wakes up"
  return WakesUp
  
p_guard = do
  string "Guard #"
  id <- integer
  string " begins shift"
  return (Guard id)

p_time = do
  char '['
  y <- integer
  string "-"
  mo <- integer
  char '-'
  d <- integer
  string " " 
  h <- integer
  char ':'
  mi <- integer
  char ']'
  return (Time y mo d h mi)

eol = char '\n'

integer = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)
  where
    base = 10
    baseDigit = digit 

processFile inputFile = do
  input <- readFile inputFile
  case parse p_events inputFile input of
    Right(events) -> process (sortBy (\e1 -> \e2 -> compare (time e1) (time e2)) events)
    Left(error) -> putStrLn (show error)

process sortedEvents = do
  putStrLn (unlines (map show (sortedEvents)))
  let processedEvents = processEvents sortedEvents
  putStrLn (unlines (map show processedEvents))
  let grouped = groupBy (\e1 -> \e2 -> (fst e1) == (fst e2)) (sortBy  (\e1 -> \e2 -> compare (fst e1) (fst e2)) processedEvents)
  putStrLn (unlines (map show (grouped)))
  
    
test = do
  processFile "input.txt"
  
main = do
  args <- getArgs
  case args of 
    [input] -> processFile input 
    _ -> putStrLn "error: specify input file"
 
processEvents :: [Event] -> [(Integer, [Integer])]
processEvents events = helper events Nothing Nothing [] [] where -- events guard id sleep-start sleep-minutes result
  helper :: [Event] -> Maybe Integer -> Maybe Integer -> [Integer] -> [(Integer, [Integer])] -> [(Integer, [Integer])]
  helper ((Event _ (Guard id)):es) Nothing _ acc1 acc2 = helper es (Just id) Nothing [] acc2
  helper (_:es) Nothing ss acc1 acc2 = helper es Nothing ss acc1 acc2
  helper ((Event _ (Guard newId)):es) (Just id) Nothing acc1 acc2 = helper es (Just newId) Nothing [] ((id, acc1):acc2)
  helper ((Event t@Time{} FallsAsleep):es) (Just id) Nothing acc1 acc2 = helper es (Just id) (Just (minute t)) acc1 acc2
  helper ((Event t@Time{} WakesUp):es) (Just id) (Just s) acc1 acc2 = helper es (Just id) Nothing ([s..(minute t)] ++ acc1) acc2
  helper [] (Just id) _ acc1 acc2 = (id, acc1):acc2