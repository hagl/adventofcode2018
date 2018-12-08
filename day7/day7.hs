import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, group, find)
import Data.Char (ord)
import Text.Regex.Posix

split :: String -> (Char, Char)
split str = let (_, _, _, [a, b]) = str =~ "Step (.) must be finished before step (.) can begin." :: (String, String, String, [String]) in (head a, head b)

process str = let parsed = ((map split) . lines) str in
  let initial = Map.fromList (map (\p -> (fst p, "")) parsed) in
  let deps = foldl (\m -> \p -> Map.insertWith (++) (snd p) [fst p] m) initial parsed in
    show (loop2 (deps, []) (0, ""))

type Plan = Map.Map Char [Char]    
type Worker = [(Int, Char)]

getAvailable :: Plan -> [Char]
getAvailable = sort . (map fst) . (filter (\(k,v) -> v == "")) . Map.toList

getAvailable2 :: (Plan, Worker) -> [Char]
getAvailable2 (p, w) = let available = getAvailable p in
  let workedOn = map snd w in
  filter (\c -> not (c `elem` workedOn)) available

step2 :: (Plan, Worker) -> Int -> (Plan, Worker)
step2 (m, w) t = 
  let done = map snd (filter ((==t) . fst) w) in
  let newMap = foldl step m done in
  let newWorker = filter ((/=t) . fst) w in 
  (newMap, newWorker)

schedule :: (Plan, Worker) -> Int -> Worker
schedule (p, w) t =
  let availableWorker = 5 - length w in
  let available = (getAvailable2 (p,w)) in
  let toSchedule = take availableWorker available in
  w ++ (map (\c -> (ord c - 4 + t, c)) toSchedule)

loop2 :: (Plan, Worker) -> (Int, String) -> (Int, String)
loop2 (p, w) t = if (Map.null p) then t else 
  let next@(newPlan, _) = step2 (p, w) (fst t) in
  let newWorker = schedule (next) (fst t) in
  if (Map.null newPlan) then t else
    loop2 (newPlan, newWorker) ((minimum (map fst newWorker)), snd t ++ "\n" ++ (show newPlan) ++ "\n" ++ (show newWorker) ++ "\n") 

findNext :: Plan -> Char
findNext = head . getAvailable

step :: Plan -> Char -> Plan
step m c = Map.map (\s -> filter (\x -> x /= c) s) (Map.delete c m) 

loop :: Plan -> String -> String
loop m acc = if (Map.null m) then acc else 
  let c = findNext m in
    loop (step m c) (acc ++ [c])

main = do
  inpStr <- readFile "input.txt"
  putStrLn (process inpStr)
