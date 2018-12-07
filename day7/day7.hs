import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Regex.Posix

split :: String -> (Char, Char)
split str = let (_, _, _, [a, b]) = str =~ "Step (.) must be finished before step (.) can begin." :: (String, String, String, [String]) in (head a, head b)

process str = let parsed = ((map split) . lines) str in
  let all = Set.fromList (parsed >>= (\p -> [fst p, snd p])) in
  let deps = foldl (\m -> \p -> Map.insertWith (++) (snd p) [fst p] m) Map.empty parsed in
  (show all) ++ "\n"  ++ (show deps)

main = do
  inpStr <- readFile "input.txt"
  putStrLn (process inpStr)