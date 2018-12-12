import Data.List (maximumBy, foldl', tails)
import Data.Maybe (maybeToList)
import Data.Array
import qualified Data.Map.Strict as Map

start = "##..##....#.#.####........##.#.#####.##..#.#..#.#...##.#####.###.##...#....##....#..###.#...#.#.#.#"

rules = Map.fromList [("##.#.", '.'), ("##.##", '.'), ("#..##", '.'), ("#.#.#", '.'), ("..#..", '#'), ("#.##.", '.'), ("##...", '#'), (".#..#", '.'), ("#.###", '.'), (".....", '.'), ("...#.", '#'), ("#..#.", '#'), ("###..", '#'), (".#...", '#'), ("###.#", '#'), ("####.", '.'), (".##.#", '#'), ("#.#..", '#'), (".###.", '#'), (".#.##", '.'), ("#####", '#'), ("....#", '.'), (".####", '.'), (".##..", '#'), ("##..#", '.'), ("#...#", '.'), ("..###", '#'), ("...##", '.'), ("#....", '.'), ("..##.", '.'), (".#.#.", '#'), ("..#.#", '#')]

step :: String -> String
step string = let seqs = filter (\l -> 5 == length l) (map (take 5) (tails ("...." ++ string ++ "...."))) in
  seqs >>= (\seq -> maybeToList (Map.lookup seq rules))

count :: (Char, Int) -> Int
count ('.', _) = 0
count ('#', x) = x

result20 = foldl (\s _ -> step s ) start [1..20]
indexed = zip result20 [-40 .. (length start)+40]
result = sum (map count indexed)

main = do
  putStrLn (show result)
