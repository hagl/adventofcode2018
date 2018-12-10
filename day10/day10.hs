import Text.Regex.Posix

data Point = P {
  x :: Int,
  y :: Int
} deriving (Show, Eq)

data Data = D {
  p :: Point,
  v :: Point
} deriving (Show, Eq)

-- position=<-30848, -10200> velocity=< 3,  1>

split :: String -> Data
split str = let (_, _, _, [x, y, dx, dy]) = str =~ "position=<(.+), (.+)> velocity=<(.+), (.+)>" :: (String, String, String, [String]) in 
  D (P (read x) (read y)) (P (read dx) (read dy))

stepP :: Int -> Data -> Point
stepP n (D (P x y) (P dx dy)) = P (x + (n * dx)) (y + (n * dy))

step :: Int -> [Data] -> [Point]
step n = map (stepP n)

minMaxBy :: (a -> Int) -> [a] -> (Int, Int)
minMaxBy f ps = let xs = map f ps in (minimum xs, maximum xs)

update :: [a] -> Int -> (a -> a) -> [a]
update list n f = 
  let (pre, x:post) = (take n list, drop n list) in
    pre ++ (f x):post

display :: [Point] -> [String]
display ps = let (minx, maxx) = minMaxBy x ps in
  let (miny, maxy) = minMaxBy y ps in 
  let (dx, dy) = (maxx - minx, maxy - miny) in 
  let empty = replicate (dy + 1) (replicate (dx+1) ' ') in
  (foldl (\ls -> \(P x y) -> update ls (y - miny) (\s -> update s (x - minx) (\_ -> 'X'))) empty ps)

process str = let parsed = ((map split) . lines) str in
  -- unlines (map show parsed)
  unlines (map (\n -> (show n) ++ "\n" ++ (unlines (display (step n parsed)))) [10330..10340])
  
main = do
  inpStr <- readFile "input.txt"
  putStrLn (process inpStr)
