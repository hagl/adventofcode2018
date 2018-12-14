
import Data.List (find, findIndices, sort, sortBy)
import Data.Array

-- data Pos = Pos {
--   x :: Int,
--   y :: Int
-- } deriving (Eq, Ord, Show)
type Pos = (Int, Int)

data Cart = Cart Pos Char Int deriving (Eq, Ord, Show)

move :: Pos -> Char -> Pos
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move (x, y) '^' = (x, y - 1)
move (x, y) 'v' = (x, y + 1)

turn :: Char -> Char -> Int -> (Char, Int)
turn d '+' 1 = (d, 2)
turn 'v' '+' 0 = ('>', 1)
turn 'v' '+' 2 = ('<', 0)
turn '^' '+' 0 = ('<', 1)
turn '^' '+' 2 = ('>', 0)
turn '<' '+' 0 = ('v', 1)
turn '<' '+' 2 = ('^', 0)
turn '>' '+' 0 = ('^', 1)
turn '>' '+' 2 = ('v', 0)
turn '>' '/' c = ('^', c)
turn '<' '/' c = ('v', c)
turn 'v' '/' c = ('<', c)
turn '^' '/' c = ('>', c)
turn '>' '\\' c = ('v', c)
turn '<' '\\' c = ('^', c)
turn 'v' '\\' c = ('>', c)
turn '^' '\\' c = ('<', c)
turn d _ t = (d, t)

dirs =  ['v','^', '<', '>']

type Board = Array (Int, Int) Char

step :: Board -> Int -> Cart -> Cart
step board t cart@(Cart pos dir turns) =
  let newPos = move pos dir in
--  let field = if (not (newPos `elem` indices board)) then error ("XXX " ++ (show t) ++ (show cart) ++ ":" ++ (show (bounds board))) else board ! newPos in
  let field = board ! newPos in
   let (newDir, newTurns) = turn dir field turns in
  Cart newPos newDir newTurns

parseCartsChar :: Int -> (Char, Int)  -> [Cart]
parseCartsChar y (c, x) = 
  if (c `elem` dirs) then [Cart (x, y) c 0] else []
  
parseCartsLine :: (String, Int) -> [Cart]
parseCartsLine (line, y) = 
  zip line [0..((length line) - 1)] >>= (parseCartsChar y)

parseCarts :: [String] -> [Cart]
parseCarts lines =
  let zipped = zip lines [0..((length lines) - 1)] in
  zipped >>= parseCartsLine

boardChar :: Char -> Char
boardChar 'v' = '|'
boardChar '^' = '|'
boardChar '<' = '-'
boardChar '>' = '-'
boardChar c = c
  
boardLine :: Int -> Int -> String -> [((Int, Int), Char)]
boardLine y x (c:cs) = ((x, y), boardChar c) : boardLine y (x + 1) cs
boardLine _ _ [] = []

boardLines :: Int -> [String] -> [((Int, Int), Char)]
boardLines y (s:ss) = (boardLine y 0 s) ++ boardLines (y + 1) ss
boardLines _ [] = []

parseBoard :: [String] -> Int -> Board
parseBoard lines maxLength = 
  array ((0,0), (maxLength - 1, (length lines) - 1)) (boardLines 0 lines)

findCrash :: Eq a => [a] -> Maybe a
findCrash (x:rest@(y:_)) = if (x == y) then Just x else findCrash rest
findCrash (x:[]) = Nothing
findCrash [] = Nothing

sortCarts :: [Cart] -> [Cart]
sortCarts = sortBy (\(Cart (x0,y0) _ _) (Cart (x1,y1) _ _) -> compare (y0,x0) (y1,x1))

stepAllAndCheck :: Board -> [Cart] -> [Cart] -> Int -> Either (Int, Int) [Cart]
stepAllAndCheck board carts [] _ = Right (sortCarts carts)
stepAllAndCheck board moved (cart:carts) n = let newCart@(Cart p0 _ _) = step board n cart in
  let crash = find (\(Cart p _ _) -> p == p0) (moved ++ carts) in
  case crash of
    Nothing -> stepAllAndCheck board (newCart:moved) carts n
    Just (Cart pos _ _) -> Left pos

run :: Board -> [Cart] -> Int -> (Int, Int)
run board carts n =
  case stepAllAndCheck board [] carts n of
    Left pos -> pos
    Right carts -> run board carts (n+1)

--  let newCarts = map (step board n) carts in
--  let sorted = sort newCarts in
--  let crash = findCrash (map (\(Cart pos _ _) -> pos) newCarts) in
--  case crash of 
--    Just pos -> pos
--    Nothing -> run board sorted (n+1)

run0 board carts n = 
  let newCarts = map (step board n) carts in
  let sorted = sortCarts newCarts in
  let crash = findCrash (map (\(Cart pos _ _) -> pos) newCarts) in
  do
    putStrLn ("#### " ++ show n)
    putStrLn (show sorted)
    putStrLn (printCarts board sorted 149 149)
    case crash of 
      Just pos -> putStrLn ("\n\n -> " ++ show pos)
      Nothing -> run0 board sorted (n+1)

printCartsChar :: Board -> [Cart] -> Int -> Int -> Char
printCartsChar board carts x0 y0 = 
  case (find (\(Cart (x,y) _ _) -> x == x0 && y == y0) carts) of
    Just (Cart _ c _) -> c
    Nothing -> board ! (x0,y0)
--  printCarts board carts 12 6
  
printCartsLine :: Board -> [Cart] -> Int -> Int -> String
printCartsLine board carts x y0 =
   map (\x -> printCartsChar board carts x y0) [0..x] 
    
printCarts :: Board -> [Cart] -> Int -> Int -> String
printCarts board carts x y =
    unlines (map (printCartsLine board carts x) [0..y])

process inputStr = 
  let ls = lines inputStr in
  let maxLength = maximum (map length ls) in
  let carts = sortCarts (parseCarts ls) in
  let padded = map (\l -> l ++ (replicate (maxLength - length l) ' ')) ls in
  let board = parseBoard padded maxLength in
  run board carts 0

main = do
  inpStr <- readFile "input.txt"
--  putStrLn (process inpStr)
  putStrLn (show (process inpStr))
--  process inpStr
