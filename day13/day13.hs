
import Data.List (find, findIndices, sort)
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
turn '^' '+' 2 = ('<', 0)
turn '<' '+' 0 = ('v', 1)
turn '<' '+' 2 = ('^', 0)
turn '>' '+' 0 = ('^', 1)
turn '>' '+' 2 = ('v', 2)
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
  let field = if (not (pos `elem` indices board)) then error ("XXX " ++ (show t) ++ (show cart) ++ ":" ++ (show (bounds board))) else board ! pos in
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

run :: Board -> [Cart] -> Int -> (Int, Int)
run board carts n = 
  let newCarts = map (step board n) carts in
  let sorted = sort newCarts in
  let crash = findCrash (map (\(Cart pos _ _) -> pos) newCarts) in
  case crash of 
    Just pos -> pos
    Nothing -> run board sorted (n+1)

printCartsChar :: [Cart] -> Int -> Int -> Char'
printCartsChar carts x0 y0 = 
  let cart = find

printCartsLine :: [Cart] -> Int -> Int -> String
printCartsLine carts x y0 =
   map (printCartsLine carts x) [0..x] 
    
printCarts :: [Cart] -> Int -> Int -> String
printCarts carts x y =
    map (printCartsLine carts x) [0..y] 

process inputStr = 
  let ls = lines inputStr in
  let maxLength = maximum (map length ls) in
  let carts = sort (parseCarts ls) in
  let padded = map (\l -> l ++ (replicate (maxLength - length l) ' ')) ls in
  let board = parseBoard padded maxLength in
  run board carts 0

main = do
  inpStr <- readFile "input2.txt"
  putStrLn (show (process inpStr))