import qualified Data.List.PointedList.Circular as Circular
import qualified Data.Map.Strict as Map
import Data.List (foldl')

data Board = Board (Circular.PointedList Int) (Map.Map Int Int) deriving Show

play :: Board -> Int -> Board
play b@(Board list scores) marble =
  if (mod marble 23 /= 0) then 
    let newList = Circular.insert marble (Circular.moveN 1 list) in
      Board newList scores
  else
    let moved = Circular.moveN (-7) list in
    let Circular.PointedList _ removed _ = moved in
    let Just newList = Circular.deleteRight moved in
    let currentPlayer = 1 + (mod (marble - 1) (length scores)) in
    let newScores = Map.update (\s -> Just (s + removed + marble)) currentPlayer scores in
      Board newList newScores

game :: Int -> Int -> Board
game players moves =
  let emptyBoard = Board (Circular.singleton 0) (Map.fromList (map (\e -> (e, 0)) [1..players])) in 
  foldl' play emptyBoard [1..moves]

result :: Board -> Int
result (Board _ scores) = maximum scores

main = putStrLn (show (result (game 452 7078400)))
