import qualified Data.Map.Strict as Map
import Data.List (foldl')

data Board = Board [Int] Int (Map.Map Int Int) deriving Show

advance :: Board -> Int -> Int
advance (Board l c _) d = mod (c + d) (length l)


play :: Board -> Int -> Board
play b@(Board list current scores) marble =
  if (mod marble 23 /= 0) then 
    let insertPos = (advance b 1) + 1 in
    let (newList, newPos) = ((take insertPos list) ++ (marble : drop insertPos list), insertPos) in
      Board newList newPos scores
  else
    let removePos = (advance b (-7)) in
    let removed:t = drop removePos list in
    let currentPlayer = 1 + (mod (marble - 1) (length scores)) in
    let newScores = Map.update (\s -> Just (s + removed + marble)) currentPlayer scores in
      Board ((take removePos list) ++ t) removePos newScores


game :: Int -> Int -> Board
game players moves =
  let emptyBoard = Board [0] 1 (Map.fromList (map (\e -> (e, 0)) [1..players])) in 
  foldl' play emptyBoard [1..moves]

result :: Board -> Int
result (Board _ _ scores) = maximum scores


-- result (game 5 23)
-- result (game 10 1618)
-- result (game 13 7999)
main = putStrLn (show (result (game 452 7078400)))
