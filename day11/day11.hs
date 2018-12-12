import Data.List (maximumBy, foldl')
import Data.Array

value :: Integer -> (Integer, Integer) -> Integer
value serial (x,y) =
  let rackId = x + 10 in
  let power1 = ((y * rackId) + serial) * rackId in
  let power100 = (mod (quot power1 100) 10) in
  let power = power100 - 5 in
  power

serial = 7347
values = array ((1,1),(300,300)) [ ((x,y), value serial (x,y)) | x <- [1..300], y <- [1..300] ]

rect :: (Integer, Integer, Integer) -> Integer 
rect (x0, y0, size) = sum [values ! (x,y) | x <- [x0..(x0+size-1)], y <- [y0..(y0+size-1)]] 

vals = [(x,y) | x<-[1..300], y <-[1..300]]

maxSize (x, y) = let maxSize = min (300 - x) (300 - y) in
  let incr s previous = previous + sum [(values ! (x + s, y + d)) + (values ! (x + d, y + s))| d <- [0 .. (s-1)]] + values ! (x + s, y + s) in
  let step (m@(k, v), p) s = let n = (incr s p) in if (n > v) then ((s, n), n) else (m, n) in
  let ((i,v),_) = foldl' step ((-1, -1000), 0) [0..maxSize] in 
    ((x,y,i+1), v)
  

mapped = map maxSize vals

result = maximumBy (\(_,v1) (_,v2) -> compare v1 v2) mapped
  
main = do
  putStrLn (show result)
