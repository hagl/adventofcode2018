import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.List (sort, group, find)
import Data.Char (digitToInt)

data Area = Area {
  id :: Integer,
  x :: Integer,
  y :: Integer,
  w :: Integer,
  h :: Integer
} deriving Show

p_areas :: CharParser () [Area]
p_areas = endBy p_line eol

-- #18 @ 925,936: 28x28

p_line = do
  char '#'
  a <- integer
  string " @ "
  x <- integer
  char ','
  y <- integer
  string ": " 
  w <- integer
  char 'x'
  h <- integer
  return (Area a x y w h)

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
  case parse p_areas inputFile input of
    -- Right(areas) -> putStrLn (show (countOverlap areas))
    -- Right(areas) -> putStrLn (show (findOverlapFree areas))
    Right(areas) -> putStrLn (show (filter (overlaps2 (areas !! 106 )) areas))
    Left(error) -> putStrLn (show error)

main = do
  args <- getArgs
  case args of 
    [input] -> processFile input 
    _ -> putStrLn "error: specify input file"

contains area x0 y0 = 
  x0 >= (x area) 
  && x0 < (x area) + (w area) 
  && y0 >= (y area)
  && y0 < (y area) + (h area)

isOverlap areas (x0, y0) = loop False areas where
  loop _ [] = False
  loop True (a:as) = if (contains a x0 y0) then True else loop True as
  loop False (a:as) = loop (contains a x0 y0) as

overlaps2 a1 a2 = (x a1) < (x a2) + (w a2) && (x a2) < (x a1) + (w a1) && (y a1) < (y a2) + (h a2) && (y a2) < (y a1) + (h a1) 


-- doesn't work on this constellation:
--  .  a  . 
--  b a/b b
--  .  a  .
overlaps area1 area2 =
    area1 `hasPointContainedIn` area2 
    || area2 `hasPointContainedIn` area1
  where
    (Area _ x y w h) `hasPointContainedIn` area =
      contains area x y
      || contains area  (x + w - 1) y
      || contains area x (y + h - 1)
      || contains area (x + w - 1) (y + h - 1)

      
 
countOverlap areas = length (filter (isOverlap areas) [(x,y) | x <- [0..1000], y <- [0..1000]])

-- findOverlapFree :: [Area] -> Maybe Area
-- findOverlapFree areas = helper [] areas where
--   helper _ [] = Nothing
--   helper pre (a:as) = if (null (filter (overlaps a) (pre ++ as))) then Just(a) else helper (a:pre) as

  -- 
findOverlapFree :: [Area] -> [Area]
findOverlapFree areas = helper [] areas [] where
  helper _ [] acc = acc
  helper pre (a:as) acc = if (null (filter (overlaps2 a) (pre ++ as))) then helper (a:pre) as (a:acc) else helper (a:pre) as acc
