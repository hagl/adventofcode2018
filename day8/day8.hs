data Node = Node {
  children :: [Node],
  metadata :: [Int]
} deriving Show

parseNode :: [Int] -> (Node, [Int])
parseNode (cc:mc:t) = 
  let (children, t') = parseChildren cc t in
  let (metadata, t'') = parseMetadata mc t' in
  (Node children metadata, t'')

parseChildren:: Int -> [Int] -> ([Node], [Int])
parseChildren 0 t = ([], t)
parseChildren c t = let (n, t') = parseNode t in 
  let (children, t'') = parseChildren (c - 1) t' in
  (n : children, t'')

parseMetadata:: Int -> [Int] -> ([Int], [Int])
parseMetadata c l = (take c l, drop c l)

checksum :: Node -> Int
checksum (Node children metadata) = sum (map checksum children) + sum metadata

checksum2 :: Node -> Int
checksum2 (Node [] metadata) = sum metadata
checksum2 (Node l metadata) = sum (map checksum2 (metadata >>= (\i -> if (i==0) || (i > length l) then [] else [l !! (i-1)])))

process str = let (root,_) = parseNode (map read (words str)) in
  let check1 = checksum root in
  let check2 = checksum2 root in
  (show check1) ++ " : " ++ (show check2)
  -- p "" root

p :: String -> Node -> String
p prefix (Node children metadata) = prefix ++ "Node " ++ (show metadata) ++ "\n" ++ (unlines (map (p (prefix ++ "  ")) children))

main = do
  inpStr <- readFile "input.txt"
  putStrLn (process inpStr)
