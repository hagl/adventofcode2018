import Data.Bits
import Data.List 
import Data.Array
import qualified Data.Map.Strict as Map

data State = State Integer Integer Integer Integer Integer Integer deriving (Show, Eq)
-- data Command = Comand Integer Integer Integer Integer  deriving (Show, Eq)

type BinOp =  Integer -> Integer -> Integer

add :: BinOp
add a b = a + b

mul :: BinOp
mul a b = a * b

ban :: BinOp
ban a b = a .&. b

bor :: BinOp
bor a b = a .|. b

set :: BinOp
set a _  = a

gtr :: BinOp
gtr a b = if a > b then 1 else 0

eqr :: BinOp
eqr a b = if a == b then 1 else 0

-- regOp :: BinOp -> Integer -> Integer -> Integer -> State -> State

sourceR :: Integer -> Integer -> State -> (Integer, Integer)
sourceR 0 0 (State r0 r1 r2 r3 r4 r5) = (r0, r0)
sourceR 0 1 (State r0 r1 r2 r3 r4 r5) = (r0, r1)
sourceR 0 2 (State r0 r1 r2 r3 r4 r5) = (r0, r2)
sourceR 0 3 (State r0 r1 r2 r3 r4 r5) = (r0, r3)
sourceR 0 4 (State r0 r1 r2 r3 r4 r5) = (r0, r4)
sourceR 0 5 (State r0 r1 r2 r3 r4 r5) = (r0, r5)
sourceR 1 0 (State r0 r1 r2 r3 r4 r5) = (r1, r0)
sourceR 1 1 (State r0 r1 r2 r3 r4 r5) = (r1, r1)
sourceR 1 2 (State r0 r1 r2 r3 r4 r5) = (r1, r2)
sourceR 1 3 (State r0 r1 r2 r3 r4 r5) = (r1, r3)
sourceR 1 4 (State r0 r1 r2 r3 r4 r5) = (r1, r4)
sourceR 1 5 (State r0 r1 r2 r3 r4 r5) = (r1, r5)
sourceR 2 0 (State r0 r1 r2 r3 r4 r5) = (r2, r0)
sourceR 2 1 (State r0 r1 r2 r3 r4 r5) = (r2, r1)
sourceR 2 2 (State r0 r1 r2 r3 r4 r5) = (r2, r2)
sourceR 2 3 (State r0 r1 r2 r3 r4 r5) = (r2, r3)
sourceR 2 4 (State r0 r1 r2 r3 r4 r5) = (r2, r4)
sourceR 2 5 (State r0 r1 r2 r3 r4 r5) = (r2, r5)
sourceR 3 0 (State r0 r1 r2 r3 r4 r5) = (r3, r0)
sourceR 3 1 (State r0 r1 r2 r3 r4 r5) = (r3, r1)
sourceR 3 2 (State r0 r1 r2 r3 r4 r5) = (r3, r2)
sourceR 3 3 (State r0 r1 r2 r3 r4 r5) = (r3, r3)
sourceR 3 4 (State r0 r1 r2 r3 r4 r5) = (r3, r4)
sourceR 3 5 (State r0 r1 r2 r3 r4 r5) = (r3, r5)
sourceR 4 0 (State r0 r1 r2 r3 r4 r5) = (r4, r0)
sourceR 4 1 (State r0 r1 r2 r3 r4 r5) = (r4, r1)
sourceR 4 2 (State r0 r1 r2 r3 r4 r5) = (r4, r2)
sourceR 4 3 (State r0 r1 r2 r3 r4 r5) = (r4, r3)
sourceR 4 4 (State r0 r1 r2 r3 r4 r5) = (r4, r4)
sourceR 4 5 (State r0 r1 r2 r3 r4 r5) = (r4, r5)
sourceR 5 0 (State r0 r1 r2 r3 r4 r5) = (r5, r0)
sourceR 5 1 (State r0 r1 r2 r3 r4 r5) = (r5, r1)
sourceR 5 2 (State r0 r1 r2 r3 r4 r5) = (r5, r2)
sourceR 5 3 (State r0 r1 r2 r3 r4 r5) = (r5, r3)
sourceR 5 4 (State r0 r1 r2 r3 r4 r5) = (r5, r4)
sourceR 5 5 (State r0 r1 r2 r3 r4 r5) = (r5, r5)

sourceI :: Integer -> Integer -> State -> (Integer, Integer)
sourceI 0 b (State r0 r1 r2 r3 r4 r5) = (r0, b)
sourceI 1 b (State r0 r1 r2 r3 r4 r5) = (r1, b)
sourceI 2 b (State r0 r1 r2 r3 r4 r5) = (r2, b)
sourceI 3 b (State r0 r1 r2 r3 r4 r5) = (r3, b)
sourceI 4 b (State r0 r1 r2 r3 r4 r5) = (r4, b)
sourceI 5 b (State r0 r1 r2 r3 r4 r5) = (r5, b)

dest :: Integer -> Integer -> State -> State
dest 0 v (State r0 r1 r2 r3 r4 r5) = (State v r1 r2 r3 r4 r5)
dest 1 v (State r0 r1 r2 r3 r4 r5) = (State r0 v r2 r3 r4 r5)
dest 2 v (State r0 r1 r2 r3 r4 r5) = (State r0 r1 v r3 r4 r5)
dest 3 v (State r0 r1 r2 r3 r4 r5) = (State r0 r1 r2 v r4 r5)
dest 4 v (State r0 r1 r2 r3 r4 r5) = (State r0 r1 r2 r3 v r5)
dest 5 v (State r0 r1 r2 r3 r4 r5) = (State r0 r1 r2 r3 r4 v)

swap (a, b) = (b, a)

combineR op a b c s = dest c ((uncurry op) (sourceR a b s)) s
combineI op a b c s = dest c ((uncurry op) (sourceI a b s)) s
combineIR op a b c s = dest c ((uncurry op) . swap $ (sourceI b a s)) s
combineI_ op a _ c s = dest c ((uncurry op) . swap $ (sourceI 0 a s)) s

addr = combineR add
addi = combineI add

mulr = combineR mul
muli = combineI mul

banr = combineR ban
bani = combineI ban

borr = combineR bor
bori = combineI bor

setr = combineR set
seti = combineI_ set

gtrr = combineR gtr
gtri = combineI gtr
gtir = combineIR gtr

eqrr = combineR eqr
eqri = combineI eqr
eqir = combineIR eqr

ops :: [Op]
ops = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr ]

type Op = Integer -> Integer -> Integer -> State -> State


step :: State -> State
step s@(State r0 r1 r2 r3 r4 r5) = let (instr, a, b, c) = prog ! (fromIntegral r4)  in
  let (State r0' r1' r2' r3' r4' r5') = instr a b c s in
  State r0' r1' r2' r3' (r4' + 1) r5'
  
-- run :: State -> Integer
-- run state = let s@(State r0 r1 r2 r3 r4 r5) = step state in
-- if ((fromIntegral r4) > 35) then r0 else run s

-- run :: State -> Integer
run state = let s@(State r0 r1 r2 r3 r4 r5) = step state in
  if ((fromIntegral r4) > 35) then s else run s

main = putStrLn (show (run (State 1 0 0 0 0 0)))

-- #ip 4
prog :: Array Integer (Integer -> Integer -> Integer -> State -> State, Integer, Integer, Integer)
prog = listArray (0,36) program

program = [
  (addi, 4, 16, 4), -- 0:  r4 = 16 + r4 // goto 17
--  (seti, 100, 5, 4),  -- 1: r1 = 1  show input to loop
 (seti, 1, 5, 1),  -- 1: r1 = 1
  (seti, 1, 2, 2),  -- 2: r2 = 1
  (mulr, 1, 2, 3),  -- 3: r3 = r1 * r2
  (eqrr, 3, 5, 3),  -- 4: r3 == (r3 == r5) ?              ? if (r1 * r2) == r5 then r0 += r1; goto 8
  (addr, 3, 4, 4),  -- 5: r4 = r3 + r4  
  (addi, 4, 1, 4),  -- 6: r4 = 1 + r4
  (addr, 1, 0, 0),  -- 7: r0 = r1 + r0                          
  (addi, 2, 1, 2),  -- 8: r2 = r2 + 1                           ? r2++ 
  (gtrr, 2, 5, 3),  --  9: r3 = r2 > r5  //                     ? if r2 > r5 then goto 12 else goto 2 
  (addr, 4, 3, 4), -- 10: r4 = r3 + r4
  (seti, 2, 7, 4),  -- 11: r4 = 2
  (addi, 1, 1, 1), -- 12: r1 = r1 + 1                           ? r1++
  (gtrr, 1, 5, 3),  -- 13: r3 = r1 > r5  //                     ? if r1 > r5 then exit else goto 2
  (addr, 3, 4, 4), -- 14: r4 = r3 + r4  //
  (seti, 1, 9, 4),  -- 15: r4 = 1          // goto 2
  (mulr, 4, 4, 4), -- 16: r4 = r4 * r4  // exit  
  (addi, 5, 2, 5), -- 17: r5 = r5 + 2  //                      ?   r5 = (r5 * 2) ^ 2 * 19 * 11
  (mulr, 5, 5, 5), -- 18: r5 = r5 * r5
  (mulr, 4, 5, 5), -- 19: r5 = r4 * r5 // r5 = r5 * 19
  (muli, 5, 11, 5),-- 20: r5 =r5 * 11 // 
  (addi, 3, 1, 3), -- 21: r3 = r3 + 1 //                        
  (mulr, 3, 4, 3), -- 22: r3 = r3 * r4 // r3 = r3 * 22  
  (addi, 3, 18, 3), -- 23: r3 = r3 + 18                        ? r3 = (r3 + 1) * 22 + 18
  (addr, 5, 3, 5), -- 24: r5 = r5 + r3                          
  (addr, 4, 0, 4), -- 25: r4 = r0 + r4 // if r0 == 0 goto 1 else goto 27 
  (seti, 0, 3, 4), -- 26: r4 = 0
  (setr, 4, 2, 3), -- 27: r3 = r4          // r3 = 27
  (mulr, 3, 4, 3), -- 28: r3 = r3 * r4 // r3 = r3 * 28
  (addr, 4, 3, 3), -- 29: r3 = r4 + r3 // r3 = 29 + r3
  (mulr, 4, 3, 3), -- 30: r3 = r4 * r3 // r3 = 30 * r3
  (muli, 3, 14, 3), -- 31: r3 = 14 * r3 
  (mulr, 3, 4, 3), -- 32: r3 = r3 * r4 // r3 = r3 * 32
  (addr, 5, 3, 5), -- 33: r5 = r5 + r3 // r5 = r5 + r3
  (seti, 0, 4, 0), -- 34: r0 = 0
  (seti, 0, 5, 4)] -- 35: r4 = 0        // goto 1

  -- r5 = 836 // 2 * 2 * 19 * 11  
  -- r3 = 40 // 1 * 22 + 18
  -- r5 = 876

  -- r3 = (27 * 28 + 29) * 30 * 14 * 32
  --  r3 = 10550400
  -- r5 = 876 + 10550400
  -- r0 = 0

  -- https://www.dcode.fr/divisors-list-number