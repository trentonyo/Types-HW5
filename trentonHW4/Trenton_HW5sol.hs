--Trenton Young
--youngtre@oregonstate.edu

module HW5sol where

-- HW5types.hs
type Prog = [Cmd]
data Cmd = LDI Int | ADD | MULT | DUP | DEC
		| SWAP | POP Int | IFELSE Prog Prog
		| LDB Bool | LEQ
         deriving Show

data Val = I Int | B Bool
		deriving Show

type Stack = [Val]
data Result = A Stack | RankError | TypeError
          deriving Show

type D = Stack -> Stack
type Rank = Int
type CmdRank = (Int, Int)
-- /HW5types.hs

semCmd :: Cmd -> Stack -> Result

semCmd (LDI (I i)) s = A i:s
semCmd (LDB (B b)) s = A b:s

semCmd DUP [] = RankError
semCmd DUP (x:xs) = A (x:x:xs)

semCmd ADD [] = RankError
semCmd ADD ((I x):[]) = RankError
semCmd ADD ((I x):(I y):s) = A ((I (x+y)):s)
semCmd ADD _ = TypeError

semCmd MULT [] = RankError
semCmd MULT ((I x):[]) = RankError
semCmd MULT ((I x):(I y):s) = A ((I(x*y)):s)
semCmd MULT _ = RankError

semCmd LEQ [] = RankError
semCmd LEQ ((I x):[]) = RankError
semCmd LEQ ((I x):(I y):s) = A ((B (x<=y)):s)
semCmd LEQ _ = RankError

semCmd (IFELSE t f) [] = RankError
semCmd (IFELSE t f) ((B x):xs) = run (if x then t else f) xs
semCmd (IFELSE t f) _ = RankError

run :: Prog -> Stack -> Result
run []            x       = A x                                -- Once we have run out of cmds, return the remaining stack
--run ((LDI a):xs)  x       = run xs ((I a):x)                  -- For LD integer, append the value to the stack                        -- !!Why aren't these in semCmd?
--run ((LDB a):xs)  x       = run xs ((B a):x)                                                                                          -- !!Why aren't these in semCmd?
--run ((DUP):xs)    x       = if (null x)   then RankError                            -- If the stack has fewer than one element, err   -- !!Why aren't these in semCmd?
--                                          else run xs ((head x):x)                -- Append the head of the stack to it               -- !!Why aren't these in semCmd?
run (c:cs) s = case semCmd c s of
                  RankError -> RankError      -- If calling secCmd c s results in a RankError, we return a RankError
                  A s' -> run cs s'           -- If calling secCmd c s results in A (Result Stack) then recursively call run with the remaining cmd's on the Result Stack
run _             []      = RankError                               -- An empty stack after all that? Errrrrrr

