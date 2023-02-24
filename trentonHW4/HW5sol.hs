--Trenton Young
--youngtre@oregonstate.edu

module HW5sol where
import HW5types

-- HW5types.hs
--type Prog = [Cmd]
--data Cmd = LDI Int | ADD | MULT | DUP | DEC
--		| SWAP | POP Int | IFELSE Prog Prog
--		| LDB Bool | LEQ
--         deriving Show
--
--data Val = I Int | B Bool
--		deriving Show
--
--type Stack = [Val]
--data Result = A Stack | RankError | TypeError
--          deriving Show
--
--type D = Stack -> Stack
--type Rank = Int
--type CmdRank = (Int, Int)
-- /HW5types.hs

-- ######### Run stuff ######### --

semCmd :: Cmd -> Stack -> Result

semCmd (LDI i) s = A ((I i):s)
semCmd (LDB b) s = A ((B b):s)

semCmd DUP [] = RankError
semCmd DUP (x:xs) = A (x:x:xs)

semCmd DEC [] = RankError
semCmd DEC ((I x):xs) = A ((I (x-1)):xs)
semCmd DEC ((B _):_) = TypeError

semCmd (POP _) [] = RankError
semCmd (POP 1) (x:xs) = A xs
semCmd (POP k) (x:xs) = semCmd (POP (k-1)) xs

semCmd SWAP [] = RankError
semCmd SWAP (x:y:xs) = A (y:x:xs)
semCmd SWAP _ = RankError

--semCmd ADD [] = RankError
--semCmd ADD ((I x):[]) = RankError
semCmd ADD ((I x):(I y):s) = A ((I (x+y)):s)
semCmd ADD ((B _):(I _):_) = TypeError
semCmd ADD ((I _):(B _):_) = TypeError
semCmd ADD ((B _):(B _):_) = TypeError
semCmd ADD _ = RankError -- Will catch empty and one-element Stack

--semCmd MULT [] = RankError
--semCmd MULT ((I x):[]) = RankError
semCmd MULT ((I x):(I y):s) = A ((I(x*y)):s)
semCmd MULT ((B _):(I _):_) = TypeError
semCmd MULT ((I _):(B _):_) = TypeError
semCmd MULT ((B _):(B _):_) = TypeError
semCmd MULT _ = RankError -- Will catch empty and one-element Stack

--semCmd LEQ [] = RankError
--semCmd LEQ ((I x):[]) = RankError
semCmd LEQ ((I x):(I y):s) = A ((B (x<=y)):s)
semCmd LEQ ((B _):(I _):_) = TypeError
semCmd LEQ ((I _):(B _):_) = TypeError
semCmd LEQ ((B _):(B _):_) = TypeError
semCmd LEQ _ = RankError -- Will catch empty and one-element Stack

--semCmd (IFELSE t f) [] = RankError
semCmd (IFELSE t f) ((B x):xs)  = run (if x then t else f) xs
semCmd (IFELSE t f) ((I _):_)   = TypeError
semCmd (IFELSE t f) _ = RankError

run :: Prog -> Stack -> Result
run [] x     = A x                        -- Once we have run out of cmds, return the remaining stack
run (c:cs) s = case semCmd c s of
                  RankError -> RankError  -- If calling secCmd c s results in a RankError, we return a RankError
                  TypeError -> TypeError  -- If calling secCmd c s results in a RankError, we return a RankError
                  A s' -> run cs s'       -- If calling secCmd c s results in A (Result Stack) then recursively call run with the remaining cmd's on the Result Stack
run _ []     = RankError                  -- An empty stack after all that? Errrrrrr

-- ######### Rank stuff ######### --

--type Rank = Int
--type CmdRank = (Rank, Rank)

rankC :: Cmd -> CmdRank
rankC ADD = (2, 1)
rankC DUP = (1, 1)
rankC LEQ = (2, 2)
rankC DEC = (1, 1)
rankC MULT = (2, 1)
rankC SWAP = (2, 2)
rankC (POP k) = (k, 0)
rankC (LDI _) = (0, 1)
rankC (LDB _) = (0, 1)
--rankC (IFELSE t f) = -- hmmmmmmmmm maybe this doesn't get called but then there would be non-exhaustive pattern

rankP :: Prog -> Rank -> Maybe Rank
rankP ((IFELSE c t f):cs) r = min (rank ((t):cs) r) (rank ((f):cs) r) --Something like this where you take the min of the branches
rankP (c:cs) r =  if fst (rankC c) > r    -- Checks if the first rank (what needs to be removed) is greater than the rank...
                    then RankError        -- ...and if it is then it's a rank error
                    else rankP cs (r - (fst (rankC c)) + (snd (rankC c))) -- Otherwise, move to the next Cmd and adjust the rank
