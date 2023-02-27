-- Trenton Young - youngtre@oregonstate.edu
-- Julian Brinkley - brinklju@oregonstate.edu
-- Jon Nelson - nelsojo4@oregonstate.edu

module HW5sol where
import HW5types
import Data.Maybe 

--  ######### Run stuff ######### --

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

semCmd ADD ((I x):(I y):s) = A ((I (x+y)):s)
semCmd ADD ((B _):(I _):_) = TypeError
semCmd ADD ((I _):(B _):_) = TypeError
semCmd ADD ((B _):(B _):_) = TypeError
semCmd ADD _ = RankError --  Will catch empty and one-element Stack

semCmd MULT ((I x):(I y):s) = A ((I(x*y)):s)
semCmd MULT ((B _):(I _):_) = TypeError
semCmd MULT ((I _):(B _):_) = TypeError
semCmd MULT ((B _):(B _):_) = TypeError
semCmd MULT _ = RankError --  Will catch empty and one-element Stack

semCmd LEQ ((I x):(I y):s) = A ((B (x<=y)):s)
semCmd LEQ ((B _):(I _):_) = TypeError
semCmd LEQ ((I _):(B _):_) = TypeError
semCmd LEQ ((B _):(B _):_) = TypeError
semCmd LEQ _ = RankError -- Will catch empty and one-element Stack

semCmd (IFELSE t f) ((B x):xs)  = run (if x then t else f) xs
semCmd (IFELSE t f) ((I _):_)   = TypeError
semCmd (IFELSE t f) _ = RankError


run :: Prog -> Stack -> Result
--run _ []     = RankError
run p s = if isNothing (rankP p (length s))
                  then RankError
                  else runHelper p s  


runHelper :: Prog -> Stack -> Result
runHelper [] x     = A x                        -- Once we have run out of Cmds, return the remaining stack
runHelper (c:cs) s = case semCmd c s of
                  RankError -> RankError        -- If calling secCmd c s results in a RankError, we return a RankError
                  TypeError -> TypeError        -- If calling secCmd c s results in a RankError, we return a RankError
                  A s' -> runHelper cs s'       -- If calling secCmd c s results in A (Result Stack) then recursively call run with the remaining cmd's on the Result Stack


--run :: Prog -> Stack -> Result
--run _ []     = RankError                  -- An empty stack after all that? Errrrrrr
--run [] x     = A x                        -- Once we have run out of cmds, return the remaining stack
--run (c:cs) s = case semCmd c s of
--                  RankError -> RankError  -- If calling secCmd c s results in a RankError, we return a RankError
--                  TypeError -> TypeError  -- If calling secCmd c s results in a RankError, we return a RankError
--                  A s' -> run cs s'       -- If calling secCmd c s results in A (Result Stack) then recursively call run with the remaining cmd's on the Result Stack



-- ######### Rank stuff ######### --

rankC :: Cmd -> CmdRank
rankC ADD = (2, 1)   
rankC DUP = (1, 2)   
rankC LEQ = (2, 1)   
rankC DEC = (1, 1)
rankC MULT = (2, 1)  
rankC SWAP = (2, 2)
rankC (POP k) = (k, 0)
rankC (LDI _) = (0, 1)
rankC (LDB _) = (0, 1)
--  rankC (IFELSE t f)  This doesn't get called because of how rankP handles IFELSE, so here is a non-exhaustive pattern

rankP :: Prog -> Rank -> Maybe Rank
rankP ((IFELSE t f):cs) r = min (rankP (t++cs) (r - 1)) (rankP (f++cs) (r - 1))   -- Pops the top element (a bool) and checks the min path
rankP [c]     r = if fst (rankC c) > r                                            -- Checks if the first rank (what needs to be removed) is greater than the rank...
                    then Nothing                                                  -- ...and if it is then it's a rank error
                    else Just (r - (fst (rankC c)) + (snd (rankC c)))             -- Otherwise, base case is reached and return r after the Cmd
rankP (c:cs)  r = if fst (rankC c) > r                                            -- Checks if the first rank (what needs to be removed) is greater than the rank...
                    then Nothing                                                  -- ...and if it is then it's a rank error
                    else rankP cs (r - (fst (rankC c)) + (snd (rankC c)))         -- Otherwise, move to the next Cmd and adjust the rank
--rankP _ _       = Nothing
