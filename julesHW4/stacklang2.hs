-- Jules Brinkley -- brinklju@oregonstate.edu
-- February 2023


module StackLang where

-- language definitions
type Prog = [Cmd]
type Stack = [Either Bool Integer]
data Cmd
    = LDI Integer
    | LDB Bool
    | LEQ
    | ADD
    | MULT
    | DUP
    | IFELSE Prog Prog
    deriving Show

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd (LDI n) s = Just ((Right n):s) -- push integer onto stack

semCmd (LDB b) s = Just ((Left b):s) -- push boolean onto stack

semCmd ADD [] = Nothing -- if there's nothing in the stack, return nothing
semCmd ADD ((Right x):[]) = Nothing -- if there's only one integer in the stack, return nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s) -- if there are two integers in the stack, add them and push the result onto the stack
semCmd ADD _ = Nothing -- if there are any other types on top of the stack, return nothing

semCmd MULT [] = Nothing -- if there's nothing in the stack, return nothing
semCmd MULT ((Right x):[]) = Nothing -- if there's only one integer in the stack, return nothing
semCmd MULT ((Right x):(Right y):s) = Just ((Right(x*y)):s) -- if there are two integers in the stack, multiply them and push the result onto the stack
semCmd MULT _ = Nothing -- if there are any other types on top of the stack, return nothing

semCmd DUP [] = Nothing -- if there's nothing in the stack, return nothing
semCmd DUP ((Left x):s) = Just ((Left x):(Left x):s) -- if there's a boolean on top of the stack, duplicate it and push the result onto the stack
semCmd DUP ((Right x):s) = Just ((Right x):(Right x):s) -- if there's an integer on top of the stack, duplicate it and push the result onto the stack

semCmd LEQ [] = Nothing -- if there's nothing in the stack, return nothing
semCmd LEQ ((Right x):[]) = Nothing -- if there's only one integer in the stack, return nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s) -- if there are two integers in the stack, compare them and push the result onto the stack
semCmd LEQ _ = Nothing -- if there are any other types on top of the stack, return nothing

semCmd (IFELSE p1 p2) [] = Nothing -- if there's nothing in the stack, return nothing
semCmd (IFELSE p1 p2) ((Left True):s) = run p1 s -- if there's a True boolean on the stack, run the first program
semCmd (IFELSE p1 p2) ((Left False):s) = run p2 s -- if there's a False boolean on the stack, run the second program
semCmd (IFELSE p1 p2) _ = Nothing -- if there are any other types on top of the stack, return nothing

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s -- if there are no commands in the program, return the stack
run (c:cs) s = case semCmd c s of -- if there are commands in the program, recursively run each command
                    Nothing -> Nothing -- if any command returns nothing, return nothing
                    Just s' -> run cs s' -- if all commands return a stack, return the final stack

p :: Prog
p = [LDB True, DUP, LDI 3, DUP, ADD] -- test program