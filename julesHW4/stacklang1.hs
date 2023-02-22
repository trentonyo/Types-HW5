-- Jules Brinkley -- brinklju@oregonstate.edu
-- February 2023

module StackLang where

-- language definitions
type Prog = [Cmd]
data Cmd
    = LD Integer    -- if I use Int instead of Integer, haskell infers the list the user inputs
    | ADD           -- as Integers and then we hit a type error. bruh moment
    | MULT
    | DUP
    deriving Show
type Stack = [Integer]

semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD n) s = Just (n:s) -- push number onto stack

semCmd ADD [] = Nothing -- if there's nothing in the stack, return nothing
semCmd ADD (x:[]) = Nothing -- if there's only one integer in the stack, return nothing
semCmd ADD (x:y:s) = Just ((x+y):s) -- if there are two integers in the stack, add them and push the result onto the stack

semCmd MULT [] = Nothing -- if there's nothing in the stack, return nothing
semCmd MULT (x:[]) = Nothing -- if there's only one integer in the stack, return nothing
semCmd MULT (x:y:s) = Just ((x*y):s) -- if there are two integers in the stack, multiply them and push the result onto the stack

semCmd DUP [] = Nothing -- if there's nothing in the stack, return nothing
semCmd DUP (x:s) = Just (x:x:s) -- if there's an integer on top of the stack, duplicate it and push the result onto the stack

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s -- if there are no commands in the program, return the stack
run (c:cs) s = case semCmd c s of -- if there are commands in the program, recursively run each command
                    Nothing -> Nothing -- if any command returns nothing, return nothing
                    Just s' -> run cs s' -- if all commands return a stack, return the final stack

p :: Prog
p = [LD 2, DUP, LD 5, ADD, DUP, MULT, LD 3] -- test program