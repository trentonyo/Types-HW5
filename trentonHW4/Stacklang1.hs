--Trenton Young
--youngtre@oregonstate.edu

module Stacklang1 where

--type Prog = [Cmd]

data Cmd
  = LD Int
  | ADD
  | MULT
  | DUP
  deriving Show

--type Stack = [Int]

run :: Prog -> Stack -> Maybe Stack
run []            x       = Just x                                -- Once we have run out of cmds, return the remaining stack
run ((LD a):xs)   x       = run xs (a:x)                          -- For LD, append the value to the stack
run ((ADD):xs)    (x:ys)  = if (null ys)  then Nothing                            -- If the stack has fewer than two elements, err
                                          else run xs ((x + head ys):(tail ys))   -- Add the first element and the head of the remainder, append to the stack
run ((MULT):xs)   (x:ys)  = if (null ys)  then Nothing                            -- If the stack has fewer than two elements, err
                                          else run xs ((x * head ys):(tail ys))   -- Multiply the first element and the head of the remainder, append to the stack
run ((DUP):xs)    x       = if (null x)   then Nothing                            -- If the stack has fewer than one element, err
                                          else run xs ((head x):x)                -- Append the head of the stack to it
run _             []      = Nothing                               -- An empty stack after all that? Errrrrrr


