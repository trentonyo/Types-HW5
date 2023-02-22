--Trenton Young
--youngtre@oregonstate.edu

module Stacklang2 where

--type Prog = [Cmd]

data Cmd
  = LDI Int
  | LDB Bool
  | LEQ
  | ADD
  | MULT
  | DUP
  | IFELSE Prog Prog
  deriving Show

--type Stack = [Either Bool Int]

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd ADD [] = Nothing
semCmd ADD ((Right x):[]) = Nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s)
semCmd ADD _ = Nothing

semCmd MULT [] = Nothing
semCmd MULT ((Right x):[]) = Nothing
semCmd MULT ((Right x):(Right y):s) = Just ((Right(x*y)):s)
semCmd MULT _ = Nothing

semCmd LEQ [] = Nothing
semCmd LEQ ((Right x):[]) = Nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

semCmd (IFELSE t f) [] = Nothing
semCmd (IFELSE t f) ((Left x):xs) = run (if x then t else f) xs
semCmd (IFELSE t f) _ = Nothing

run :: Prog -> Stack -> Maybe Stack
run []            x       = Just x                                -- Once we have run out of cmds, return the remaining stack
run ((LDI a):xs)  x       = run xs ((Right a):x)                  -- For LD integer, append the value to the stack
run ((LDB a):xs)  x       = run xs ((Left a):x)
run ((DUP):xs)    x       = if (null x)   then Nothing                            -- If the stack has fewer than one element, err
                                          else run xs ((head x):x)                -- Append the head of the stack to it
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
run _             []      = Nothing                               -- An empty stack after all that? Errrrrrr

