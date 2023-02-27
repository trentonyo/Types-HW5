-- Homework 5 (types)
module HW5types where
import Data.Maybe

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