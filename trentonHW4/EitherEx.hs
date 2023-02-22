type Prog = [Cmd]

type Stack = [Either Bool Int]
data Cmd = ADD
          | LEQ

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd ADD [] = Nothing
semCmd ADD ((Right x):[]) = Nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s)
semCmd ADD _ = Nothing

semCmd LEQ [] = Nothing
semCmd LEQ ((Right x):[]) = Nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

semCmd (IFELSE p1 p2) (x:s) 

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
