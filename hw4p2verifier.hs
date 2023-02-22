import Stacklang2
{- Test Cases for Part 1 Stack Language 2
   Test by loading hw4p2verifier.hs
   Call function runAllTests
   Results
		Test 1:  Just [Right 36]
		Test 2:  Just [Right 1,Left True,Left True,Right 3]
		Test 3:  Nothing
		Test 4:  Nothing
		Test 5:  Just [Right 9,Right 5,Right 7,Right 9]
		Test 6:  Just [Right 20]
		Test 7:  Just [Right 63,Right 63,Right 9]
		Test 8:  Just [Right 4,Right 5,Right 7,Right 9]
		Test 9:  Just [Right 10]
		Test 10:  Just [Right 100,Right 100]
		Test 11:  Just [Right 15]
-}

s1 :: Stack
s1 = [Right 1,Right 3, Right 5, Right 7, Right 9]
s2 :: Stack
s2 = [Left True, Right 3]
t1 = [LDI 3,DUP,ADD,DUP,MULT] 
t2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
t3 = [LEQ]
t4 = [ADD, ADD, MULT, DUP]
t5 = [LEQ, IFELSE [] [], LDI 9]
t6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
t7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [], ADD]
t8 = [LDI 5, LDI 1, LEQ, IFELSE [LDB True, IFELSE [LDI 10] [LDI 100]] [LDI 20, DUP]]
t9 = [LDI 5, LDI 1, LEQ, IFELSE [LDB False, IFELSE [LDI 10] [LDI 100]] [LDI 20, DUP], DUP]
t10 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]


tList = [t1, t2, t3, t4, t5, t6, t4, t7, t8, t9, t10]
sList = [[], s2, s2, [], s1, [], s1, s1, [], [], []]

runAll :: [Stack] -> [Prog]-> Int -> String
runAll [] _ _= " "
runAll _ [] _ = " "
runAll (s:ss) (p:ps) n = "Test "++show(n)++":  "++show(run p s) ++ "\n\n" ++ (runAll ss ps (n+1))
		
runAllTests :: IO ()
runAllTests = putStrLn (runAll sList tList 1)


