import Stacklang1
{- Test Cases for Part 1 Stack Language 1
   Test by loading hw4p1verifier.hs
   Call function runAllTests
 Results 
	Test 1:  Just [36]
	Test 2:  Just [13]
	Test 3:  Just [10]
	Test 4:  Nothing
	Test 5:  Just [10,2,3,4,5]
	Test 6:  Nothing
	Test 7:  Just [1,1,1,2,3,4,5]
-}
 
s1 :: Stack
s1 = [1, 2, 3, 4, 5]
s2 :: Stack
s2 = [10]

t1 = [LD 3,DUP,ADD,DUP,MULT] 
t2 = [LD 3,ADD]
t3 = []
t4 = [ADD, ADD, ADD, ADD]
t5 = [LD 10, MULT]
t6 = [MULT]
t7 = [DUP, DUP]
tList = [t1, t2, t3, t4, t5, t6, t7]
sList = [[], s2, s2, [], s1, s2, s1]

runAll :: [Stack] -> [Prog]->Int-> String
runAll [] _ _ = " "
runAll _ [] _ = " "

runAll (s:ss) (p:ps) n = "Test "++show(n)++":  "++show(run p s) ++ "\n\n" ++ (runAll ss ps (n+1))		
runAllTests :: IO ()
runAllTests = putStrLn (runAll sList tList 1)
--   

--
