
import HW5types
import HW5sol

-- rankAll ( ranks all programs in pList and stacks sList)
-- rankAllEmpty (ranks all programs in pList with an empty stack)
-- runAllTests  ( run all programs in pList with stacks in sList) )
-- runAllEmpty  ( run all programs in pList with empty stack )

--test programs
p1 = [LDI 3, DUP, ADD, LDI 5, SWAP] 
p2 = [LDI 8, POP 1, LDI 3, DUP, POP 2, LDI 4] 
p3 = [LDI 3, LDI 4, LDI 5, MULT, ADD] 
p4 = [DUP] 
p5 = [POP 4] 
p6 = [LDB True, IFELSE [ADD] [LDI 7], ADD ]
p7 = [LDB True, LDI 1, LDI 10, LDI 5, IFELSE [ADD] [LDI 7], ADD ]
p8 = [LDI 20, LDI 1, LDI 10, LDI 5, LEQ, IFELSE [ADD] [LDI 7], DUP ]
p9 = [LDB True, LDB False, MULT]
p10 = [LDI 10, DEC, DUP, DUP, DUP, POP 2]
p11 = [LDI 10, LDI 20, LEQ, DEC]
p12 = [LDI 10, LDI 5, LDB True, IFELSE [LDB True, IFELSE [ADD, DUP] [MULT]] [LDI 7] ]
p13 = [LDI 10, LDI 5, LDB True, IFELSE [LDB False, IFELSE [ADD, DUP] [MULT]] [LDI 7] ]
p14 = [LDI 10, LDI 5, LDB False, IFELSE [LDB True, IFELSE [ADD, DUP] [MULT]] [LDI 7] ]
p15 = [LDI 10, LDI 5, LDB False, IFELSE [LDB True, IFELSE [ADD, ADD] [MULT]] [LDI 7] ]

pList  = [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15]

sList =[s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15 ]
eList = [ [], [],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]
s1 = [B True]
s2 = [I 2]
s3 = [I 1, I 3]
s4 = [B False, I 4, I 7]
s5 = [I 1, I 4, I 6]
s6 = [I 10]
s7 = [I 100, I 6]
s8 = [I 10, I 20, I 1]
s9 = [I 3, I 8]
s10 = [I 1, I 4, I 6]
s11 = [I 10]
s12 = [I 100, I 6]
s13 = [I 10, I 20, I 1]
s14 = [I 3, I 8]
s15 = [I 3, I 8]

prettyERank :: [Prog] -> [Stack] -> String
prettyERank [] _ = " "
prettyERank (p:pp) (s:ss) = "p:"++show(p)++"  s:"++show(s)++" ==> "++show (rankP p (length s))++"\n\n"++ (prettyERank pp ss)

rankAll :: IO ()
rankAll = putStrLn ("\nRank programs with stacks \n\n"++(prettyERank pList sList))

rankAllEmpty :: IO ()
rankAllEmpty = putStrLn ("\nRank programs with empty stacks \n\n"++(prettyERank pList eList))

prettySRun :: Prog -> Stack ->String
prettySRun p s = " s:"++show s++" p:"++show p++" ==> "++show (run p s)++"\n"


prettyERun :: Prog -> String
prettyERun p = show p++" ==> "++show (run p [])++"\n"


runS :: [Prog] -> [Stack]-> IO ()
runS [] _ = putStrLn " "
runS _ [] = putStrLn " "
runS (p:ps)(s:ss) = do 
						putStrLn (prettySRun p s)
						runS ps ss
runAll:: IO()
runAll = runS pList sList

runAllEmpty :: IO()
runAllEmpty = runS pList eList