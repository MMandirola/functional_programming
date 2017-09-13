import Data.List
data LambdaTerm = Variable [Char] | Aplication LambdaTerm LambdaTerm | Abstraction [Char] LambdaTerm deriving (Show)

toString:: LambdaTerm -> [Char]
toString (Variable a) = a
toString (Aplication a b) = toString a ++ " " ++ toString b
toString (Abstraction a b) = "(\x03bb"++ a ++ "." ++ toString b ++")"

freeVars:: LambdaTerm -> [[Char]]
freeVars (Variable a) = [a]
freeVars (Aplication a b) = nub(freeVars a ++ freeVars b)
freeVars (Abstraction a b) = delete a (freeVars b)

boundVars:: LambdaTerm -> [[Char]]
boundVars (Variable a) = []
boundVars (Aplication a b) = nub(boundVars a ++ boundVars b)
boundVars (Abstraction a b) = nub([a] ++ boundVars b)

substitution:: LambdaTerm -> [Char] -> LambdaTerm -> LambdaTerm
substitution (Variable a) b c = if (b == a) then c else (Variable a)
substitution (Aplication a b) c d = (Aplication (substitution a c d) (substitution b c d) )
substitution (Abstraction a b) c d = if a == c then (Abstraction a b) else (Abstraction a (substitution b c d))

redexes:: LambdaTerm -> [LambdaTerm]
redexes (Variable a) = []
redexes l@(Aplication (Abstraction a b) c ) = [l] ++ redexes b ++ redexes c
redexes (Aplication a b ) = redexes a ++ redexes b
redexes (Abstraction a b ) = redexes b

reduceNO:: LambdaTerm -> LambdaTerm
reduceNO (Aplication (Abstraction a b) c) = reduceNO(substitution b a c)
reduceNO (Aplication a b) = (Aplication (reduceNO a) (reduceNO b))
reduceNO (Abstraction a b) = (Abstraction a (reduceNO b))
reduceNO a = a

isRedex:: LambdaTerm -> Bool
isRedex (Aplication (Abstraction a b) c) = True
isRedex _ = False

reduceB:: LambdaTerm -> LambdaTerm
reduceB (Aplication (Abstraction a b) c) = substitution b a c 
reduceB a = a

reduceAO:: LambdaTerm -> LambdaTerm
reduceAO (Aplication (Abstraction a b) c) = if isRedex(c) then reduceAO(Aplication (Abstraction a b) (reduceAO c))  else (substitution b a c) 
reduceAO (Aplication a b)=  (Aplication (reduceAO a) (reduceAO b))
reduceAO (Abstraction a b) = (Abstraction a (reduceAO b))
reduceAO a = a

test1 = Variable "a"
test2 = Variable "b"
test3 = Variable "c"
test4 = Aplication test1 test2
test5 = Abstraction "d" test1
test6 = Abstraction "a" test1
test7 = Aplication (Abstraction "d" test1) test1
test8 = Aplication test7 test4
test9 = Abstraction "e" test7
test10 = Aplication (Abstraction "y" (Aplication (Abstraction "x" (Aplication (Variable "y") (Variable "x"))) (test1))) (Variable "x")