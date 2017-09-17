import Data.List
data LambdaTerm = Variable [Char] | Aplication LambdaTerm LambdaTerm | Abstraction [Char] LambdaTerm deriving (Show, Eq)

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

combinations:: Int -> [[Char]]
combinations 0 = []
combinations 1 = map (:[]) ['a'..'z']
combinations n = [y++x | x <- (combinations 1), y <- (combinations (n-1))]

variableNames::[[Char]]
variableNames = concat(map combinations [1..])

firstnotina:: [[Char]] -> [Char]
firstnotina a = (filter (\x ->(elemIndex x a)== Nothing) variableNames) !! 0

alphaConversion:: LambdaTerm -> [[Char]] -> LambdaTerm
alphaConversion l@(Variable a) b = l
alphaConversion (Aplication l@(Abstraction a b) c ) d = (Aplication (alphaConversion l (d++(freeVars c))) (alphaConversion c (d++(freeVars c))))
alphaConversion (Aplication a b) c= (Aplication (alphaConversion a c) (alphaConversion b c))
alphaConversion (Abstraction a b) c = if ((elemIndex a c) /= Nothing) then alphaConversion (Abstraction (firstnotina c) (substitution b a (Variable (firstnotina c)))) c else (Abstraction a (alphaConversion b c))

isNF::LambdaTerm -> Bool
isNF a = redexes(a) == []

reduceNO:: LambdaTerm -> LambdaTerm
reduceNO (Aplication (Abstraction a b) c) = reduceNO(substitution b a c)
reduceNO (Aplication a b) = (Aplication (reduceNO a) (reduceNO b))
reduceNO (Abstraction a b) = (Abstraction a (reduceNO b))
reduceNO a = a

reduceAO::LambdaTerm -> LambdaTerm
reduceAO (Aplication (Abstraction a b) c) = if (isNF b) then (if (isNF c) then substitution b a c else (reduceAO (Aplication (Abstraction a b) (reduceAO c)))) else (reduceAO ((Aplication (Abstraction a (reduceAO b)) c)))
reduceAO (Aplication a b) = (Aplication (reduceAO a) (reduceAO b))
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
