import Data.List
data LambdaTerm = Variable [Char] | Aplication LambdaTerm LambdaTerm | Abstraction [Char] LambdaTerm deriving (Eq, Show)

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

inNormalForm:: LambdaTerm -> Bool
inNormalForm a = redexes(a) == []

normalReduction:: LambdaTerm -> LambdaTerm
normalReduction (Aplication (Abstraction x m) n) = substitution m x n
normalReduction (Aplication m n)
    | m2 /= m = (Aplication m2 n)
    | otherwise = (Aplication m (normalReduction n))
    where m2 = normalReduction m

normalReduction (Abstraction x m) = Abstraction x (normalReduction m)
normalReduction a = a

applicativeReduction:: LambdaTerm -> LambdaTerm
applicativeReduction (Aplication (Abstraction x m) n) 
    | m2 == m && n2 == n = substitution m x n
    | m2 == m = Aplication (Abstraction x m) (applicativeReduction n)
    | otherwise = Aplication (Abstraction x (applicativeReduction m)) (n)
    where 
        m2 = applicativeReduction m
        n2 = applicativeReduction n
        
applicativeReduction (Aplication m n)
    | m2 /= m = (Aplication m2 n)
    | otherwise = (Aplication m (applicativeReduction n))
    where m2 = applicativeReduction m
    
applicativeReduction (Abstraction x m) = Abstraction x (applicativeReduction m)
applicativeReduction a = a

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