import Data.List
data LambdaTerm = Variable [Char] | Aplication LambdaTerm LambdaTerm | Abstraction [Char] LambdaTerm deriving (Show)

toString:: LambdaTerm -> [Char]
toString (Variable a) = a
toString (Aplication a b) = toString(a) ++ " " ++ toString(b)
toString (Abstraction a b) = "(\x03bb"++ a ++ "." ++ toString(b)++")"

freeVars:: LambdaTerm -> [[Char]]
freeVars (Variable a) = [a]
freeVars (Aplication a b) = nub(freeVars(a) ++ freeVars(b))
freeVars (Abstraction a b) = delete a (freeVars(b))

boundVars:: LambdaTerm -> [[Char]]
boundVars (Variable a) = []
boundVars (Aplication a b) = nub(boundVars(a) ++ boundVars(b))
boundVars (Abstraction a b) = nub([a] ++ boundVars(b))

substitution:: LambdaTerm -> [Char] -> LambdaTerm -> LambdaTerm
substitution (Variable a) b c = if (b == a) then c else (Variable a)
substitution (Aplication a b) c d = (Aplication (substitution a c d) (substitution b c d) )
substitution (Abstraction a b) c d = if a == c then (Abstraction a b) else (Abstraction a (substitution b c d))

test1 = Variable "a"
test2 = Aplication test1 test1
test3 = Abstraction "a" test2
test4 = Aplication test3 test2
test5 = Abstraction "a" test4
test6 = Abstraction "b" test5
test7 = Aplication test1 test2
test8 = Aplication test4 test5
test9 = Aplication test4 test4
test10 = Aplication test3 test1