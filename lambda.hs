import Data.List

data LambdaTerm =
    Variable [Char]
    | Aplication LambdaTerm LambdaTerm
    | Abstraction [Char] LambdaTerm
    | KBool Bool
    | KInt Int
    | KIf
    | KLt
    | KMult
    | KSub
    deriving (Eq, Show)

toString:: LambdaTerm -> [Char]
toString (Variable a) = a
toString (Aplication a b) = "(" ++ toString a ++ " " ++ toString b ++ ")"
toString (Abstraction a b) = "(\x03bb"++ a ++ "." ++ toString b ++")"
toString (KBool a) = show a
toString (KInt a) = show a
toString (KIf) = "?"
toString (KLt) = "<"
toString (KMult) = "*"
toString (KSub) = "-"

freeVars:: LambdaTerm -> [[Char]]
freeVars (Variable a) = [a]
freeVars (Aplication a b) = nub(freeVars a ++ freeVars b)
freeVars (Abstraction a b) = delete a (freeVars b)
freeVars a = []

boundVars:: LambdaTerm -> [[Char]]
boundVars (Variable a) = []
boundVars (Aplication a b) = nub(boundVars a ++ boundVars b)
boundVars (Abstraction a b) = nub([a] ++ boundVars b)
boundVars a = []

substitution:: LambdaTerm -> [Char] -> LambdaTerm -> LambdaTerm
substitution (Variable a) b c = if (b == a) then c else (Variable a)
substitution (Aplication a b) c d = (Aplication (substitution a c d) (substitution b c d) )
substitution (Abstraction a b) c d = if a == c then (Abstraction a b) else (Abstraction a (substitution b c d))
substitution a b c = a

redexes:: LambdaTerm -> [LambdaTerm]
redexes (Variable a) = []
redexes l@(Aplication (Abstraction a b) c ) = [l] ++ redexes b ++ redexes c
redexes (Aplication a b ) = redexes a ++ redexes b
redexes (Abstraction a b ) = redexes b
redexes a = []


deltaRedexes:: LambdaTerm -> [LambdaTerm]
deltaRedexes l@(Aplication ((Aplication KLt (KInt a))) (KInt b)) = [l]
deltaRedexes l@(Aplication ((Aplication KMult (KInt a))) (KInt b)) = [l]
deltaRedexes l@(Aplication ((Aplication KSub (KInt a))) (KInt b)) = [l]
deltaRedexes l@(Aplication (Aplication (Aplication KIf (KBool a)) b) c) = [l] ++ deltaRedexes b ++ deltaRedexes c
deltaRedexes (Aplication a b ) = deltaRedexes a ++ deltaRedexes b
deltaRedexes (Abstraction a b ) = deltaRedexes b
deltaRedexes _ = []



inNormalForm:: LambdaTerm -> Bool
inNormalForm a = (redexes(a) == []) && ((deltaRedexes a) == [])

normalReduction:: LambdaTerm -> LambdaTerm
normalReduction (Aplication (Abstraction x m) n) = substitution m x n
normalReduction (Aplication (Aplication KLt (KInt a)) (KInt b)) = KBool (a < b)
normalReduction (Aplication (Aplication KMult (KInt a)) (KInt b)) = KInt (a * b)
normalReduction (Aplication (Aplication KSub (KInt a)) (KInt b)) = KInt (a - b)
normalReduction (Aplication (Aplication (Aplication KIf (KBool a)) b) c) = if a then b else c
normalReduction (Aplication m n)
    | m2 /= m = (Aplication m2 n)
    | otherwise = (Aplication m (normalReduction n))
    where m2 = normalReduction m

normalReduction (Abstraction x m) = Abstraction x (normalReduction m)
normalReduction a = a

applicativeReduction:: LambdaTerm -> LambdaTerm
applicativeReduction (Aplication (Aplication KLt (KInt a)) (KInt b)) = KBool (a < b)
applicativeReduction (Aplication (Aplication KMult (KInt a)) (KInt b)) = KInt (a * b)
applicativeReduction (Aplication (Aplication KSub (KInt a)) (KInt b)) = KInt (a - b)
applicativeReduction (Aplication (Aplication (Aplication KIf (KBool a)) b) c)
    |b == b2 && c == c2 = if a then b else c
    |b == b2 =  (Aplication (Aplication (Aplication KIf (KBool a)) b) (applicativeReduction c))
    |otherwise = (Aplication (Aplication (Aplication KIf (KBool a)) (applicativeReduction b)) c)
    where
        b2 = applicativeReduction b
        c2 = applicativeReduction c
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

fullNormalReduction:: LambdaTerm -> [LambdaTerm]
fullNormalReduction a
    | a2 == a = [a2]
    | otherwise = [a] ++ fullNormalReduction a2
    where a2 = normalReduction a

fullApplicativeReduction:: LambdaTerm -> [LambdaTerm]
fullApplicativeReduction a
    | a2 == a = [a2]
    | otherwise = [a] ++ fullApplicativeReduction a2
    where a2 = applicativeReduction a

nAbs:: [String] -> LambdaTerm -> LambdaTerm
nAbs vs b = foldr Abstraction b vs

nApl:: [LambdaTerm] -> LambdaTerm
nApl es = foldl1 Aplication es

nAplR:: [LambdaTerm] -> LambdaTerm
nAplR es = foldr1 Aplication es

nVar:: [String] -> LambdaTerm
nVar vs = nApl(map Variable vs)

nVarR:: [String] -> LambdaTerm
nVarR vs = nAplR(map Variable vs)

naturalToChurch:: Int -> LambdaTerm
naturalToChurch n = nAbs ["s", "z"] (nVarR (take n (repeat "s") ++ ["z"]))

true = Abstraction "x" (Abstraction "y" (Variable "x"))
false = Abstraction "x" (Abstraction "y" (Variable "y"))
andc = Abstraction "a" (Abstraction "b" (Aplication (Aplication (Variable "a") (Variable "b")) false))
ifc = Abstraction "c" (Abstraction "t" (Abstraction "f" (Aplication (Aplication (Variable "c") (Variable "t")) (Variable "f"))))
orc = Abstraction "a" (Abstraction "b" (Aplication(Aplication (Variable "a") true) (Variable "b")))
notc = Abstraction "a" (Aplication (Aplication (Variable "a") false) true)
zero = Abstraction "s" (Abstraction "z" (Variable "z"))
one = Abstraction "s" (Abstraction "z" (Aplication (Variable "s") (Variable "z")))
add = Abstraction "m" (Abstraction "n" (Abstraction "s" (Abstraction "z" (Aplication (Aplication (Variable "m") (Variable "s")) (Aplication (Aplication (Variable "n") (Variable "s")) (Variable "z"))))))
mult =  Abstraction "m" (Abstraction "n" (Abstraction "s" (Abstraction "z" (Aplication (Aplication (Variable "m") (Aplication (Variable "n") (Variable "s"))) (Variable "z")))))
operator =  nAbs ["x"] (nApl [(Variable "h"),nVar ["x", "x"]])
fixed_operator = nAbs  ["h"] (nApl [operator,operator])
factorial = nAbs ["f","n"] (nApl [KIf, nApl [KLt, (Variable "n"),(KInt 2)],(KInt 1),nApl [KMult,Variable "n",nApl[Variable "f",nApl[KSub,Variable "n",KInt 1]]]])


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
test11 = Aplication (Aplication (Aplication KIf (KBool True)) test10) test8
