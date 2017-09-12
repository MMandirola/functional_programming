data Expression = Number Integer | Sum Expression Expression | Prod Expression Expression

evaluate :: Expression -> Integer
evaluate (Number a) = a
evaluate (Sum a b) = evaluate(a) + evaluate(b)
evaluate (Prod a b) = evaluate(a) * evaluate(b)

toString :: Expression -> [Char]
toString (Number a) = show a
toString (Sum a b) =  "(" ++ toString a ++ " + " ++ toString b ++ ")"
toString (Prod a b) = toString a ++ " * " ++ toString b