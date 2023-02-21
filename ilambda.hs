data Expression = Var Char | Func String Expression | Appl [Expression] deriving (Show, Eq)

showExpr (Var c) = [c]
showExpr (Func head body) = "(\\" ++ head ++ "." ++ (showExpr body) ++ ")"
showExpr (Appl xs) = foldl (++) [] $ map showExpr xs

_split_on s1 "" x = error ("Couldn't find expected '"++[x] ++ "' in \"" ++ s1 ++ "\"")
_split_on s1 (c:s2) x = if c == x then (s1, s2) else _split_on (s1++[c]) s2 x

split_on = _split_on ""

split_closing_paren' _ before "" = error ("Couldn't find matching parentheses in \"" ++ reverse before ++ "\"")
split_closing_paren' 0 before (')':rest) = (reverse before, rest)
split_closing_paren' n before (')':rest) = split_closing_paren' (n-1) before rest
split_closing_paren' n before ('(':rest) = split_closing_paren' (n+1) before rest
split_closing_paren' n before (c:rest) = split_closing_paren' n (c:before) rest

split_closing_paren = split_closing_paren' 0 ""

tokenize ("", exprs) = exprs
tokenize (('\\':rest), exprs) = let (head, body) = rest `split_on` '.' in 
    if ' ' `elem` body then 
        let (body', rest') = body `split_on` ' ' in 
            tokenize (rest', (Func head $ parse body'):exprs) 
    else 
        tokenize ("", (Func head $ parse body):exprs)
tokenize (('(':rest), exprs) = let (inner, after) = split_closing_paren rest in
    tokenize (after, (parse inner): exprs)
tokenize ((' ':rest, exprs)) = tokenize (rest, exprs)
tokenize (('.':rest, exprs)) = error "Stray . found"
tokenize ((x:rest, exprs)) = tokenize (rest, Var x:exprs)

tokens string = tokenize (string, [])

parse = Appl . reverse . tokens

replaceArg c e var@(Var x) = if c==x then e else var
replaceArg c e func@(Func head body) = if c `elem` head then func else Func head (replaceArg c e body)
replaceArg c e (Appl exprs) = Appl $ map (replaceArg c e) exprs

reduce (Appl [x]) = reduce x
reduce (Appl ((Func [] body):rest)) = reduce $ Appl (body:rest)
reduce (Appl ((Func [param] body):arg:arg_rest)) = reduce $ Appl (replaceArg param arg body:arg_rest)
reduce (Appl ((Func (param1:param_rest) body):arg1:arg_rest)) = reduce $ Appl (Func param_rest (replaceArg param1 arg1 body):arg_rest)
reduce (Appl exprs) = reduce $ Appl $ map reduce exprs
reduce (Func head body) = Func head $ reduce body
reduce x = x

beta = showExpr . reduce . parse