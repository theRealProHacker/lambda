-- http://learnyouahaskell.com/input-and-output#hello-world
import System.Environment (getArgs)
import Control.Monad (when)

data Expression = Var Char | Func String Expression | Appl [Expression] deriving (Show, Eq)

showExpr :: Expression -> String
showExpr (Var c) = [c]
showExpr (Func head body) = "(\\" ++ head ++ "." ++ (showExpr body) ++ ")"
showExpr (Appl xs) = foldl (++) [] $ map showExpr xs

_split_on :: String -> String -> Char -> (String, String)
_split_on s1 "" x = error ("Couldn't find expected '"++[x] ++ "' in \"" ++ s1 ++ "\"")
_split_on s1 (c:s2) x
    | c == x = (s1, s2)
    | otherwise = _split_on (s1++[c]) s2 x

-- Splits the String on the first occurence of the Char into two parts
-- Caller must ensure that Char is `elem` of String if errors should be avoided
split_on :: String -> Char -> (String, String)
split_on = _split_on ""

split_closing_paren' :: Int -> String -> String -> (String, String)
-- before needs to be reversed when it is used
split_closing_paren' _ before "" = error ("Couldn't find matching parentheses in \"" ++ reverse before ++ "\"")
split_closing_paren' 0 before (')':rest) = (reverse before, rest)
split_closing_paren' n before (')':rest) = split_closing_paren' (n-1) before rest
split_closing_paren' n before ('(':rest) = split_closing_paren' (n+1) before rest
split_closing_paren' n before (c:rest) = split_closing_paren' n (c:before) rest

-- Splits the String on the matching occurence of a closing parentheses into parts before and after
-- Raises an error if not found
split_closing_paren :: String -> (String, String)
split_closing_paren = split_closing_paren' 0 ""

tokenize :: (String,[Expression]) -> [Expression]
tokenize ("", exprs) = exprs
tokenize (('\\':rest), exprs) = let (head, body) = rest `split_on` '.' in
    -- \a.a b == (\a.a)b
    if ' ' `elem` body then
        let (body', rest') = body `split_on` ' ' in
            tokenize (rest', (Func head $ parse body'):exprs)
    else 
        tokenize ("", (Func head $ parse body):exprs)
tokenize (('(':rest), exprs) = let (inner, after) = split_closing_paren rest in
    tokenize (after, (parse inner): exprs)
-- Skip spaces, error on periods
tokenize ((' ':rest, exprs)) = tokenize (rest, exprs)
tokenize (('.':rest, exprs)) = error "Stray . found"
-- Any other chars are Vars
tokenize ((x:rest, exprs)) = tokenize (rest, Var x:exprs)

tokens :: String -> [Expression]
tokens string = tokenize (string, [])

parse :: String -> Expression
parse = Appl . reverse . tokens

-- β-reduces the expression once
reduce :: Expression -> Expression
-- (a) == a
reduce (Appl [x]) = reduce x
-- \.a == a
reduce (Appl ((Func [] body):rest)) = Appl (body:rest)
-- (\a.aa)b == bb
reduce (Appl ((Func [param] body):arg:arg_rest)) = Appl (replaceInBody param arg body:arg_rest)
-- (\ab.ba)c == (\b.bc)
reduce (Appl ((Func (param1:param_rest) body):arg1:arg_rest)) = Appl (Func param_rest (replaceInBody param1 arg1 body):arg_rest)
-- b((\a.a)b) == bb
reduce (Appl exprs) = Appl $ map reduce exprs
-- (\a.(\b.b)(\c.c)) == (\a.(\c.c))
reduce (Func head body) = Func head $ reduce body
--  ==  (blank expressions are parsed as "Appl []")
-- a == a
-- (\a.a) == (\a.a)
reduce x = x

-- Takes the variable name, the expression to replace, and the expression to replace in
replaceInBody :: Char -> Expression -> Expression -> Expression
-- Replace a var if it has the same name as c
replaceInBody c e var@(Var x) = if c==x then e else var
-- If the function itself binds c then we don't traverse into it
replaceInBody c e func@(Func head body) = if c `elem` head then func else Func head (replaceInBody c e body)
replaceInBody c e (Appl exprs) = Appl $ map (replaceInBody c e) exprs

-- For this you'd obviously love compile flags
debug = False

main_loop :: Expression -> IO(Expression)
main_loop x = do
    let y = reduce x
    if y == x then
        return y
    else do
        when debug $ do
            putStr $ show y
            putStr " -> "
            putStrLn $ showExpr y
        main_loop y

main = do
    x <- getArgs
    expr <- main_loop $ parse $ unwords x
    putStrLn $ showExpr expr