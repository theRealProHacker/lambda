# \ambda - A Lambda calculator written in Haskell

> Inspired by the course "Funktionale Programmierung" @ FU Berlin

A suite of programs to evaluate lambda calculus expressions. Try it out right now [on the web](https://therealprohacker.github.io/lambda/)

# How to run locally

First you need to install git and clone the repo
```shell
> git clone https://github.com/theRealProHacker/lambda.git
> cd lambda
```

## Python

Run the Python script very easily. No dependencies required but at least Python 3.10!

```shell
> py pylambda.py
```

The script will then ask you to input your lambda expression and evaluate it, when you hit enter. 

## Haskell

The amazing part is that it's about 100 lines of code with a lot of comments and a cli attached. Without all comments, type signatures and the cli interface, its a bit less than half of that!
Make sure that you have [`ghc`](https://www.haskell.org/ghc/) installed and in your Path. Then you can compile [`lambda.hs`](lambda.hs).

```shell
> ghc --make lambda
```

Now you can run the executable like this.

```shell
> lambda (\kbc.k(bb)c)(\xy.xya)(\f.f)
(\c.ac)
```

> Note: These examples are for Windows. On Linux replace `lambda` with `./lambda`

As you may have noticed, the λ is replaced with a back slash (\\) 
inspired by the notation for lambda functions in Haskell. 
Also whitespaces have a significance as they end the body of a function.

```shell
> lambda \kbc.k(bb)c \xy.xya \f.f
(\c.ca)
```

### Run without compiling
Alternatively you can run the code without compiling it.

```shell
> runhaskell lambda \kbc.k(bb)c \xy.xya \f.f
(\c.ca)
```

### Running in debug mode

In [`lambda.hs`](lambda.hs) you can see this line

```hs
debug = False
```

If you set `debug` to `True` you can see the steps of the reduction process, which is perfect for introspection because it also dumps the ast at every step.

### In the interpreter

You can also run [`ilambda.hs`](ilambda.hs) in an interactive ghci session. But don't forget to use double backslashes (\\\\) (which in Markdown ironically needs 4 backslashes). 

```shell
> ghci ilambda
GHCi, version 8.10.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( ilambda.hs, interpreted )
Ok, one module loaded.
*Main> (\a.a)b
"*** Exception: Stray . found 
CallStack (from HasCallStack):
  error, called at ilambda.hs:30:32 in main:Main
*Main> beta "(\\a.a)b"
"b"
```

# **WTF** is lambda calculus

From [Wikipedia - the free encyclopedia](https://en.wikipedia.org/wiki/Lambda_calculus)

> Lambda calculus (also written as λ-calculus) is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution. It is a universal model of computation that can be used to simulate any Turing machine. It was introduced by the mathematician Alonzo Church in the 1930s as part of his research into the foundations of mathematics.

It is basically a first programming language from the 1930s before computers even existed! To use it here replace the lambda symbol (λ) with a back slash (\). 

# The future of this

## Files and variables

Going further I'd like to introduce a file input mode where you can define variables that can then be used globally.

```hs
// define Variables
True = (\tf.t)
False = (\tf.f)

// and use them
And = \ab.ab(False)
Or = \ab.a(True)b
```

But that'd just become more and more like Haskell.

## Haskell transpiler

Combined with the above you could convert your lambda expressions into haskell code. 

`x = \ab.ab` would become

```hs
x a b = a b
```

# Random Fun
Just for fun I put the smallest "possible" compressed version of [`ilambda.hs`](ilambda.hs) (the already compressed version) here, which is just 5 lines of code. I'll bet you that you can't make it much smaller. The code is technically not exactly the same because errors are thrown implicitly but nice and shiny explicit errors that tell you exactly where you went wrong aren't really a requirement. 

```hs
data Expression = Var Char | Func String Expression | Appl [Expression] deriving (Show, Eq);showExpr x = case x of (Var c) -> [c]; (Func head body) -> "(\\" ++ head ++ "." ++ (showExpr body) ++ ")"; (Appl xs) -> foldl (++) [] $ map showExpr xs;
_split_on s1 (c:s2) x = if c == x then (s1, s2) else _split_on (s1++[c]) s2 x;split_closing_paren' n before (c:rest) = if c == ')' then if n==0 then (reverse before, rest) else split_closing_paren' (n-1) before rest else if c=='(' then split_closing_paren' (n+1) before rest else split_closing_paren' n (c:before) rest;tokenize exprs str = case str of ""->exprs; '\\':rest -> let (head, body) = _split_on "" rest '.' in if ' ' `elem` body then let (body', rest') = _split_on "" body ' ' in tokenize ((Func head $ parse body'):exprs) rest' else tokenize ((Func head $ parse body):exprs) ""; '(':rest -> let (inner, after) = (split_closing_paren' 0 "" rest) in tokenize ((parse inner): exprs) after; ' ':rest -> tokenize exprs rest; x:rest -> tokenize (Var x:exprs) rest;
parse = Appl . reverse . tokenize [];replaceArg c e expr = case expr of var@(Var x) -> if c==x then e else var; func@(Func head body) -> if c `elem` head then func else Func head (replaceArg c e body); (Appl exprs) -> Appl $ map (replaceArg c e) exprs;
reduce x = case x of (Appl [x]) -> reduce x; (Appl ((Func [] body):rest)) -> reduce $ Appl (body:rest); (Appl ((Func [param] body):arg:arg_rest)) -> reduce $ Appl (replaceArg param arg body:arg_rest); (Appl ((Func (param1:param_rest) body):arg1:arg_rest)) -> reduce $ Appl (Func param_rest (replaceArg param1 arg1 body):arg_rest); (Appl exprs) -> reduce $ Appl $ map reduce exprs; (Func head body) -> Func head $ reduce body; _ -> x;
beta = showExpr . reduce . parse
```
