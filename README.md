# \ambda - A Lambda calculator written in Haskell

This was made just for fun inspired by a computer science course in functional programming at Freie Universität Berlin. 

The amazing part is that it's about 100 lines of code with a lot of comments and a cli attached. Without all comments, type signatures and the cli interface, its a bit less than half of that!

## How to run

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

If you set `debug` to `True` you can see the steps of the reduction process, which is perfect for introspection because it also dumps the ast.

## In the interpreter

You can also run [`ilambda.hs`](ilambda.hs) in an interactive ghci session. But don't forget to use double backslashes (\\\\), which in Markdown ironically needs 4 backslashes. 

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

## The future of this

### Files and variables

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
Also, I'd probably rather implement that in Python or really anything else but in Haskell or Brainfuck.

### Haskell transpiler

Combined with the above you could convert your lambda expressions into haskell code. 

`x = \ab.ab` would become

```hs
x a b = a b
```

## Random Fun
Just for fun I put the smallest "possible" compressed version of [`ilambda.hs`](ilambda.hs) (the already compressed version) here, which is just 24 lines of code. I'll bet you that you can't make it much smaller. The code is technically not exactly the same because errors are thrown implicitly but nice and shiny explicit errors that tell you exactly where you went wrong aren't really a requirement. 

```hs
data Expression = Var Char | Func String Expression | Appl [Expression] deriving (Show, Eq)
showExpr (Var c) = [c]
showExpr (Func head body) = "(\\" ++ head ++ "." ++ (showExpr body) ++ ")"
showExpr (Appl xs) = foldl (++) [] $ map showExpr xs
_split_on s1 (c:s2) x = if c == x then (s1, s2) else _split_on (s1++[c]) s2 x
split_closing_paren' n before (c:rest) = if c == ')' then if n==0 then (reverse before, rest) else split_closing_paren' (n-1) before rest else if c=='(' then split_closing_paren' (n+1) before rest else split_closing_paren' n (c:before) rest
split_closing_paren = split_closing_paren' 0 ""
tokenize ("", exprs) = exprs
tokenize (('\\':rest), exprs) = let (head, body) = _split_on "" rest '.' in if ' ' `elem` body then let (body', rest') = _split_on "" body ' ' in tokenize (rest', (Func head $ parse body'):exprs) else tokenize ("", (Func head $ parse body):exprs)
tokenize (('(':rest), exprs) = let (inner, after) = split_closing_paren rest in tokenize (after, (parse inner): exprs)
tokenize ((' ':rest, exprs)) = tokenize (rest, exprs)
tokenize ((x:rest, exprs)) = tokenize (rest, Var x:exprs)
parse string = Appl $ reverse $ tokenize (string, [])
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
```