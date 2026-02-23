"""
This is a lambda reducer (α and β) in Python

It is mostly just a copy of the Haskell-Code 
but also does alpha-reduction which the Haskell-version cannot provide
"""

import re
from itertools import chain
from string import ascii_letters, whitespace
from dataclasses import dataclass

Char = str
letters = set(ascii_letters)
whitespace_pattern = re.compile(r"^\s+")

class Expression: 
    def all_vars(self)->set[Char]:
        ...

    def free_vars(self, bound_vars: set[Char])->set[Char]:
        ...

    def rename_bound(self, var: Char, into: Char):
        ...

    def rename_all(self, var: Char, into: Char):
        ...

    def areduce(self):
        r"""
        We need to alpha reduce when an expression 
        has a free variable and a bound variable at the same time

        (\ha.ha)a -> (\hb.hb)a

        Because beta reducing the first term yields (\a.aa) which is different
        to (\b.ab) which is the result of beta reducing the latter term.
        """
        free = self.free_vars(set())
        # _all = all_vars(expr)
        for f in free:
            rest = letters - self.all_vars()
            try:
                self.rename_bound(f, rest.pop())
            except KeyError:
                raise RuntimeError("Ran out of letters")

    def breduce(self):
        ...

    def replace_var(self, var: str, wth: "Expression")->"Expression":
        return self

@dataclass
class Var(Expression):
    char: Char

    def __str__(self):
        return self.char
    
    def all_vars(self)->set[Char]:
        return {self.char}
    
    def free_vars(self, bound_vars: set[Char])->set[Char]:
        return {self.char} - bound_vars
    
    def rename_bound(self, var: Char, into: Char):
        pass
    
    def rename_all(self, var: Char, into: Char):
        self.char = self.char.replace(var, into)

    def replace_var(self, var: str, wth: Expression)->Expression:
        return wth if self.char == var else self
            

@dataclass
class Func(Expression):
    head: str
    body: Expression

    def __str__(self):
        return rf"(\{self.head}.{self.body})"
    
    def all_vars(self)->set[Char]:
        return set(self.head) | self.body.all_vars()
    
    def free_vars(self, bound_vars: set[Char])->set[Char]:
        return self.body.free_vars(bound_vars | set(self.head)) - bound_vars
    
    def rename_bound(self, var: Char, into: Char):
        if var in self.head:
            self.head = self.head.replace(var, into)
            self.body.rename_all(var, into)
        else:
            self.body.rename_bound(var, into)
            
    def rename_all(self, var: Char, into: Char):
        self.head = self.head.replace(var, into)
        self.body.rename_all(var, into)

    def replace_var(self, var: str, wth: Expression)->Expression:
        # Don't replace bound variables inside the functions body
        if not var in self.head:
            return Func(self.head, self.body.replace_var(var, wth))
        return self
    
    def breduce(self):
        self.body.breduce()


@dataclass
class Appl(Expression):
    xs: list[Expression]

    def __str__(self):
        parts = []
        for i, x in enumerate(self.xs):
            s = str(x)
            if i > 0 and type(x) is Appl:
                s = f"({s})"
            parts.append(s)
        return "".join(parts)
    
    def all_vars(self)->set[Char]:
        return set(chain.from_iterable(x.all_vars() for x in self.xs))
    
    def free_vars(self, bound_vars: set[Char])->set[Char]:
        return set(chain.from_iterable(x.free_vars(bound_vars) for x in self.xs)) - bound_vars
    
    def rename_bound(self, var: Char, into: Char):
        for expr in self.xs:
            expr.rename_bound(var, into)
            
    def rename_all(self, var: Char, into: Char):
        for expr in self.xs:
            expr.rename_all(var, into)

    def replace_var(self, var: str, wth: Expression)->Expression:
        return Appl([x.replace_var(var, wth) for x in self.xs])
    
    def breduce(self):
        if not self.xs:
            return self
        elif len(self.xs)>=2 and type(func:=self.xs[0]) is Func and func.head:
            param, func.head = func.head[0], func.head[1:]
            func.body = func.body.replace_var(param, self.xs.pop(1))
        else:
            for expr in self.xs:
                expr.breduce()


class ParseError(ValueError, SyntaxError):
    pass


def find_closing_paren(text: str):
    count = 1
    for i,c in enumerate(text):
        if c == ")":
            count -= 1
            if count == 0:
                return i
        elif c == "(":
            count += 1
    return -1


def _parse_expr(rest: str) -> list[Expression]:
    lambda_chars = r"\λ"
    # can't extend these below
    dot_chars = r"."
    paren_chars = r"()"
    special_chars = lambda_chars + dot_chars + paren_chars
    result = []
    while rest:
        first, rest = rest[0], rest[1:]
        # function
        if first in lambda_chars:
            if dot_chars not in rest:
                raise ParseError("Found a function without a dot")
            head, body = rest.split(".", 1)
            if any(c in special_chars for c in head):
                raise ParseError("Found a special character in the head of a function")
            if any(c in whitespace for c in head):
                raise ParseError("Found whitespace in the head of a function")
            rest = ""
            if " " in body:
                count = 0
                for i,c in enumerate(body):
                    if c == ")":
                        count -= 1
                    elif c == "(":
                        count += 1
                    elif whitespace_pattern.match(body[i:]) and not count:
                        body, rest = body[:i], body[i:].lstrip()
                        break
            result.append(Func(head, parse_expr(body)))
        elif first == "(":
            closing = find_closing_paren(rest)
            if closing == -1:
                raise ParseError("Found opening parentheses without matching closing parentheses")
            inside, rest = rest[:closing], rest[closing+1:]
            result.append(parse_expr(inside))
        elif first in dot_chars:
            raise ParseError("Found a stray dot")
        elif first == ")":
            raise ParseError("Found stray closing parantheses")
        elif first in whitespace:
            continue
        else:
            result.append(Var(first))
    return result

def parse_expr(text: str) -> Expression:
    if not text:
        raise ValueError("Can't parse empty input")
    if not text.strip():
        raise ValueError("Can't parse whitespace-only input")
    result = _parse_expr(text)
    if len(result) == 1:
        return result[0]
    return Appl(result)

def reduce(expr: Expression)->Expression:
    while True:
        old_expr = str(expr)
        expr = simple_reduce(expr)
        reduction_type = "alpha"
        expr.areduce()
        if old_expr == str(expr):
            reduction_type = "beta"
            expr.breduce()
        expr = simple_reduce(expr)
        if old_expr == str(expr):
            return expr
        print(f"Step ({reduction_type}): {old_expr}->{expr}")

def simple_reduce(expr: Expression)->Expression:
    match expr:
        case Func("", x):
            return simple_reduce(x)
        case Func(head, body):
            return Func(head, simple_reduce(body))
        case Appl([x]):
            return simple_reduce(x)
        case Appl(xs):
            new_xs = []
            for i, x in enumerate(xs):
                if i == 0 and type(x) is Appl:
                    new_xs.extend(x.xs)
                else:
                    new_xs.append(x)
            expr.xs = [simple_reduce(x) for x in new_xs]
            return expr
    return expr


if __name__ == "__main__":
    text = input("Please provide your expression: ")
    print(reduce(parse_expr(text)))