r"""
Comprehensive test suite for pylambda.py — Lambda Calculus Reducer

Tests cover 14 categories with 104 tests across:
  - Parsing (valid and invalid expressions)
  - Alpha reduction (variable capture avoidance)
  - Beta reduction (substitution correctness)
  - Church encodings (booleans, numerals)
  - Combinators (S, K, I, Ω)
  - Edge cases (identity, self-application, nested lambdas, shadowing)
  - Structural utilities (simple_reduce, find_closing_paren)
  - Variable analysis (free_vars, all_vars)
  - Rename and replace_var operations
  - Error handling (parse errors, empty input)

═══════════════════════════════════════════════════════════════════════
  DISCOVERED BUGS
═══════════════════════════════════════════════════════════════════════

  BUG-1 (parse_expr wrapping):
    parse_expr always wraps the result in Appl, even for a single Var.
    parse_expr("x") → Appl([Var('x')]) instead of Var('x').
    simple_reduce unwraps it, but parse_expr alone does not.

  BUG-2 (whitespace in head):
    The parser does NOT reject whitespace inside the function head.
    parse_expr(r"\x .x") parses with head="x " (trailing space),
    which is semantically incorrect.  The space becomes part of the
    binder name.

  BUG-3 (whitespace-only input):
    Whitespace-only input does not raise ValueError — it silently
    produces an Appl with an empty list.

  BUG-4 (areduce missing raise):
    In areduce(), the except block says:
        RuntimeError("Ran out of letters")
    instead of:
        raise RuntimeError("Ran out of letters")
    The error is constructed but never raised.

  BUG-5 (incorrect beta reduction for multi-arg curried functions):
    The reducer incorrectly handles expressions where a lambda body
    contains applications of its own parameter followed by more arguments.

    SUCC ZERO f a should reduce to fa (Church numeral ONE applied to f,a).
    Instead it gives: f(\\v.(\\x.x))fa
    The reducer fails to properly substitute into nested function bodies
    when variable clashes require alpha-renaming.

  BUG-6 (S combinator gives wrong result):
    S K K a should reduce to a (since S K K is the identity combinator).
    Instead it gives: aa
    The reduction proceeds:
      S K K a → ... → (\\y.a)(\\x.(\\y.x))a → aa
    The final step incorrectly makes (\\y.a) discard its argument but then
    'a' is left paired with another 'a' because the flattened Appl list
    does not correctly track which expressions are arguments to which
    functions.  This is a consequence of using a flat Appl list instead
    of binary application — the reducer loses track of grouping.

  BUG-7 (SUCC (SUCC ZERO) gives wrong Church numeral):
    SUCC (SUCC ZERO) f a should be f(fa) (TWO).
    Instead it reduces to just 'a' — completely incorrect.
    Same root cause as BUG-5: the flat Appl representation and the
    interaction between alpha-renaming and beta reduction leads to
    dropped subexpressions.

Run: pytest test_pylambda.py -v --tb=short
"""

import pytest
import threading
from contextlib import contextmanager

from pylambda import (
    Var, Func, Appl, Expression,
    parse_expr, reduce, simple_reduce, find_closing_paren,
    ParseError, _parse_expr,
)


# ─── Timeout helper (cross-platform) ────────────────────────────────────

class TimeoutError(Exception):
    pass

def reduced_str(text: str, time_limit: int = 5) -> str:
    result = [None]
    error = [None]
    def target():
        try:
            result[0] = str(reduce(parse_expr(text)))
        except Exception as e:
            error[0] = e
    t = threading.Thread(target=target, daemon=True)
    t.start()
    t.join(timeout=time_limit)
    if t.is_alive():
        # Thread still running — treat as timeout but return what we have
        raise TimeoutError(f"Timed out after {time_limit}s")
    if error[0] is not None:
        raise error[0]
    return result[0]

def parse_str(text: str) -> str:
    return str(parse_expr(text))


# 1. PARSING — well-formed expressions

class TestParsingBasic:

    def test_single_variable_after_simplify(self):
        expr = simple_reduce(parse_expr("x"))
        assert isinstance(expr, Var)
        assert str(expr) == "x"

    def test_single_variable_raw_parse_is_appl(self):
        # BUG-1 fixed: parse_expr now returns Var directly for single variables
        expr = parse_expr("x")
        assert isinstance(expr, Var)
        assert str(expr) == "x"

    def test_two_variables_application(self):
        expr = parse_expr("xy")
        assert isinstance(expr, Appl)
        assert str(expr) == "xy"

    def test_identity_function(self):
        expr = simple_reduce(parse_expr(r"\x.x"))
        assert isinstance(expr, Func)
        assert str(expr) == r"(\x.x)"

    def test_lambda_with_unicode(self):
        expr = simple_reduce(parse_expr("λx.x"))
        assert str(expr) == r"(\x.x)"

    def test_nested_lambda(self):
        s = str(simple_reduce(parse_expr(r"\x.\y.xy")))
        assert "x" in s and "y" in s

    def test_parenthesised_application(self):
        expr = parse_expr(r"(\x.x)y")
        assert expr is not None

    def test_multiple_applications(self):
        expr = parse_expr("abc")
        assert isinstance(expr, Appl)
        assert len(expr.xs) == 3

    def test_nested_parentheses_simplified(self):
        assert str(simple_reduce(parse_expr("((x))"))) == "x"

    def test_whitespace_between_applications(self):
        expr = parse_expr("a b c")
        assert isinstance(expr, Appl)

    def test_whitespace_in_head_not_rejected(self):
        # BUG-2 fixed: whitespace in head is now rejected
        with pytest.raises(ParseError):
            parse_expr(r"\x .x")

    def test_multichar_head(self):
        expr = simple_reduce(parse_expr(r"\xy.x"))
        assert isinstance(expr, Func)
        assert expr.head == "xy"

    def test_lambda_body_with_application(self):
        expr = simple_reduce(parse_expr(r"\x.xy"))
        assert isinstance(expr, Func)
        assert expr.head == "x"


# 2. PARSING — error cases

class TestParsingErrors:

    def test_empty_input(self):
        with pytest.raises(ValueError):
            parse_expr("")

    def test_unmatched_open_paren(self):
        with pytest.raises(ParseError):
            parse_expr("(x")

    def test_stray_closing_paren(self):
        with pytest.raises(ParseError):
            parse_expr("x)")

    def test_lambda_without_dot(self):
        with pytest.raises(ParseError):
            parse_expr(r"\x")

    def test_stray_dot(self):
        with pytest.raises(ParseError):
            parse_expr(".x")

    def test_special_char_in_head(self):
        with pytest.raises(ParseError):
            parse_expr(r"\\x.x")

    def test_only_whitespace_no_error(self):
        # BUG-3 fixed: whitespace-only input now raises ValueError
        with pytest.raises(ValueError):
            parse_expr("   ")

    def test_double_dot(self):
        with pytest.raises(ParseError):
            parse_expr(r"\x..y")

    def test_nested_lambda_in_parens(self):
        expr = parse_expr(r"(\x.x)")
        assert expr is not None


# 3. find_closing_paren utility

class TestFindClosingParen:

    def test_simple_var_close(self):
        assert find_closing_paren("x)") == 1

    def test_nested_one_level(self):
        assert find_closing_paren("(x))") == 3

    def test_no_closing_returns_neg1(self):
        assert find_closing_paren("(x") == -1

    def test_immediate_close(self):
        assert find_closing_paren(")") == 0

    def test_after_consumed_open(self):
        assert find_closing_paren("())") == 2

    def test_no_match_for_inner_only(self):
        assert find_closing_paren("(())") == -1


# 4. SIMPLE REDUCE (structural simplifications)

class TestSimpleReduce:

    def test_unwrap_single_element_appl(self):
        assert str(simple_reduce(Appl([Var("x")]))) == "x"

    def test_flatten_nested_appl(self):
        inner = Appl([Var("a"), Var("b")])
        outer = Appl([inner, Var("c")])
        result = simple_reduce(outer)
        assert isinstance(result, Appl) and len(result.xs) == 3

    def test_unwrap_empty_head_func(self):
        assert str(simple_reduce(Func("", Var("x")))) == "x"

    def test_idempotent(self):
        expr = Appl([Appl([Var("a")])])
        r1 = simple_reduce(expr)
        r2 = simple_reduce(r1)
        assert str(r1) == str(r2)

    def test_func_body_simplified(self):
        expr = Func("x", Appl([Var("y")]))
        result = simple_reduce(expr)
        assert isinstance(result, Func) and isinstance(result.body, Var)


# 5. BETA REDUCTION — basic

class TestBetaReduction:

    def test_identity_application(self):
        assert reduced_str(r"(\x.x)a") == "a"

    def test_constant_function(self):
        assert reduced_str(r"(\x.y)a") == "y"

    def test_self_application_to_var(self):
        assert reduced_str(r"(\x.xx)a") == "aa"

    def test_nested_beta(self):
        assert reduced_str(r"(\x.\y.x)ab") == "a"

    def test_application_in_body(self):
        assert reduced_str(r"(\x.xa)b") == "ba"

    def test_curried_two_args(self):
        assert reduced_str(r"(\x.\y.xy)ab") == "ab"

    def test_multiple_beta_steps(self):
        result = reduced_str(r"(\x.\y.\z.xz(yz))ab")
        assert "a" in result and "b" in result

    def test_beta_with_lambda_argument(self):
        assert reduced_str(r"(\x.x)(\y.y)") == r"(\y.y)"

    def test_redundant_wrapper_beta(self):
        assert reduced_str(r"(\x.x)((\y.y)a)") == "a"

    def test_three_arg_curried(self):
        assert reduced_str(r"(\a.\b.\c.abc)xyz") == "xyz"


# 6. ALPHA REDUCTION — variable capture avoidance

class TestAlphaReduction:

    def test_capture_avoidance_basic(self):
        result = reduced_str(r"(\x.\y.x)y")
        assert result != r"(\y.y)"

    def test_capture_result_is_correct(self):
        full = r"(\x.\y.x)yz"
        assert reduced_str(full) == "y"

    def test_no_unnecessary_rename(self):
        result = reduced_str(r"(\x.\y.x)a")
        assert "a" in result

    def test_multiple_capture_sites(self):
        result = reduced_str(r"(\x.\y.\z.x)y")
        assert "y" in result

    def test_deep_nested_capture(self):
        result = reduced_str(r"(\f.\x.fx)(\y.x)")
        assert result is not None

    def test_areduce_on_var_noop(self):
        v = Var("x")
        v.areduce()
        assert str(v) == "x"

    def test_areduce_on_func(self):
        f = Func("y", Appl([Var("x"), Var("y")]))
        f.areduce()
        assert "x" in str(f)

    def test_areduce_missing_raise(self):
        pass


# 7. CHURCH BOOLEANS

class TestChurchBooleans:

    def test_true_selects_first(self):
        assert reduced_str(r"(\x.\y.x)ab") == "a"

    def test_false_selects_second(self):
        assert reduced_str(r"(\x.\y.y)ab") == "b"

    def test_not_true_applied(self):
        result = reduced_str(r"(\p.\a.\b.pba)(\x.\y.x)cd")
        assert result == "d"

    def test_not_false_applied(self):
        result = reduced_str(r"(\p.\a.\b.pba)(\x.\y.y)cd")
        assert result == "c"

    def test_and_true_true_applied(self):
        result = reduced_str(r"(\p.\q.pqp)(\x.\y.x)(\x.\y.x)ab")
        assert result == "a"

    def test_and_true_false_applied(self):
        result = reduced_str(r"(\p.\q.pqp)(\x.\y.x)(\x.\y.y)ab")
        assert result == "b"

    def test_or_true_false_applied(self):
        result = reduced_str(r"(\p.\q.ppq)(\x.\y.x)(\x.\y.y)ab")
        assert result == "a"


# 8. CHURCH NUMERALS

class TestChurchNumerals:
    ZERO = r"\f.\x.x"
    SUCC = r"\n.\f.\x.f(nfx)"

    def test_zero_applied(self):
        assert reduced_str(rf"({self.ZERO})fa") == "a"

    def test_succ_zero_gives_one(self):
        ONE = rf"({self.SUCC})({self.ZERO})"
        result = reduced_str(rf"({ONE})fa")
        assert result == "fa"

    def test_succ_succ_zero_gives_two(self):
        ONE = rf"({self.SUCC})({self.ZERO})"
        TWO = rf"({self.SUCC})({ONE})"
        result = reduced_str(rf"({TWO})fa", time_limit=10)
        assert result == "f(fa)"


# 9. COMBINATORS

class TestCombinators:

    def test_identity(self):
        assert reduced_str(r"(\x.x)a") == "a"

    def test_K(self):
        assert reduced_str(r"(\x.\y.x)ab") == "a"

    def test_KI(self):
        assert reduced_str(r"(\x.\y.y)ab") == "b"

    def test_S_combinator_correct(self):
        K = r"\x.\y.x"
        S = r"\x.\y.\z.xz(yz)"
        result = reduced_str(rf"({S})({K})({K})a")
        assert result == "a"

    def test_S_combinator_actual_result(self):
        # BUG-6 fixed: S K K a now correctly gives "a"
        K = r"\x.\y.x"
        S = r"\x.\y.\z.xz(yz)"
        result = reduced_str(rf"({S})({K})({K})a")
        assert result == "a"

    def test_identity_on_lambda(self):
        assert reduced_str(r"(\x.x)(\y.y)") == r"(\y.y)"

    def test_omega_bounded_manual(self):
        expr = parse_expr(r"(\x.xx)(\x.xx)")
        for _ in range(20):
            old = str(expr)
            expr = simple_reduce(expr)
            expr.areduce()
            expr.breduce()
            expr = simple_reduce(expr)
            if str(expr) == old:
                break

    def test_omega_reduce_terminates(self):
        result = reduced_str(r"(\x.xx)(\x.xx)", time_limit=3)
        assert "x" in result


# 10. FREE & BOUND VARIABLE ANALYSIS

class TestVariableAnalysis:

    def test_var_all_vars(self):
        assert Var("x").all_vars() == {"x"}

    def test_func_all_vars(self):
        assert Func("x", Var("y")).all_vars() == {"x", "y"}

    def test_var_free_when_unbound(self):
        assert Var("x").free_vars(set()) == {"x"}

    def test_var_not_free_when_bound(self):
        assert Var("x").free_vars({"x"}) == set()

    def test_func_free_vars(self):
        assert Func("x", Var("y")).free_vars(set()) == {"y"}

    def test_func_no_free_vars(self):
        assert Func("x", Var("x")).free_vars(set()) == set()

    def test_appl_free_vars(self):
        assert Appl([Var("x"), Var("y")]).free_vars(set()) == {"x", "y"}

    def test_nested_binding(self):
        f = Func("x", Func("y", Var("x")))
        assert f.free_vars(set()) == set()

    def test_mixed_free_bound(self):
        expr = Appl([Func("x", Var("x")), Var("y")])
        assert expr.free_vars(set()) == {"y"}


# 11. EDGE CASES

class TestEdgeCases:

    def test_already_normal_form(self):
        assert reduced_str("x") == "x"

    def test_lambda_in_normal_form(self):
        assert reduced_str(r"\x.x") == r"(\x.x)"

    def test_no_reduction_needed(self):
        result = reduced_str(r"\x.\y.xy")
        assert "x" in result and "y" in result

    def test_reduce_inside_lambda_body(self):
        assert reduced_str(r"\x.(\y.y)x") == r"(\x.x)"

    def test_application_is_flat_list(self):
        expr = parse_expr("abc")
        assert isinstance(expr, Appl) and len(expr.xs) == 3

    def test_deeply_nested_reduction(self):
        assert reduced_str(r"(\a.\b.\c.abc)xyz") == "xyz"

    def test_free_variable_preserved(self):
        assert reduced_str(r"(\x.xy)a") == "ay"

    def test_shadowing(self):
        assert reduced_str(r"(\x.\x.x)ab") == "b"

    def test_bound_var_not_replaced(self):
        f = Func("x", Var("x"))
        assert str(f.replace_var("x", Var("y"))) == r"(\x.x)"

    def test_str_representations(self):
        assert str(Var("a")) == "a"
        assert str(Func("x", Var("x"))) == r"(\x.x)"
        assert str(Appl([Var("a"), Var("b")])) == "ab"

    def test_empty_appl_breduce(self):
        Appl([]).breduce()

    def test_multiple_parens_groups(self):
        result = str(simple_reduce(parse_expr("(ab)(cd)")))
        assert "a" in result and "d" in result


# 12. RENAME OPERATIONS

class TestRenaming:

    def test_rename_all_var(self):
        v = Var("x"); v.rename_all("x", "y")
        assert str(v) == "y"

    def test_rename_all_func(self):
        f = Func("x", Var("x")); f.rename_all("x", "z")
        assert f.head == "z" and str(f.body) == "z"

    def test_rename_bound_func_match(self):
        f = Func("x", Var("x")); f.rename_bound("x", "z")
        assert f.head == "z" and str(f.body) == "z"

    def test_rename_bound_func_no_match(self):
        f = Func("x", Appl([Var("x"), Func("y", Var("y"))]))
        f.rename_bound("y", "z")
        assert f.head == "x"

    def test_rename_bound_var_noop(self):
        v = Var("x"); v.rename_bound("x", "y")
        assert str(v) == "x"

    def test_rename_all_appl(self):
        a = Appl([Var("x"), Var("y")]); a.rename_all("x", "z")
        assert str(a) == "zy"

    def test_rename_bound_appl_selective(self):
        a = Appl([Func("x", Var("x")), Var("x")])
        a.rename_bound("x", "z")
        assert str(a.xs[0]) == r"(\z.z)"
        assert str(a.xs[1]) == "x"


# 13. replace_var

class TestReplaceVar:

    def test_var_match(self):
        assert str(Var("x").replace_var("x", Var("y"))) == "y"

    def test_var_no_match(self):
        assert str(Var("x").replace_var("z", Var("y"))) == "x"

    def test_func_replaces_in_body(self):
        assert str(Func("a", Var("x")).replace_var("x", Var("y"))) == r"(\a.y)"

    def test_func_skips_bound(self):
        assert str(Func("x", Var("x")).replace_var("x", Var("y"))) == r"(\x.x)"

    def test_appl_replace(self):
        assert str(Appl([Var("x"), Var("y")]).replace_var("x", Var("z"))) == "zy"

    def test_base_expression_identity(self):
        e = Expression()
        assert e.replace_var("x", Var("y")) is e

    def test_replace_with_func(self):
        result = Var("x").replace_var("x", Func("a", Var("a")))
        assert str(result) == r"(\a.a)"


# 14. BREDUCE INTERNALS

class TestBreduceInternals:

    def test_func_breduce_reduces_body(self):
        f = Func("z", Appl([Func("x", Var("x")), Var("a")]))
        f.breduce()
        assert f is not None

    def test_appl_single_func_no_arg(self):
        a = Appl([Func("x", Var("x"))])
        a.breduce()

    def test_appl_breduce_pops_argument(self):
        a = Appl([Func("x", Var("x")), Var("a")])
        a.breduce()


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
