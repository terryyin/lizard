"""
Cognitive Complexity extension (-Ecognitive).

The expected values follow SonarSource's white paper "Cognitive Complexity,
a new way of measuring understandability" (G. Ann Campbell).  Comments like
``// +2 (nesting=1)`` use the paper's notation.
"""
import os
import subprocess
import sys
import unittest
from unittest.mock import patch

from lizard import FileAnalyzer, get_extensions, parse_args, analyze_file
import lizard
from lizard_ext.lizardcognitive import LizardExtension as Cognitive


def analyze(filename, code):
    return FileAnalyzer(get_extensions([Cognitive()])).analyze_source_code(
        filename, code)


def cogc(filename, code):
    """The cognitive complexity of every function, in source order."""
    return [f.cognitive_complexity for f in analyze(filename, code).function_list]


def cpp(code):
    return cogc("a.cpp", code)


def java(code):
    return cogc("a.java", "class A {" + code + "}")


def py(code):
    return cogc("a.py", code)


class TestCppIncrements(unittest.TestCase):

    def test_no_increment_for_the_function_itself(self):
        self.assertEqual([0], cpp("int f() { return g(1, 2); }"))

    def test_if(self):
        self.assertEqual([1], cpp("void f() { if (a) x(); }"))

    def test_if_else(self):
        self.assertEqual([2], cpp("void f() { if (a) { x(); } else { y(); } }"))

    def test_if_else_if_else_chain_costs_one_each(self):
        self.assertEqual([3], cpp("""
            void f() {
              if (a) { x(); }        // +1
              else if (b) { y(); }   // +1
              else { z(); }          // +1
            }"""))

    def test_braceless_if_else_if_else(self):
        self.assertEqual([3], cpp("void f() { if (a) x(); else if (b) y(); else z(); }"))

    def test_loops(self):
        self.assertEqual([1], cpp("void f() { for (int i = 0; i < n; i++) x(); }"))
        self.assertEqual([1], cpp("void f() { while (a) x(); }"))
        self.assertEqual([1], cpp("void f() { for (auto& e : v) x(e); }"))

    def test_do_while_counts_once(self):
        self.assertEqual([1], cpp("void f() { do { x(); } while (a); }"))
        self.assertEqual([1], cpp("void f() { do x(); while (a); }"))

    def test_switch_counts_once_regardless_of_cases(self):
        self.assertEqual([1], cpp("""
            const char* getWords(int number) {
              switch (number) {              // +1
                case 1: return "one";
                case 2: return "a couple";
                case 3: return "a few";
                default: return "lots";
              }
            }"""))

    def test_ternary_operator(self):
        self.assertEqual([1], cpp("int f() { return a ? b : c; }"))

    def test_gnu_elvis_is_null_coalescing(self):
        self.assertEqual([0], cpp("int f() { return a ?: c; }"))

    def test_catch_counts_once_per_clause_try_and_finally_do_not(self):
        self.assertEqual([2], cpp("""
            void f() {
              try { x(); }
              catch (const A& a) { y(); }   // +1
              catch (...) { z(); }          // +1
            }"""))

    def test_goto(self):
        self.assertEqual([1], cpp("void f() { goto out; out: return; }"))

    def test_plain_break_continue_and_return_do_not_count(self):
        self.assertEqual([1], cpp("""
            void f() {
              while (a) {         // +1
                break;
                continue;
              }
              return;
            }"""))

    def test_direct_recursion_counts_once(self):
        self.assertEqual([2], cpp("""
            int fact(int n) {
              if (n <= 1) return 1;          // +1
              return n * fact(n - 1);        // +1 recursion
            }"""))
        self.assertEqual([1], cpp("""
            int f(int n) {
              return f(n - 1) + f(n - 2);    // +1, only once
            }"""))

    def test_method_call_with_same_name_on_another_object_is_not_recursion(self):
        self.assertEqual([0], cpp("int A::size() { return other.size(); }"))
        self.assertEqual([1], cpp("int A::size() { return this->size(); }"))


class TestCppLogicalOperatorSequences(unittest.TestCase):

    def test_one_sequence_of_like_operators(self):
        self.assertEqual([2], cpp("void f() { if (a && b && c && d) x(); }"))
        self.assertEqual([2], cpp("void f() { if (a || b || c || d) x(); }"))

    def test_each_change_of_operator_is_a_new_sequence(self):
        self.assertEqual([4], cpp("""
            void f() {
              if (a          // +1 for if
                  && b && c  // +1
                  || d || e  // +1
                  && f)      // +1
                x();
            }"""))

    def test_parentheses_start_a_new_sequence(self):
        self.assertEqual([3], cpp("""
            void f() {
              if (a          // +1 for if
                  &&         // +1
                  !(b && c)) // +1
                x();
            }"""))

    def test_sequences_in_assignments_and_returns_count(self):
        self.assertEqual([2], cpp("bool f() { bool x = a && b; return c || d; }"))

    def test_sequences_in_different_statements_are_distinct(self):
        self.assertEqual([2], cpp("void f() { x = a && b; y = c && d; }"))

    def test_rvalue_references_are_not_logical_and(self):
        self.assertEqual([0], cpp("void f(T&& x) { }"))
        self.assertEqual([0], cpp("void f() { auto&& y = std::move(z); }"))
        self.assertEqual([1], cpp("void f() { for (auto&& e : v) x(e); }"))

    def test_rvalue_reference_in_lambda_parameter_is_not_counted(self):
        self.assertEqual([0], cpp("void f() { auto l = [](T&& t) { return t; }; }"))


class TestCppNesting(unittest.TestCase):

    def test_nested_structures_pay_for_their_depth(self):
        self.assertEqual([6], cpp("""
            void f() {
              if (a) {             // +1
                for (;;) {         // +2 (nesting=1)
                  while (b) { }    // +3 (nesting=2)
                }
              }
            }"""))

    def test_sequential_structures_do_not(self):
        self.assertEqual([3], cpp("""
            void f() {
              if (a) { }
              for (;;) { }
              while (b) { }
            }"""))

    def test_else_and_else_if_have_no_nesting_penalty_but_nest_their_body(self):
        self.assertEqual([9], cpp("""
            int nested(int a, int b) {
              if (a) {            // +1
                if (b) {          // +2 (nesting=1)
                  while (c) {}    // +3 (nesting=2)
                } else if (d) {   // +1
                  x = 1;
                } else {          // +1
                  x = 2;
                }
              }
              return a ? b : c;   // +1
            }"""))
        self.assertEqual([4], cpp("""
            void f() {
              if (a) { }          // +1
              else {              // +1
                if (b) { }        // +2 (nesting=1)
              }
            }"""))

    def test_braceless_bodies_nest(self):
        self.assertEqual([3], cpp("void f() { if (a) if (b) x(); }"))
        self.assertEqual([3], cpp("""
            void f() {
              for (;;)
                if (a)
                  x();
            }"""))

    def test_dangling_else_binds_to_the_inner_if(self):
        self.assertEqual([4], cpp("void f() { if (a) if (b) x(); else y(); }"))

    def test_try_and_finally_do_not_nest(self):
        self.assertEqual([9], java("""
            void myMethod () {
              try {
                if (condition1) {                   // +1
                  for (int i = 0; i < 10; i++) {    // +2 (nesting=1)
                    while (condition2) { }          // +3 (nesting=2)
                  }
                }
              } catch (ExcepType1 | ExcepType2 e) { // +1
                if (condition2) { }                 // +2 (nesting=1)
              }
            }"""))

    def test_switch_nests_its_cases(self):
        self.assertEqual([3], cpp("""
            void f() {
              switch (x) {          // +1
                case 1: if (a) x(); // +2 (nesting=1)
                        break;
                default: break;
              }
            }"""))

    def test_ternary_nests_and_is_nested(self):
        self.assertEqual([3], cpp("int f() { return a ? b : c ? d : e; }"))
        self.assertEqual([3], cpp("void f() { if (a) x = b ? c : d; }"))

    def test_lambda_nests_without_an_increment(self):
        self.assertEqual([2], cpp("""
            void f() {
              auto l = [](int a) {   // +0 (nesting=1)
                if (a) return 1;     // +2
                return 0;
              };
            }"""))
        self.assertEqual([2], cpp("""
            void f() {
              std::sort(v.begin(), v.end(), [&](int a, int b) mutable -> bool {
                if (a) return true;  // +2 (nesting=1)
                return a < b;
              });
            }"""))

    def test_initializer_braces_are_not_blocks(self):
        self.assertEqual([3], cpp("""
            void f() {
              if (a) {               // +1
                int v[] = {1, 2, 3};
                if (b) x({4, 5});    // +2 (nesting=1)
              }
            }"""))

    def test_nested_class_or_struct_in_function_body(self):
        self.assertEqual([2], cpp("""
            void f() {
              struct L { int g() { return 1; } };
              if (a) { }             // +1
              if (b) { }             // +1
            }"""))

    def test_structures_in_function_signature_default_arguments_are_ignored(self):
        self.assertEqual([1], cpp("int f(int a = b ? 1 : 2) { if (a) return 1; return 0; }"))


class TestSpecificationAppendixCExamples(unittest.TestCase):
    """Appendix C of the white paper, verbatim (Java)."""

    def test_sum_of_primes(self):
        self.assertEqual([7], java("""
            int sumOfPrimes(int max) {
              int total = 0;
              OUT: for (int i = 1; i <= max; ++i) { // +1
                for (int j = 2; j < i; ++j) {       // +2
                  if (i % j == 0) {                 // +3
                    continue OUT;                   // +1
                  }
                }
                total += i;
              }
              return total;
            }"""))

    def test_lambda_increments_nesting_level(self):
        self.assertEqual([2], java("""
            void myMethod2 () {
              Runnable r = () -> {          // +0 (but nesting level is now 1)
                if (condition1) { }         // +2 (nesting=1)
              };
            }"""))

    def test_overridden_symbol_from(self):
        self.assertEqual([19], java("""
            private MethodJavaSymbol overriddenSymbolFrom(ClassJavaType classType) {
              if (classType.isUnknown()) {                          // +1
                return Symbols.unknownMethodSymbol;
              }
              boolean unknownFound = false;
              List<JavaSymbol> symbols = classType.getSymbol().members().lookup(name);
              for (JavaSymbol overrideSymbol : symbols) {           // +1
                if (overrideSymbol.isKind(JavaSymbol.MTH)           // +2 (nesting = 1)
              && !overrideSymbol.isStatic()) {                      // +1
                  MethodJavaSymbol methodJavaSymbol = (MethodJavaSymbol)overrideSymbol;
                  if (canOverride(methodJavaSymbol)) {              // +3 (nesting = 2)
                    Boolean overriding = checkOverridingParameters(methodJavaSymbol,
                classType);
                    if (overriding == null) {                       // +4 (nesting = 3)
                      if (!unknownFound) {                          // +5 (nesting = 4)
                        unknownFound = true;
                      }
                    } else if (overriding) {                        // +1
                      return methodJavaSymbol;
                    }
                  }
                }
              }
              if (unknownFound) {                                   // +1
                return Symbols.unknownMethodSymbol;
              }
              return null;
            }                                     // total complexity = 19"""))

    def test_add_version(self):
        self.assertEqual([35], java("""
            private void addVersion(final Entry entry, final Transaction txn)
             throws PersistitInterruptedException, RollbackException {
              final TransactionIndex ti = _persistit.getTransactionIndex();
              while (true) {                                        // +1
                try {
                  synchronized (this) {
                    if (frst != null) {                             // +2 (nesting = 1)
                      if (frst.getVersion() > entry.getVersion()) { // +3 (nesting = 2)
                        throw new RollbackException();
                      }
                      if (txn.isActive()) {                         // +3 (nesting = 2)
                        for                                         // +4 (nesting = 3)
                            (Entry e = frst; e != null; e = e.getPrevious()) {
                          final long version = e.getVersion();
                          final long depends = ti.wwDependency(version,
                txn.getTransactionStatus(), 0);
                          if (depends == TIMED_OUT) {               // +5 (nesting = 4)
                            throw new WWRetryException(version);
                          }
                          if (depends != 0                          // +5 (nesting = 4)
               && depends != ABORTED) {                             // +1
                            throw new RollbackException();
                          }
                        }
                      }
                    }
                    entry.setPrevious(frst);
                    frst = entry;
                    break;
                  }
                } catch (final WWRetryException re) {               // +2 (nesting = 1)
                  try {
                    final long depends = _persistit.getTransactionIndex()
              .wwDependency(re.getVersionHandle(),txn.getTransactionStatus(),
              SharedResource.DEFAULT_MAX_WAIT_TIME);
                    if (depends != 0                                // +3 (nesting = 2)
              && depends != ABORTED) {                              // +1
                      throw new RollbackException();
                    }
                  } catch (final InterruptedException ie) {         // +3 (nesting = 2)
                    throw new PersistitInterruptedException(ie);
                  }
                } catch (final InterruptedException ie) {           // +2 (nesting = 1)
                  throw new PersistitInterruptedException(ie);
                }
              }
            }                                    // total complexity = 35"""))

    def test_to_regexp(self):
        self.assertEqual([20], java(r"""
            private static String toRegexp(String antPattern,
             String directorySeparator) {
              final String escapedDirectorySeparator = '\\' + directorySeparator;
              final StringBuilder sb = new StringBuilder(antPattern.length());
              sb.append('^');
              int i = antPattern.startsWith("/") ||                 // +1
             antPattern.startsWith("\\") ? 1 : 0;                   // +1
              while (i < antPattern.length()) {                     // +1
                final char ch = antPattern.charAt(i);
                if (SPECIAL_CHARS.indexOf(ch) != -1) {              // +2 (nesting = 1)
                  sb.append('\\').append(ch);
                } else if (ch == '*') {                             // +1
                  if (i + 1 < antPattern.length()                   // +3 (nesting = 2)
              && antPattern.charAt(i + 1) == '*') {                 // +1
                    if (i + 2 < antPattern.length()                 // +4 (nesting = 3)
              && isSlash(antPattern.charAt(i + 2))) {               // +1
                      sb.append("(?:.*")
              .append(escapedDirectorySeparator).append("|)");
                      i += 2;
                    } else {                                        // +1
                      sb.append(".*");
                      i += 1;
                    }
                  } else {                                          // +1
                    sb.append("[^").append(escapedDirectorySeparator).append("]*?");
                  }
                } else if (ch == '?') {                             // +1
                  sb.append("[^").append(escapedDirectorySeparator).append("]");
                } else if (isSlash(ch)) {                           // +1
                  sb.append(escapedDirectorySeparator);
                } else {                                            // +1
                  sb.append(ch);
                }
                i++;
              }
              sb.append('$');
              return sb.toString();
            }                                    // total complexity = 20"""))


class TestJavaSpecifics(unittest.TestCase):

    def test_labeled_break_and_continue_count_plain_ones_do_not(self):
        self.assertEqual([4], java("""
            void f() {
              outer: for (;;) {        // +1
                for (;;) {             // +2
                  break outer;         // +1
                  break;
                }
              }
            }"""))

    def test_generic_wildcard_is_not_a_ternary(self):
        self.assertEqual([0], java("void f() { List<? extends Foo> x = null; Map<String, ?> y; }"))

    def test_switch_arrow_case_is_not_a_lambda(self):
        self.assertEqual([3], java("""
            void f(int x) {
              switch (x) {             // +1
                case 1 -> { if (a) y(); }   // +2 (nesting=1)
                default -> z();
              }
            }"""))


class TestPython(unittest.TestCase):

    def test_if_elif_else_and_nesting(self):
        self.assertEqual([9], py("""
def f(a, b):
    if a and b:          # +1 +1
        for x in y:      # +2 (nesting=1)
            if x:        # +3 (nesting=2)
                pass
            elif z:      # +1
                pass
            else:        # +1
                pass
"""))

    def test_sequential_blocks_are_not_nested(self):
        self.assertEqual([2], py("""
def f(a):
    if a:
        pass
    while a:
        pass
"""))

    def test_conditional_expression(self):
        self.assertEqual([1], py("def f(a):\n    return 1 if a else 2\n"))
        self.assertEqual([3], py("def f(a):\n    return 1 if a else 2 if b else 3\n"))
        self.assertEqual([1], py("def f(a):\n    return g(1 if a else 2)\n"))

    def test_comprehension_filter_is_not_a_condition(self):
        self.assertEqual([0], py("def f(xs):\n    return [x for x in xs if x]\n"))
        self.assertEqual([0], py("def f(xs):\n    return {k: v for k, v in xs.items() if v}\n"))
        self.assertEqual([1], py("def f(xs):\n    return [x if x else 0 for x in xs]\n"))

    def test_try_except_finally(self):
        self.assertEqual([3], py("""
def f():
    try:
        pass
    except ValueError:   # +1
        if a:            # +2 (nesting=1)
            pass
    finally:
        pass
"""))

    def test_boolean_operator_sequences(self):
        self.assertEqual([3], py("def f():\n    return a and b or c and d\n"))
        self.assertEqual([3], py("def f():\n    if a and (b or c):\n        pass\n"))
        self.assertEqual([0], py("def f():\n    return not a\n"))

    def test_match_statement_counts_once(self):
        self.assertEqual([3], py("""
def m(x):
    match x:             # +1
        case 1:
            if y:        # +2 (nesting=1)
                pass
        case _:
            pass
"""))

    def test_match_as_a_variable_name_does_not_count(self):
        self.assertEqual([0], py("def m(x):\n    match = x\n    return match\n"))

    def test_recursion(self):
        self.assertEqual([2], py("def g(n):\n    return 1 if n <= 1 else n * g(n - 1)\n"))
        self.assertEqual([1], py("class A:\n    def g(self):\n        return self.g()\n"))
        self.assertEqual([0], py("class A:\n    def g(self):\n        return other.g()\n"))

    def test_lambda_nests(self):
        self.assertEqual([2], py("def f():\n    return lambda x: 1 if x else 0\n"))

    def test_nested_function_is_measured_on_its_own(self):
        self.assertEqual([1, 1], py("""
def outer(a):
    if a:                # +1 (outer)
        def inner(b):
            if b:        # +1 (inner: nesting starts at 0)
                pass
        return inner
"""))

    def test_line_continuation_does_not_start_a_statement(self):
        self.assertEqual([2], py("""
def f(a):
    if a and \\
            b:
        pass
"""))

    def test_multi_line_condition_in_parentheses(self):
        self.assertEqual([2], py("""
def f(a):
    if (a and
            b and
            c):
        pass
"""))


class TestOtherBraceLanguages(unittest.TestCase):

    def test_javascript(self):
        self.assertEqual([7], cogc("a.js", """
            function save(options, callback) {
              if (typeof options === 'function') {     // +1
                callback = options;
              }
              options || (options = {});               // +1
              label: for (var i = 0; i < 3; i++) {     // +1
                if (i) continue label;                 // +2 +1
              }
              return self.isNew() ? 'create' : 'update';  // +1
            }"""))

    def test_javascript_optional_chaining_and_nullish_coalescing(self):
        self.assertEqual([0], cogc("a.js", "function f(a) { return a?.b ?? c; }"))

    def test_go(self):
        self.assertEqual([9], cogc("a.go", """
            func f(x int) int {
                if x > 0 && y {          // +1 +1
                    for i := 0; i < x; i++ {   // +2
                        switch i {             // +3
                        case 1:
                            break
                        }
                    }
                } else if z {            // +1
                } else {                 // +1
                }
                return 0
            }"""))

    def test_csharp_nullable_types_are_not_ternaries(self):
        self.assertEqual([1], cogc("a.cs", """
            class A {
              int F(int? x) {
                int? y = null;
                return x ?? (y ?? 0) > 0 ? 1 : 0;    // +1
              }
            }"""))

    def test_kotlin_when_and_elvis(self):
        self.assertEqual([4], cogc("a.kt", """
            fun f(x: Int?): Int {
                val y = x ?: 0
                when (y) {                     // +1
                    1 -> if (a) b() else c()   // +2 (nesting=1) +1
                    else -> d()
                }
                return y
            }"""))

    def test_rust(self):
        self.assertEqual([6], cogc("a.rs", """
            fn f(x: Option<i32>) -> i32 {
                if let Some(v) = x {        // +1
                    match v {               // +2 (nesting=1)
                        1 => 1,
                        _ => 0,
                    }
                } else {                    // +1
                    loop { break; }         // +2 (nesting=1)
                }
            }"""))


class TestFlatLanguages(unittest.TestCase):
    """Blocks are not brace-delimited: increments without nesting penalty."""

    def test_ruby(self):
        self.assertEqual([3], cogc("a.rb", """
def f(a)
  if a && b
    x
  elsif c
    y
  end
end
"""))

    def test_lua_else_if_is_one_increment(self):
        self.assertEqual([2], cogc("a.lua", """
function f(a)
  if a then
    x()
  elseif b then
    y()
  end
end
"""))


class TestProfilesBuiltFromTheReaders(unittest.TestCase):
    """The control-flow keywords come from the language readers; a profile
    only adds what cyclomatic complexity does not count."""

    def test_perl_keywords_come_from_its_reader(self):
        # unless, until, if and elsif are all listed by PerlReader.
        self.assertEqual([4], cogc("a.pl", """
sub f { unless ($a) { x(); } until ($b) { y(); } if ($c) { } elsif ($d) { } }
"""))

    def test_php_elseif_foreach_and_match_come_from_its_reader(self):
        self.assertEqual([4], cogc("a.php", """<?php
function f($a) { if ($a) { } elseif ($b) { } foreach ($x as $y) { } $z = match($a) { 1 => 2 }; }
"""))

    def test_ruby_unless_is_added_to_what_the_reader_lists(self):
        self.assertEqual([1], cogc("a.rb", """
def f(a)
  unless a
    x
  end
end
"""))

    def test_lua_until_closes_repeat_and_is_not_counted(self):
        self.assertEqual([1], cogc("a.lua", """
function f(x)
  repeat x = x + 1 until x > 3
end
"""))

    def test_swift_guard_from_the_reader_and_repeat_from_the_profile(self):
        self.assertEqual([2], cogc("a.swift", """
func f() {
  guard let x = y else { return }
  repeat { } while a
}
"""))

    def test_go_select_and_goto_are_added_by_the_profile(self):
        self.assertEqual([3], cogc("a.go", """
package main
func f() {
  for a { }      // +1
  select { }     // +1
  goto L         // +1
L:
}
"""))


class TestBackwardCompatibility(unittest.TestCase):

    source = """
        int f(T&& x) {
          if (a && b) { g(); }
          for (auto&& e : v) { }
          switch (x) { case 1: break; case 2: break; }
          return a ? b : c;
        }
        void h() { auto l = [](int a) { return a; }; }
        """

    def test_other_metrics_are_unchanged_by_the_extension(self):
        plain = analyze_file.analyze_source_code("a.cpp", self.source)
        with_ext = analyze("a.cpp", self.source)
        for before, after in zip(plain.function_list, with_ext.function_list):
            for attr in ("name", "long_name", "cyclomatic_complexity", "nloc",
                         "token_count", "parameter_count", "start_line",
                         "end_line", "max_nesting_depth"):
                self.assertEqual(getattr(before, attr), getattr(after, attr), attr)
        self.assertEqual(plain.nloc, with_ext.nloc)
        self.assertEqual(plain.token_count, with_ext.token_count)

    def test_tokens_pass_through_untouched(self):
        class Recorder(object):
            def __init__(self):
                self.tokens = []

            def __call__(self, tokens, reader):
                for token in tokens:
                    self.tokens.append(token)
                    yield token

        plain, recorded = Recorder(), Recorder()
        FileAnalyzer(get_extensions([plain])).analyze_source_code("a.cpp", self.source)
        FileAnalyzer(get_extensions([Cognitive(), recorded])).analyze_source_code(
            "a.cpp", self.source)
        self.assertEqual(plain.tokens, recorded.tokens)

    def test_every_function_has_the_field(self):
        result = analyze("a.cpp", "void f(); void g() {}")
        self.assertEqual([0], [f.cognitive_complexity for f in result.function_list])


class TestFunctionsTheExtensionNeverSees(unittest.TestCase):
    """A body-less Perl ``sub fwd;`` is created and finished by the reader
    within one step, between two tokens, so the extension is never handed a
    token of it.  It still needs the field."""

    test_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    fixture = os.path.join(test_dir, "test_languages", "testdata",
                           "perl_oneliners.pl")

    def test_bodyless_sub_has_zero_cognitive_complexity(self):
        result = FileAnalyzer(get_extensions([Cognitive()]))(self.fixture)
        by_name = dict((f.name, f.cognitive_complexity)
                       for f in result.function_list)
        self.assertEqual(6, len(by_name))
        self.assertEqual(0, by_name['OneLinerTest::empty_oneliner'])
        self.assertEqual(0, by_name['OneLinerTest::simple_oneliner'])
        self.assertEqual(1, by_name['OneLinerTest::condition_oneliner'])  # ?:

    def test_lizard_py_run_as_a_script(self):
        """``python lizard.py -Ecognitive`` loads lizard twice, as ``__main__``
        and as ``lizard``; the functions are instances of the former."""
        lizard_dir = os.path.dirname(self.test_dir)
        result = subprocess.run(
            [sys.executable, os.path.join(lizard_dir, 'lizard.py'),
             '-Ecognitive', self.fixture],
            capture_output=True, text=True, cwd=lizard_dir)
        self.assertEqual(0, result.returncode, msg=result.stderr or result.stdout)
        self.assertRegex(result.stdout, r"\s0\s+OneLinerTest::empty_oneliner@")
        self.assertRegex(result.stdout, r"\s1\s+OneLinerTest::condition_oneliner@")


class TestCommandLine(unittest.TestCase):

    def test_default_threshold(self):
        options = parse_args(['lizard', '-Ecognitive'])
        self.assertEqual(15, options.thresholds['cognitive_complexity'])

    def test_threshold_option(self):
        options = parse_args(['lizard', '-Ecognitive', '--CogC', '25'])
        self.assertEqual(25, options.thresholds['cognitive_complexity'])

    def test_generic_threshold_option_wins(self):
        options = parse_args(['lizard', '-Ecognitive', '-Tcognitive_complexity=3'])
        self.assertEqual(3, options.thresholds['cognitive_complexity'])

    def test_sorting_by_cognitive_complexity(self):
        options = parse_args(['lizard', '-Ecognitive', '-s', 'cognitive_complexity'])
        self.assertEqual(['cognitive_complexity'], options.sorting)

    def test_no_threshold_without_the_extension(self):
        options = parse_args(['lizard'])
        self.assertNotIn('cognitive_complexity', options.thresholds)

    @patch('lizard.auto_read', create=True)
    @patch('lizard.md5_hash_file')
    @patch('os.walk')
    @patch('sys.stdout')
    def test_output_has_a_cogc_column(self, stdout, os_walk, _, auto_read):
        os_walk.return_value = [('.', [], ['a.cpp'])]
        auto_read.return_value = "void foo() { if (a) { if (b) {} } }"
        lizard.main(['lizard', '-Ecognitive'])
        output = ''.join(call.args[0] for call in stdout.write.call_args_list)
        self.assertIn("CogC", output)
        self.assertRegex(output, r"\s1\s+3\s+")   # CCN column is followed by CogC

    @patch('lizard.auto_read', create=True)
    @patch('lizard.md5_hash_file')
    @patch('os.walk')
    @patch('sys.stdout')
    def test_warning_when_threshold_exceeded(self, stdout, os_walk, _, auto_read):
        os_walk.return_value = [('.', [], ['a.cpp'])]
        auto_read.return_value = "void foo() { if (a) { if (b) {} } }"
        self.assertEqual(1, lizard.main(['lizard', '-Ecognitive', '--CogC', '2']))
        self.assertEqual(0, lizard.main(['lizard', '-Ecognitive', '--CogC', '3']))


if __name__ == '__main__':
    unittest.main()
