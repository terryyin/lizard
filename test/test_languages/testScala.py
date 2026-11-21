__author__ = 'David Baum'

import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_scala_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.scala", source_code).function_list


class TestScala(unittest.TestCase):
    def test_empty(self):
        functions = get_scala_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_scala_function_list('''
            for {
                i <- 0 to 10
                if i % 2 == 0
                if i > 5
            }
                ''')
        self.assertEqual(0, len(result))
    def test_function(self):
        result = get_scala_function_list('''
            def foo() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_func_with_one_parameter(self):
        result = get_scala_function_list('''
            def main(args: Array[String]) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("main", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_func_with_two_parameters(self):
        result = get_scala_function_list('''
            def foo(array: Array[String], name: String) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_complexity(self):
        result = get_scala_function_list('''
            def foo(query: String): Bool = {
                if(query.equalsIgnoreCase('foo')) {
                    println(true)
                } else {
                    println(false)
                }

                for {
                    i <- 0 to 10
                    if i % 2 == 0
                    if i > 5
                }
            }''')
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(5, result[0].cyclomatic_complexity)

    def test_comments(self):
        result = get_scala_function_list('''
            object HelloWorld {
               /* This is my first java program.
                * This will print 'Hello World' as the output
                * This is an example of multi-line comments.
                */
               def main(args: Array[String]) {
                  // Prints Hello World
                  // This is also an example of single line comment.
                  var a = 1
                  var b = 1
                  if(a == b) {
                      println("Hello, world!")
                  }
               }
            }
            ''')
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].parameter_count)

    def test_field(self):
        result = get_scala_function_list('''
            class A {
                def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
                def list(): Future[Seq[Person]] = db.run {
                  people.result
                }
            }
            ''')
        self.assertEqual(1, len(result))
        self.assertEqual('list', result[0].name)

    def test_oneliner(self):
        result = get_scala_function_list('''
                def list(): Future[Seq[Person]] = a && b
                def list1(): Future[Seq[Person]] = a && b
            ''')
        self.assertEqual(2, len(result))
        self.assertEqual('list', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_oneliner_with_braces(self):
        result = get_scala_function_list('''
                def list(): Future[Seq[Person]] = a + {
                a && b}
                def list(): Future[Seq[Person]] = a + {
                a && b}
            ''')
        self.assertEqual(2, len(result))
        self.assertEqual('list', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nested(self):
        result = get_scala_function_list('''
                def list1(): Future[Seq[Person]] = a + {
                    def list2(): Future[Seq[Person]] = a + b
                }
            ''')
        self.assertEqual(2, len(result))
        self.assertEqual('list2', result[0].name)

    def test_generic_function_with_type_param(self):
        result = get_scala_function_list('''
            def identity[T](x: T): T = { x }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("identity", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_generic_function_with_multiple_type_params(self):
        result = get_scala_function_list('''
            def merge[A, B](a: A, b: B): String = { a.toString + b.toString }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("merge", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_generic_method_in_class_not_dropped(self):
        result = get_scala_function_list('''
            class Box {
                def plain(x: Int): Int = { x + 1 }
                def generic[T](x: T): T = { x }
                def after(y: Int): Int = { y * 2 }
            }
                ''')
        names = sorted(f.name for f in result)
        self.assertEqual(["after", "generic", "plain"], names)

    def test_brace_body_ends_at_its_closing_brace(self):
        result = get_scala_function_list('''
            object Pricing {
              def discount(order: Order): Int = {
                if (order.isLarge) 10 else 0
              } // no discount for small orders

              val defaultRate = if (enabled) 5 else 0

              def total(order: Order): Int = {
                order.amount
              }
            }
            ''')
        self.assertEqual([(3, 5, 3, 21), (9, 11, 3, 15)],
                         [(f.start_line, f.end_line, f.nloc, f.token_count)
                          for f in result])

    def test_next_line_case_does_not_count_for_the_function_above(self):
        result = get_scala_function_list('''
            object Shapes {
              def area(s: Shape): Double = s.width * s.height
              case class Circle(radius: Double)
              def kind(s: Shape): String = s match {
                case _: Circle => "circle"
                case _ => "other"
              }
              case object Empty
            }
            ''')
        self.assertEqual([("area", 3, 1), ("kind", 8, 3)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_expression_body_on_the_next_line(self):
        result = get_scala_function_list('''
            class Report {
              def title(r: Row): String =
                if (r.isEmpty) "none" else if (r.isShort) r.name else "long"
              def size(r: Row): Int = r.count
            }
            ''')
        self.assertEqual([("title", 4, 3), ("size", 5, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_expression_body_with_arguments_on_later_lines(self):
        result = get_scala_function_list('''
            object Defaults {
              def config(env: Env): Config = Config(
                env.name,
                if (env.debug) 1 else 0)
              def id(env: Env): Int = env.id
            }
            ''')
        self.assertEqual([("config", 5, 2), ("id", 6, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_expression_body_ends_at_a_closing_brace_on_its_line(self):
        result = get_scala_function_list('''
            class Worker {
              val task = new Runnable { def run(): Unit = execute() }
              def stop(w: Worker): Unit = {
                if (w.running) w.halt()
              }
            }
            ''')
        self.assertEqual([("run", 3, 3), ("stop", 4, 6)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_expression_body_that_continues_on_the_next_line(self):
        result = get_scala_function_list('''
            object Access {
              def allowed(u: User): Boolean =
                u.isAdmin ||
                  u.roles.contains("editor")
              def label(u: User): String = if (u.isAdmin) {
                "admin"
              }
              else {
                "user"
              }
              def names(us: Seq[User]): Seq[String] = us
                .filter(_.active)
                .map(_.name)
              def load(path: String): String = try {
                read(path)
              }
              catch {
                case e: IOException => ""
              }
            }
            ''')
        self.assertEqual([("allowed", 5, 2), ("label", 11, 2),
                          ("names", 14, 1), ("load", 20, 3)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_hash_code_call_in_expression_body(self):
        result = get_scala_function_list('''
            class Key(name: String) {
              def hash(k: Key): Int = improve(k.##)
              def same(k: Key): Boolean = k.name == name
            }
            ''')
        self.assertEqual([("hash", 3), ("same", 4)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_with_a_member_without_parameters(self):
        result = get_scala_function_list('''
            object Flags {
              def on(): Flag =
                new Flag { override def value = true }
            }
            ''')
        self.assertEqual([("on", 3, 4)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_expression_body_after_a_condition_on_its_own_line(self):
        result = get_scala_function_list('''
            object Pick {
              def pick(a: Boolean, b: Boolean): Int = if (a)
                if (b) 1 else 2
              else 3
              def none(): Int = 0
            }
            ''')
        self.assertEqual([("pick", 5, 3), ("none", 6, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_body_ends_at_a_semicolon(self):
        result = get_scala_function_list('''
            object Values {
              def one(): Int = { 1 }; val a = if (ok) 2 else 3
              def two(): Int = 2; val b = if (ok) 4 else 5
            }
            ''')
        self.assertEqual([("one", 3, 1), ("two", 4, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_expression_body_after_a_default_in_a_second_parameter_list(self):
        result = get_scala_function_list('''
            class Ref {
              def send(m: Any)(implicit s: Ref = Ref.none): Unit = ()

              def stop(): Unit = ()
            }
            ''')
        self.assertEqual([("send", 3), ("stop", 5)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_that_continues_after_else(self):
        result = get_scala_function_list('''
            object Pick {
              def pick(a: Boolean): Int = if (a) 1 else
                if (a) 2 else 3
              def none(): Int = 0
            }
            ''')
        self.assertEqual([("pick", 4, 3), ("none", 5, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_enclosing_function_ends_at_a_brace_after_an_inner_body(self):
        result = get_scala_function_list('''
            object Outer {
              def outer(): Unit = {
                def inner(): Int =
                  1 }
            }
            ''')
        self.assertEqual([("inner", 4, 5), ("outer", 3, 5)],
                         [(f.name, f.start_line, f.end_line)
                          for f in result])

    def test_member_without_parameters_counts_once(self):
        result = get_scala_function_list('''
            object Flags {
              def on(): AnyRef =
                new AnyRef { def value = true }
            }
            ''')
        self.assertEqual([("on", 2, 15)],
                         [(f.name, f.nloc, f.token_count) for f in result])

    def test_expression_body_with_a_do_while_loop(self):
        result = get_scala_function_list('''
            object Ticker {
              def repeat(again: Boolean): Unit = do { tick() } while (again)
              def once(again: Boolean): Unit = do tick() while (again)
              def after(): Int = 1
            }
            ''')
        self.assertEqual([("repeat", 3), ("once", 4), ("after", 5)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_with_a_do_while_loop_after_a_condition(self):
        result = get_scala_function_list('''
            object Ticker {
              def maybe(ok: Boolean): Unit = if (ok) do tick() while (ok)
              def loop(n: Int): Unit = while (n > 0) do step()
              def after(): Int = 1
            }
            ''')
        self.assertEqual([("maybe", 3), ("loop", 4), ("after", 5)],
                         [(f.name, f.end_line) for f in result])

    def test_function_with_a_backtick_name(self):
        result = get_scala_function_list('''
            object Matcher {
              def outer(): Boolean = {
                def `match`(x: Int): Boolean = x > 0
                `match`(1)
              }
              def `set asScala`(s: Set[Int]): Int = s.size
            }
            ''')
        self.assertEqual([("match", 4, 4), ("outer", 3, 6), ("set", 7, 7)],
                         [(f.name, f.start_line, f.end_line)
                          for f in result])

    def test_expression_body_that_continues_after_a_comparison(self):
        result = get_scala_function_list('''
            object Compare {
              def le(a: Int, b: Int): Boolean = a <=
                b
              def after(): Int = 0
            }
            ''')
        self.assertEqual([("le", 4), ("after", 5)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_after_a_nested_condition(self):
        result = get_scala_function_list('''
            object Pick {
              def pick(a: Boolean): Int = if (if (a) true else false)
                1
              else 0
              def after(): Int = 0
            }
            ''')
        self.assertEqual([("pick", 5), ("after", 6)],
                         [(f.name, f.end_line) for f in result])

    def test_closing_brace_after_an_inner_body_counts_once(self):
        result = get_scala_function_list('''
            object Outer {
              def outer(): Unit = {
                def inner(): Int =
                  1 }
            }
            ''')
        self.assertEqual([("inner", 8), ("outer", 10)],
                         [(f.name, f.token_count) for f in result])

    def test_expression_body_with_an_xml_literal(self):
        result = get_scala_function_list('''
            object Page {
              def row(kv: (String, String)) = <tr><td>{kv._1}</td></tr>
              def cell(v: String) = <td>{v}</td>
            }
            ''')
        self.assertEqual([("row", 3), ("cell", 4)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_that_ends_with_a_symbolic_member(self):
        result = get_scala_function_list('''
            object Ops {
              def bind(counter: Counter): Int => Int = counter.+
              def next(): Int = 2
            }
            ''')
        self.assertEqual([("bind", 3), ("next", 4)],
                         [(f.name, f.end_line) for f in result])

    def test_expression_body_with_a_block_on_the_next_line(self):
        result = get_scala_function_list('''
            object Ticker {
              def work(ok: Boolean): Unit = repeat
              {
                if (ok) println("tick")
              }
              def next(): Int = 1
            }
            ''')
        self.assertEqual([("work", 6, 2), ("next", 7, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_expression_body_with_a_loop_that_continues_on_the_next_line(self):
        result = get_scala_function_list('''
            object Ticker {
              def repeat(ok: Boolean): Unit = do { tick() }
                while (ok)
              def each(ok: Boolean): Unit = for { i <- 1 to 2 }
                if (ok) println(i)
              def next(): Int = 1
            }
            ''')
        self.assertEqual([("repeat", 4), ("each", 6), ("next", 7)],
                         [(f.name, f.end_line) for f in result])

    def test_block_after_a_blank_line_does_not_continue_the_body(self):
        result = get_scala_function_list('''
            object Journal {
              def send(ok: Boolean): Unit = {
                if (ok) println("ok")
              }

              {
                if (ready) println("init")
              }
              def next(): Int = 1
            }
            ''')
        self.assertEqual([("send", 5, 2), ("next", 10, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_function_after_a_comment_that_ends_on_its_line(self):
        result = get_scala_function_list('''
            object C {
              def first(): Int = 1 /* note
              */ def next(): Int = 2
            }
            ''')
        self.assertEqual([("first", 3), ("next", 4)],
                         [(f.name, f.start_line) for f in result])

    def test_block_after_a_brace_body_does_not_continue_it(self):
        result = get_scala_function_list('''
            object C {
              def first(): Int = { 1 }
              { if (ready) println("init") }
              def next(): Int = 2
            }
            ''')
        self.assertEqual([("first", 3, 1), ("next", 5, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_do_while_loop_with_a_semicolon(self):
        result = get_scala_function_list('''
            object C {
              def repeat(ok: Boolean): Unit = do (); while (ok)
              def next(): Int = 2
            }
            ''')
        self.assertEqual([("repeat", 3, 3), ("next", 4, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_block_on_the_next_line_after_a_comment_line(self):
        result = get_scala_function_list('''
            object Ticker {
              def work(ok: Boolean): Unit = repeat
              // keep the block below
              {
                if (ok) println("tick")
              }
              def next(): Int = 1
            }
            ''')
        self.assertEqual([("work", 7, 2), ("next", 8, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])

    def test_definition_text_in_a_multi_line_string(self):
        result = get_scala_function_list('''
            object C {
              def usage(): String = """ "def fake()"
                """
              def next(): Int =
                1
            }
            ''')
        self.assertEqual([("usage", 3, 4), ("next", 5, 6)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_call_of_a_function_with_the_backtick_name_def(self):
        result = get_scala_function_list('''
            object C {
              def `def`(): Int = 1
              def call(): Int = `def`()
              def next(): Int =
                2
            }
            ''')
        self.assertEqual([("def", 3, 3), ("call", 4, 4), ("next", 5, 6)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_expression_body_that_continues_after_a_shift_or_less_than(self):
        for operator in ('<<', '>>', '>>>', '<'):
            with self.subTest(operator=operator):
                result = get_scala_function_list('''
                    object C {
                      def first(x: Int): Any = x %s
                        1
                      def next(): Int = 2
                    }
                    ''' % operator)
                self.assertEqual(
                    [("first", 4, 2), ("next", 5, 1)],
                    [(f.name, f.end_line, f.nloc) for f in result])

    def test_statement_after_a_comment_that_ends_on_its_line(self):
        result = get_scala_function_list('''
            object C {
              def first(): Int =
                1 /* note
                */ println(if (ready) 2 else 3)
            }
            ''')
        self.assertEqual([("first", 4, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in result])
