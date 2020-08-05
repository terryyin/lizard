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
