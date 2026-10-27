import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardns import LizardExtension as NestedStructure
from .testHelpers import get_python_function_list_with_extension


def process_python(source):
    return get_python_function_list_with_extension(source, NestedStructure())


class TestPythonNestedStructures(unittest.TestCase):

    def test_no_structures(self):
        result = process_python("def fun():\n pass")
        self.assertEqual(0, result[0].max_nested_structures)

    def test_if_structure(self):
        result = process_python("def fun():\n if a:\n  return")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_for_structure(self):
        result = process_python("def fun():\n for a in b:\n  foo()")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_condition_in_if_structure(self):
        result = process_python("def fun():\n if a and b:\n  return")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_elif(self):
        result = process_python("""
        def c():
          if a:
            baz()
          elif c:
            foo()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_nested_if_structures(self):
        result = process_python("""
        def c():
          if a:
            if b:
              baz()
          else:
            foo()
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_equal_metric_structures(self):
        result = process_python("""
        def c():
          if a:
            if b:
              baz()
          else:
            foo()

          for a in b:
            if c:
              bar()
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_while(self):
        result = process_python("""
        def c():
          while a:
            baz()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_try_except(self):
        result = process_python("""
        def c():
          try:
            f.open()
          except Exception as err:
            print(err)
          finally:
            f.close()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_two_functions(self):
        result = process_python("""
        def c():
          try:
            if a:
              foo()
          except Exception as err:
            print(err)

        def d():
          for a in b:
            for x in y:
              if i:
                return j
        """)
        self.assertEqual(2, result[0].max_nested_structures)
        self.assertEqual(3, result[1].max_nested_structures)

    def test_nested_functions(self):
        result = process_python("""
        def c():
            def d():
                for a in b:
                    for x in y:
                        if i:
                            return j
            try:
                if a:
                    foo()
            except Exception as err:
                print(err)

        """)
        self.assertEqual(3, result[0].max_nested_structures)
        self.assertEqual(2, result[1].max_nested_structures)

    def test_ternary_and_comprehension_are_not_structures(self):
        result = process_python("""
        def c(items):
            chosen = [x for x in items if x]
            return chosen if chosen else None
        """)
        self.assertEqual(0, result[0].max_nested_structures)

    def test_structures_do_not_pile_up_from_one_function_to_the_next(self):
        result = process_python("""
        def a(x):
            if x:
                return 1
            return 0

        def b(x):
            if x:
                return 1
            return 0
        """)
        self.assertEqual([1, 1], [f.max_nested_structures for f in result])

    def test_match_is_a_structure_and_its_cases_are_not(self):
        result = process_python("""
        def c(code):
            match code:
                case 1:
                    return "one"
                case _:
                    if code:
                        return "other"
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_async_for_and_async_with(self):
        result = process_python("""
        async def c(source):
            async with source:
                async for item in source:
                    if item:
                        return item
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_structure_on_a_continuation_line_is_not_nested(self):
        result = process_python("""
        def c(items):
            total = sum(x
                        for x in items)
            if total:
                return total
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_structures_do_not_leak_between_files(self):
        analyzer = FileAnalyzer(get_extensions([NestedStructure()]))
        source = "def f(x):\n    if x:\n        return x\n"
        for filename in ('first.py', 'second.py'):
            result = analyzer.analyze_source_code(filename, source)
            self.assertEqual([1], [f.max_nested_structures
                                   for f in result.function_list])

    def test_nested_function_restores_outer_structures(self):
        result = process_python("""
        def outer(x):
            if x:
                def inner(y):
                    if y:
                        return y
                if x > 1:
                    return inner(x)
            return x
        """)
        self.assertEqual([('outer.inner', 1), ('outer', 2)],
                         [(f.name, f.max_nested_structures) for f in result])

    def test_match_variable_is_not_a_structure(self):
        result = process_python("""
        def use(match):
            match = 1
            match.group()
            match(1)
            match[0]
            if match:
                return match
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_with_structure(self):
        result = process_python("""
        def c():
            with open(f) as input_file:
                foo(f)
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_for_else(self):
        result = process_python("""
        def c():
            for i in range(10):
                break
            else:
                for j in range(i):
                    print(j)
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_while_else(self):
        result = process_python("""
        def c(i):
            while i < 10:
                break
            else:
                for j in range(i):
                    print(j)
        """)
        self.assertEqual(2, result[0].max_nested_structures)
