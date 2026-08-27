import unittest
from lizard import analyze_file


def get_python_function_list(source_code):
    return analyze_file.analyze_source_code("a.py", source_code).function_list


class Test_Python_type_parameters(unittest.TestCase):
    """Type parameter lists must not overwrite the function name.

    `def f[T](x)` was reported as a function named ']', because every token
    between `def` and `(` restarted the function.
    """

    def test_generic_function_keeps_its_name_and_long_name(self):
        code = (
            'def soma[T](a: T, b: T) -> T:\n'
            '    return a\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual('soma', functions[0].name)
        self.assertEqual('soma [ T ]( a : T , b : T )', functions[0].long_name)

    def test_generic_function_metrics_match_non_generic(self):
        generic = get_python_function_list(
            'def soma[T](a: T, b: T) -> T:\n'
            '    if a:\n'
            '        return a\n'
            '    return b\n'
        )[0]
        plain = get_python_function_list(
            'def soma(a, b):\n'
            '    if a:\n'
            '        return a\n'
            '    return b\n'
        )[0]
        self.assertEqual(plain.name, generic.name)
        self.assertEqual(plain.parameter_count, generic.parameter_count)
        self.assertEqual(plain.cyclomatic_complexity,
                         generic.cyclomatic_complexity)

    def test_multiple_type_parameters(self):
        code = (
            'def par[K, V](k: K, v: V) -> tuple[K, V]:\n'
            '    return (k, v)\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual('par', functions[0].name)
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual('par [ K , V ]( k : K , v : V )',
                         functions[0].long_name)

    def test_type_parameter_with_nested_bound(self):
        code = (
            'def f[T: list[int]](x: T) -> T:\n'
            '    return x\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual('f', functions[0].name)
        self.assertEqual('f [ T : list [ int ] ]( x : T )',
                         functions[0].long_name)

    def test_generic_method(self):
        code = (
            'class C:\n'
            '    def m[T](self, x: T) -> T:\n'
            '        return x\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual('m', functions[0].name)
        self.assertEqual('m [ T ]( self , x : T )', functions[0].long_name)

    def test_async_generic_function(self):
        code = (
            'async def busca[T](x: T) -> T:\n'
            '    return x\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual('busca', functions[0].name)
        self.assertEqual('busca [ T ]( x : T )', functions[0].long_name)

    def test_nested_generic_functions(self):
        code = (
            'def outer[T](x: T):\n'
            '    def inner[U](y: U):\n'
            '        return y\n'
            '    return inner\n'
        )
        functions = get_python_function_list(code)
        names = [f.name for f in functions]
        self.assertEqual(['outer.inner', 'outer'], names)
        by_name = {f.name: f.long_name for f in functions}
        self.assertEqual('outer [ T ]( x : T )', by_name['outer'])
        self.assertEqual('outer.inner [ U ]( y : U )', by_name['outer.inner'])

    def test_default_typevartuple_and_paramspec(self):
        samples = (
            ('def f[T = int](x: T) -> T:\n    return x\n',
             'f [ T = int ]( x : T )'),
            ('def f[*Ts](x):\n    return x\n',
             'f [ * Ts ]( x )'),
            ('def f[**P](x):\n    return x\n',
             'f [ ** P ]( x )'),
        )
        for code, long_name in samples:
            with self.subTest(code=code):
                functions = get_python_function_list(code)
                self.assertEqual('f', functions[0].name)
                self.assertEqual(long_name, functions[0].long_name)

    def test_constraints_and_nested_callable_bound(self):
        samples = (
            ('def f[T: (int, str)](x: T) -> T:\n    return x\n',
             'f [ T : ( int , str ) ]( x : T )'),
            ('def f[T: Callable[[int], str]](x: T) -> T:\n    return x\n',
             'f [ T : Callable [ [ int ] , str ] ]( x : T )'),
        )
        for code, long_name in samples:
            with self.subTest(code=code):
                functions = get_python_function_list(code)
                self.assertEqual('f', functions[0].name)
                self.assertEqual(1, functions[0].parameter_count)
                self.assertEqual(long_name, functions[0].long_name)

    def test_multiline_type_parameter_list(self):
        code = (
            'def f[\n'
            '    T,\n'
            '    U,\n'
            '](x: T, y: U) -> T:\n'
            '    return x\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual('f', functions[0].name)
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual('f [ T , U , ]( x : T , y : U )',
                         functions[0].long_name)

    def test_string_literal_containing_bracket_in_bound(self):
        samples = (
            ('def f[T: "]"](x: T):\n    return x\n',
             'f [ T : "]" ]( x : T )'),
            ('def f[T: f"["](x: T):\n    return x\n',
             'f [ T : f "[" ]( x : T )'),
        )
        for code, long_name in samples:
            with self.subTest(code=code):
                functions = get_python_function_list(code)
                self.assertEqual('f', functions[0].name)
                self.assertEqual(long_name, functions[0].long_name)

    def test_unclosed_type_parameter_list_recovers_at_next_def(self):
        """An unbalanced '[' must not swallow the rest of the file."""
        code = (
            'def f[T\n'
            '\n'
            'def g(x):\n'
            '    return x\n'
            '\n'
            'def h(y):\n'
            '    return y\n'
        )
        names = [f.name for f in get_python_function_list(code)]
        self.assertEqual(['g', 'h'], names)

    def test_unclosed_type_parameter_list_recovers_at_class(self):
        code = (
            'def f[T\n'
            '\n'
            'class C:\n'
            '    def m(self):\n'
            '        return 1\n'
        )
        names = [f.name for f in get_python_function_list(code)]
        self.assertEqual(['m'], names)

    def test_missing_function_name_keeps_previous_function(self):
        """`def [](x)` is invalid syntax and must not break the file."""
        code = (
            'def boa(z):\n'
            '    return z\n'
            '\n'
            'def [](x):\n'
            '    return x\n'
        )
        names = [f.name for f in get_python_function_list(code)]
        self.assertEqual(['boa'], names)
