import unittest

from .testTypeScript import get_ts_function_list


class Test_ts_param_type_filtering(unittest.TestCase):
    """Type keywords should be filtered from parameter counts."""

    def test_simple_typed_params(self):
        """fn(x: string, y: number) should have 2 params"""
        code = 'function fn(x: string, y: number, z: boolean) {}'
        functions = get_ts_function_list(code)
        self.assertEqual(3, functions[0].parameter_count)

    def test_generic_type_in_params(self):
        """fn(x: Map<string, number>, y: boolean) — generic comma not counted"""
        code = 'function fn(x: Map<string, number>, y: boolean) {}'
        functions = get_ts_function_list(code)
        self.assertEqual(2, functions[0].parameter_count)

    def test_nested_generic_in_params(self):
        """fn(x: Promise<Array<string>>, y: number) — nested generics"""
        code = 'function fn(x: Promise<Array<string>>, y: number) {}'
        functions = get_ts_function_list(code)
        self.assertEqual(2, functions[0].parameter_count)

    def test_multiple_generic_params(self):
        """fn(a: Map<K, V>, b: Set<T>, c: number) — multiple generics"""
        code = 'function fn(a: Map<K, V>, b: Set<T>, c: number) {}'
        functions = get_ts_function_list(code)
        self.assertEqual(3, functions[0].parameter_count)

    def test_parameter_count_with_union_return_type_and_following_function(self):
        """Issue #476: union return type must not swallow the body opening brace."""
        code = '''
        export function function1 (
          parameter1: Param1,
          parameter2: Param2,
          parameter3: Param3,
          parameter4: Param4
        ): something | undefined {
          const newVariable = parameter1.newExample?.trim();
          if (!newVariable) {
            return undefined;
          }

          return something;
        }

        function function2(param1: Param1, param2: Param2): boolean {
          return anotherFunction(param1.thing, param2.anotherThing);
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual('function1', functions[0].name)
        self.assertEqual(4, functions[0].parameter_count)



class Test_ts_function_end_after_literals(unittest.TestCase):
    # https://github.com/terryyin/lizard/issues/497

    def spans(self, code):
        return [(f.name, f.start_line, f.end_line)
                for f in get_ts_function_list(code)]

    def test_regex_literal_as_call_argument(self):
        code = (
            "function a(s: string) {\n"
            "  const m = s.match(/x/);\n"
            "  return m;\n"
            "}\n"
            "function b() { return 1; }\n"
            "function c() { return 2; }\n"
        )
        self.assertEqual([('a', 1, 4), ('b', 5, 5), ('c', 6, 6)],
                         self.spans(code))

    def test_backtick_in_string_inside_template_expression(self):
        code = (
            "function a(k: string, e: boolean) {\n"
            "  return `${k}${e ? \" and `x` is empty\" : \"\"}`;\n"
            "}\n"
            "function b() { return 1; }\n"
            "function c() { return 2; }\n"
        )
        self.assertEqual([('a', 1, 3), ('b', 4, 4), ('c', 5, 5)],
                         self.spans(code))
