import unittest
import tempfile
import shutil
from os import path
from lizard import analyze_file
from .testHelpers import get_cpp_fileinfo
from lizard_ext.lizardfans import LizardExtension as Fans


def get_cpp_fans_extension(source):
    return get_cpp_fileinfo(source)


def get_cpp_function_list_with_fans_extension(file_path, source_code):
    return analyze_file.analyze_source_code(file_path, source_code).function_list


class FakeReader(object):
    class FI(object):
        pass

    def __init__(self):
        self.fileinfo = self.FI()
        self.tokens = {}
        self.context = self
        self.name_list = []

    def get_fan_in_fan_out(self):
        return self.fileinfo.fan_in, self.fileinfo.fan_out

    def get_token(self):
        return self.tokens


class TestFans(unittest.TestCase):
    def setUp(self):
        self.reader = FakeReader()
        self.ext = Fans()
        self.test_dir = tempfile.mkdtemp()
        self.body = ['{\n', '\tif (a && b){\n', '\tfun(a);\n',
                     '\tif( a != 0 ){\n', '\n', '\ta = b;\n',
                     '\t c = a;\n', '\t}\n', '}\n']

    def tearDown(self):
        # Terminate the temporary after tests
        shutil.rmtree(self.test_dir)

    def test_tokens(self):
        self.tokens = list(self.ext(["a", "b"], self.reader))
        self.assertEqual(['a', 'b'], self.tokens)

    def test_fans_initiation(self):
        list(self.ext(["a", "b"], self.reader))
        self.assertEqual((0, 0), self.reader.get_fan_in_fan_out())

    def test_empty_body(self):
        result = get_cpp_function_list_with_fans_extension("a.cpp",
                                                           "int fun(a){})")
        body = self.ext.method_open(result[0])
        self.assertEqual([], body)

    def test_open_function_with_body(self):
        f = open(path.join(self.test_dir, 'a.cpp'), 'w+')
        f.writelines("""int c() {
    if (a && b) {
        if( a != 0 ){
            a = b;
        }
    }
}
        """)
        f = open(path.join(self.test_dir, 'a.cpp'))
        result = get_cpp_function_list_with_fans_extension(f.name, f.read())
        f.close()
        body = self.ext.method_open(result[0])
        self.assertEqual(body, ['    if (a && b) {\n',
                                '        if( a != 0 ){\n',
                                '            a = b;\n',
                                '        }\n',
                                '    }\n',
                                '}\n'])

    def test_fan_in_fan_out_only_result(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        self.ext.calculate_fan_in_fan_out(['fun', 'bar'],
                                          self.body, result,
                                          result.function_list[1])
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(0, result.function_list[0].fan_out)
        self.assertEqual(0, result.function_list[1].fan_in)
        self.assertEqual(1, result.function_list[1].fan_out)

    def test_single_dual_fans_result(self):
        result = get_cpp_fans_extension("""int fun(a){}""")
        self.body[6] = '\t bar(a)\n'
        self.ext.calculate_fan_in_fan_out(['fun'],
                                          self.body, result,
                                          result.function_list[0])
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(1, result.function_list[0].fan_out)

    def test_multi_body_method_result_part1(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        self.body[6] = '\t bar(a)\n'
        self.ext.calculate_fan_in_fan_out(['fun', 'bar'],
                                          self.body, result,
                                          result.function_list[0])
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(2, result.function_list[0].fan_out)
        self.assertEqual(1, result.function_list[1].fan_in)
        self.assertEqual(0, result.function_list[1].fan_out)

    def test_multi_body_method_result_part2(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        self.body[6] = '\t fun(a)\n'
        self.ext.calculate_fan_in_fan_out(['fun', 'bar'],
                                          self.body, result,
                                          result.function_list[1])
        self.assertEqual(2, result.function_list[0].fan_in)
        self.assertEqual(0, result.function_list[0].fan_out)
        self.assertEqual(0, result.function_list[1].fan_in)
        self.assertEqual(2, result.function_list[1].fan_out)

    def test_multi_functions_with_similar_function_name_results(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}
                                        int foobar(c){}""")
        self.body[6] = '\t fun(a)\n'
        self.body[7] = '\t foo(a)\n'
        self.ext.calculate_fan_in_fan_out(['fun', 'bar', 'foobar'],
                                          self.body, result,
                                          result.function_list[0])
        self.assertEqual(2, result.function_list[0].fan_in)
        self.assertEqual(2, result.function_list[0].fan_out)
        self.assertEqual(0, result.function_list[1].fan_in)
        self.assertEqual(0, result.function_list[1].fan_out)
        self.assertEqual(0, result.function_list[2].fan_in)
        self.assertEqual(0, result.function_list[2].fan_out)
