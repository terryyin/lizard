import unittest
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
from mock import patch, mock_open
from lizard import analyze_file
from .testHelpers import get_cpp_fileinfo
from lizard_ext.lizardfans import LizardExtension as Fans
from lizard_ext.lizardfans import open_method, get_method_body, CalculateFans


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
        self.body = ['{\n', '\tif (a && b){\n', '\tfun(a);\n',
                     '\tif( a != 0 ){\n', '\n', '\ta = b;\n',
                     '\t c = a;\n', '\t}\n', '}\n']

    def test_tokens(self):
        self.tokens = list(self.ext(["a", "b"], self.reader))
        self.assertEqual(['a', 'b'], self.tokens)

    def test_fans_initiation(self):
        list(self.ext(["a", "b"], self.reader))
        self.assertEqual((0, 0), self.reader.get_fan_in_fan_out())

    @patch('{}.open'.format(__name__), create=True)
    def test_some(self, mock_open):
        result = get_cpp_function_list_with_fans_extension("a.cpp",
                                                           "int fun(a){})")
        mock_open.side_effect = [
            mock_open(result[0].filename).return_value
        ]
        self.assertEqual(None, open_method(result[0].filename))
        mock_open.assert_called_once_with("a.cpp")
        mock_open.reset_mock()

    def test_open_function_infile_write(self):
        m = mock_open()
        with patch('{}.open'.format(__name__), m, create=True):
            with open('a.cpp', 'w') as h:
                h.write('some stuff')
        m.assert_called_once_with('a.cpp', 'w')
        handle = m()
        handle.write.assert_called_once_with('some stuff')

    def test_open_function_infile_writelines(self):
        m = mock_open()
        with patch('{}.open'.format(__name__), m, create=True):
            with open('a.cpp', 'w') as h:
                h.writelines("""int c() {
    if (a && b) {
        if( a != 0 ){
            a = b;
        }
    }
}""")
        m.assert_called_once_with('a.cpp', 'w')
        handle = m()
        handle.writelines.assert_called_once_with("""int c() {
    if (a && b) {
        if( a != 0 ){
            a = b;
        }
    }
}""")

    def test_get_method_body(self):
        result = get_cpp_function_list_with_fans_extension("a.cpp","""int c() {
    if (a && b) {
        if( a != 0 ){
            a = b;
        }
    }
}""")
        infile = StringIO("""int c() {
    if (a && b) {
        if( a != 0 ){
            a = b;
        }
    }
}""")
        body = get_method_body(infile, result[0])
        self.assertEqual(body, ['    if (a && b) {\n',
                                '        if( a != 0 ){\n',
                                '            a = b;\n',
                                '        }\n',
                                '    }\n',
                                '}'])

    def test_fan_in_fan_out_only_result(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        CalculateFans(['fun', 'bar'],
                      self.body, result,
                      result.function_list[1]).calculate_fan_in_fan_out()
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(0, result.function_list[0].fan_out)
        self.assertEqual(0, result.function_list[1].fan_in)
        self.assertEqual(1, result.function_list[1].fan_out)

    def test_single_dual_fans_result(self):
        result = get_cpp_fans_extension("""int fun(a){}""")
        self.body[6] = '\t bar(a)\n'
        CalculateFans(['fun'],
                      self.body, result,
                      result.function_list[0]).calculate_fan_in_fan_out()
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(1, result.function_list[0].fan_out)

    def test_multi_body_method_result_part1(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        self.body[6] = '\t bar(a)\n'
        CalculateFans(['fun', 'bar'],
                      self.body, result,
                      result.function_list[0]).calculate_fan_in_fan_out()
        self.assertEqual(1, result.function_list[0].fan_in)
        self.assertEqual(2, result.function_list[0].fan_out)
        self.assertEqual(1, result.function_list[1].fan_in)
        self.assertEqual(0, result.function_list[1].fan_out)

    def test_multi_body_method_result_part2(self):
        result = get_cpp_fans_extension("""int fun(a){}
                                        int bar(a){}""")
        self.body[6] = '\t fun(a)\n'
        CalculateFans(['fun', 'bar'],
                      self.body, result,
                      result.function_list[1]).calculate_fan_in_fan_out()
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
        CalculateFans(['fun', 'bar', 'foobar'],
                      self.body, result,
                      result.function_list[0]).calculate_fan_in_fan_out()
        self.assertEqual(2, result.function_list[0].fan_in)
        self.assertEqual(2, result.function_list[0].fan_out)
        self.assertEqual(0, result.function_list[1].fan_in)
        self.assertEqual(0, result.function_list[1].fan_out)
        self.assertEqual(0, result.function_list[2].fan_in)
        self.assertEqual(0, result.function_list[2].fan_out)
