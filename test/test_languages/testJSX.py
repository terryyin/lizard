import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import JavaScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list

class Test_tokenizing_JSX(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(JavaScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_simple_standalone(self):
        self.check_tokens(['<abc />'], '<abc />')

    def test_simple_open_closing(self):
        self.check_tokens(['<abc></abc>'], '<abc></abc>')

    def test_open_closing_with_content(self):
        self.check_tokens(['(', '<abc>xxx  +yyy</abc>', ')'], '(<abc>xxx  +yyy</abc>)')

    def test_nested(self):
        self.check_tokens(['(', '<abc>', '<b>xxx</b>', '</abc>', ')'], '(<abc><b>xxx</b></abc>)')

    def test_nested_save_tag(self):
        self.check_tokens(['(', '<b>', '<b>xxx</b>', '</b>', ')'], '(<b><b>xxx</b></b>)')

    def test_with_embeded_code(self):
        self.check_tokens(['<abc>{', 'x', '</abc>'], '<abc>{x}</abc>')

    def test_with_attributes(self):
        self.check_tokens(['<abc x="x">a</abc>'], '<abc x="x">a</abc>')

    def test_with_embeded_attributes(self):
        self.check_tokens(['y', '<abc x={}>a</abc>', '<a></a>'], '<abc x={y}>a</abc><a></a>')

    def test_less_than(self):
        self.check_tokens(['a', '<', '3', ' ', 'x', '>'], 'a<3 x>')

    def test_with_less_than2(self):
        self.check_tokens(['a', '<', 'b', ' ', 'and', ' ', 'c', '>', ' ', 'd'], 'a<b and c> d')

    def test_complicated_properties(self):
        self.check_tokens(['data', ' ', '=>', '(', ')', '<StaticQuery render={} />'], '<StaticQuery render={data =>()} />')


class Test_parser_for_JavaScript_X(unittest.TestCase):

    def test_simple_function(self):
        functions = get_js_function_list("x=>x")
        self.assertEqual("(anonymous)", functions[0].name)

    def test_complicated(self):
        code = '''
          <StaticQuery render={data => ()} />
        '''

        functions = get_js_function_list(code)
        self.assertEqual("(anonymous)", functions[0].name)

