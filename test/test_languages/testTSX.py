import unittest
from lizard import analyze_file
from lizard_languages import TSXReader


def get_tsx_function_list(source_code):
    return analyze_file.analyze_source_code("a.tsx", source_code).function_list

class Test_tokenizing_TSX(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TSXReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_simple_standalone(self):
        self.check_tokens(['<abc />'], '<abc />')

    def test_simple_open_closing(self):
        self.check_tokens(['<abc>', '</abc>'], '<abc></abc>')

    def test_open_closing_with_content(self):
        self.check_tokens(['(', '<abc>', 'xxx', '  ', '+', 'yyy', '</abc>', ')'], '(<abc>xxx  +yyy</abc>)')

    def test_nested(self):
        self.check_tokens(['(', '<abc>', '<b>', 'xxx', '</b>', '</abc>', ')'], '(<abc><b>xxx</b></abc>)')

    def test_nested_save_tag(self):
        self.check_tokens(['(', '<b>', '<b>', 'xxx', '</b>', '</b>', ')'], '(<b><b>xxx</b></b>)')

    def test_with_embeded_code(self):
        self.check_tokens(['<abc>', '{', 'x', '}', '</abc>'], '<abc>{x}</abc>')

    def test_with_attributes(self):
        self.check_tokens(['<abc x="x">a</abc>'], '<abc x="x">a</abc>')

    def test_with_embeded_attributes(self):
        self.check_tokens(['y'], '<abc x={y}>a</abc><a></a>')

    def test_less_than(self):
        self.check_tokens(['a', '<', '3', ' ', 'x', '>'], 'a<3 x>')

    def test_with_less_than2(self):
        self.check_tokens(['a', '<', 'b', ' ', 'and', ' ', 'c', '>', ' ', 'd'], 'a<b and c> d')

    def test_complicated_properties(self):
        self.check_tokens(['data', ' ', '=>', '(', ')'], '<StaticQuery render={data =>()} />')


class Test_parser_for_TypeScript_X(unittest.TestCase):

    def test_simple_function(self):
        functions = get_tsx_function_list("x=>x")
        self.assertEqual("(anonymous)", functions[0].name)

    def test_complicated(self):
        code = '''
          <StaticQuery render={data => ()} />
        '''

        functions = get_tsx_function_list(code)
        self.assertEqual("(anonymous)", functions[0].name)

    def test_type_annotation(self):
        code = '''
          const MyComponent: React.FC = () => {
            return <div>Hello</div>;
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual("MyComponent", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_plain_type_annotation(self):
        code = '''
          const MyComponent: React.FC = () => {
            return "hello";
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual("MyComponent", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_complex_jsx_with_typescript_annotations1(self):
        code = '''
          const GridComponent = () => {
            return (
              <div>
                <Grid
                  onClick={ (e: Event) => handleClick(e) }
                />
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        # The main function should be parsed correctly
        self.assertIn("(anonymous)", function_names)
        self.assertIn("GridComponent", function_names)


    def test_complex_jsx_with_typescript_annotations(self):
        code = '''
          const GridComponent = () => {
            return (
              <div>
                <Grid
                  style={{ width: '30%' }}
                  onClick={ (e: React.MouseEvent) => handleClick(e) }
                />
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        # The main function should be parsed correctly
        self.assertIn("(anonymous)", function_names)
        self.assertIn("GridComponent", function_names)

    def test_complex_jsx_with_typescript_annotations2(self):
        code = '''
          const GridComponent = () => {
            return (
              <div>
                <Grid
                  getRowId={ (model: GridRowModel) => model.id }
                  onClick={ (e: React.MouseEvent) => handleClick(e) }
                  onCreated={ (e: Event) => handleCreated(e) }
                />
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        # The main function should be parsed correctly
        self.assertIn("(anonymous)", function_names)
        self.assertIn("GridComponent", function_names)