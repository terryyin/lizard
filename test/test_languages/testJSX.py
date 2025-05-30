import unittest
from lizard import  analyze_file

def get_jsx_function_list(source_code):
    return analyze_file.analyze_source_code("a.jsx", source_code).function_list

class Test_parser_for_JavaScript_X(unittest.TestCase):

    def test_simple_function(self):
        functions = get_jsx_function_list("x=>x")
        self.assertEqual("(anonymous)", functions[0].name)

    def test_complicated(self):
        code = '''
          <StaticQuery render={data => ()} />
        '''

        functions = get_jsx_function_list(code)
        self.assertEqual("(anonymous)", functions[0].name)
        
    def test_complex_jsx_attributes(self):
        code = '''
          const GridComponent = () => {
            return (
              <div>
                <Grid
                  getRowId={ (model) => model.id }
                  onClick={ (e) => handleClick(e) }
                  style={{ width: '30%' }}
                  onKeyDown={(e) => {
                    if (e.key === 'Enter') {
                      doSomething();
                    }
                  }}
                />
              </div>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        # The main function should be parsed correctly
        self.assertEqual("GridComponent", functions[0].name)
        # The function should have the correct complexity
        self.assertEqual(1, functions[0].cyclomatic_complexity)

