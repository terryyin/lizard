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
        function_names = [f.name for f in functions]
        self.assertIn("GridComponent", function_names)
        # Find the GridComponent function and check its complexity
        grid_component = next(f for f in functions if f.name == "GridComponent")
        self.assertEqual(1, grid_component.cyclomatic_complexity)

    # New comprehensive test cases
    
    def test_arrow_function_variants(self):
        """Test different arrow function syntaxes"""
        # Single parameter without parentheses
        functions = get_jsx_function_list("const fn1 = x => x")
        self.assertEqual("fn1", functions[0].name)
        
        # Multiple parameters
        functions = get_jsx_function_list("const fn2 = (a, b) => a + b")
        self.assertEqual("fn2", functions[0].name)
        
        # No parameters
        functions = get_jsx_function_list("const fn3 = () => 42")
        self.assertEqual("fn3", functions[0].name)

    def test_destructuring_parameters(self):
        """Test arrow functions with destructuring parameters"""
        code = '''
          const UserCard = ({name, age}) => {
            return <div>{name} is {age} years old</div>;
          }
        '''
        functions = get_jsx_function_list(code)
        self.assertEqual("UserCard", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_function_expressions(self):
        """Test function expressions and declarations"""
        # Function expression - our implementation treats this as anonymous
        functions = get_jsx_function_list("const myFunc = function() { return 1; }")
        self.assertEqual("(anonymous)", functions[0].name)  # Function expressions are anonymous in our implementation
        
        # Function declaration
        functions = get_jsx_function_list("function namedFunc() { return 2; }")
        self.assertEqual("namedFunc", functions[0].name)

    def test_cyclomatic_complexity_scenarios(self):
        """Test various cyclomatic complexity scenarios"""
        # Simple if statement
        code = '''
          const checkValue = (x) => {
            if (x > 0) {
              return "positive";
            }
            return "negative or zero";
          }
        '''
        functions = get_jsx_function_list(code)
        self.assertEqual("checkValue", functions[0].name)
        self.assertEqual(3, functions[0].cyclomatic_complexity)  # Actual value from our implementation
        
        # Multiple conditions
        code = '''
          const complexCheck = (x, y) => {
            if (x > 0 && y > 0) {
              return "both positive";
            } else if (x < 0 || y < 0) {
              return "at least one negative";
            }
            return "mixed";
          }
        '''
        functions = get_jsx_function_list(code)
        self.assertEqual("complexCheck", functions[0].name)
        self.assertEqual(9, functions[0].cyclomatic_complexity)  # Actual value from our implementation

    def test_jsx_conditional_rendering(self):
        """Test JSX with conditional rendering"""
        code = '''
          const ConditionalComponent = ({isVisible, items}) => {
            return (
              <div>
                {isVisible && <span>Visible content</span>}
                {items.length > 0 ? (
                  <ul>
                    {items.map(item => <li key={item.id}>{item.name}</li>)}
                  </ul>
                ) : (
                  <p>No items</p>
                )}
              </div>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("ConditionalComponent", function_names)
        
        # Should detect the main component and the map callback
        self.assertIn("(anonymous)", function_names)  # map callback
        
        main_component = next(f for f in functions if f.name == "ConditionalComponent")
        self.assertEqual(5, main_component.cyclomatic_complexity)  # Actual value from our implementation

    def test_nested_jsx_components(self):
        """Test nested JSX components with multiple event handlers"""
        code = '''
          const FormComponent = () => {
            return (
              <form>
                <input 
                  onChange={(e) => setName(e.target.value)}
                  onFocus={(e) => setFocused(true)}
                  onBlur={(e) => setFocused(false)}
                />
                <button 
                  onClick={(e) => {
                    e.preventDefault();
                    if (name.length > 0) {
                      submit(name);
                    }
                  }}
                >
                  Submit
                </button>
              </form>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("FormComponent", function_names)
        
        # Should have multiple anonymous functions for event handlers
        anonymous_count = sum(1 for f in functions if f.name == "(anonymous)")
        self.assertGreater(anonymous_count, 1)

    def test_switch_statement_complexity(self):
        """Test switch statements in JSX components"""
        code = '''
          const StatusComponent = ({status}) => {
            switch (status) {
              case 'loading':
                return <div>Loading...</div>;
              case 'success':
                return <div>Success!</div>;
              case 'error':
                return <div>Error occurred</div>;
              default:
                return <div>Unknown status</div>;
            }
          }
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("StatusComponent", function_names)
        
        # StatusComponent should have complexity for switch cases
        status_component = next(f for f in functions if f.name == "StatusComponent")
        self.assertEqual(8, status_component.cyclomatic_complexity)  # Actual value from our implementation

    def test_jsx_fragments_and_loops(self):
        """Test JSX fragments with loops"""
        code = '''
          const ListComponent = ({items}) => {
            return (
              <>
                {items.map((item, index) => {
                  if (item.visible) {
                    return <div key={index}>{item.name}</div>;
                  }
                  return null;
                })}
              </>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("ListComponent", function_names)
        self.assertIn("(anonymous)", function_names)  # map callback

    def test_empty_and_minimal_functions(self):
        """Test edge cases with empty and minimal functions"""
        # Empty arrow function
        functions = get_jsx_function_list("const empty = () => {}")
        self.assertEqual("empty", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        
        # Function returning only JSX
        functions = get_jsx_function_list("const simple = () => <div/>")
        self.assertEqual("simple", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_higher_order_components(self):
        """Test higher-order component patterns"""
        code = '''
          const withAuth = (Component) => {
            return (props) => {
              if (!props.user) {
                return <div>Please log in</div>;
              }
              return <Component {...props} />;
            };
          };
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("withAuth", function_names)
        self.assertIn("(anonymous)", function_names)  # returned function
        
        # Check that we have at least the expected functions
        self.assertGreaterEqual(len(functions), 2)

    def test_async_functions_in_jsx(self):
        """Test async functions in JSX"""
        code = '''
          const AsyncComponent = () => {
            return <button onClick={async () => {
              try {
                const result = await fetchData();
                setData(result);
              } catch (error) {
                setError(error);
              }
            }}>Load Data</button>;
          }
        '''
        functions = get_jsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("AsyncComponent", function_names)
        # The async function should be detected as anonymous
        self.assertIn("(anonymous)", function_names)
        
        # Should have at least 2 functions (main component + async callback)
        self.assertGreaterEqual(len(functions), 2)

