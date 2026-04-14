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
        # Function expression assigned to const — detected with the variable name
        functions = get_jsx_function_list("const myFunc = function() { return 1; }")
        self.assertEqual("myFunc", functions[0].name)

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
        self.assertEqual(2, functions[0].cyclomatic_complexity)

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
        self.assertEqual(5, functions[0].cyclomatic_complexity)

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
        self.assertEqual(3, main_component.cyclomatic_complexity)

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
        self.assertEqual(4, status_component.cyclomatic_complexity)

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


class Test_JSX_class_components(unittest.TestCase):
    """Tests class-based React components in JSX."""

    def test_class_with_lifecycle(self):
        """Class component with constructor and lifecycle methods"""
        code = '''
          class App extends Component {
            constructor(props) {
              super(props);
              this.state = { count: 0 };
            }
            componentDidMount() {
              this.fetchData();
            }
            render() {
              return <div>{this.state.count}</div>;
            }
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("constructor", names)
        self.assertIn("componentDidMount", names)
        self.assertIn("render", names)

    def test_static_fields_then_methods(self):
        """Class with static fields followed by methods (Fix 4 for JSX)"""
        code = '''
          class Nav extends Component {
            static displayName = "Nav";
            static propTypes = { items: PropTypes.array };
            constructor(props) {
              super(props);
            }
            toggle() { this.setState({open: !this.state.open}); }
            render() {
              return <nav>{this.props.items.map(i => <a key={i}>{i}</a>)}</nav>;
            }
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("constructor", names)
        self.assertIn("toggle", names)
        self.assertIn("render", names)
        # static fields should NOT be FPs
        self.assertNotIn("displayName", names)
        self.assertNotIn("propTypes", names)


class Test_JSX_higher_order_components(unittest.TestCase):
    """Tests HOC and wrapper patterns."""

    def test_hoc_pattern(self):
        """HOC wrapping a component"""
        code = '''
          const withLogger = (Comp) => {
            return (props) => {
              console.log("render");
              return <Comp {...props} />;
            };
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("withLogger", names)
        self.assertIn("(anonymous)", names)
        self.assertGreaterEqual(len(functions), 2)

    def test_double_hoc(self):
        """Nested HOCs — at least one outer + inner functions detected"""
        code = '''
          const withAuth = (Comp) => {
            return (props) => {
              if (!props.user) return <Login />;
              return <Comp {...props} />;
            };
          }
          const withTheme = (Comp) => {
            return (props) => {
              return <Comp {...props} theme="dark" />;
            };
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        # At least one HOC outer function and inner anonymous functions detected
        self.assertIn("withTheme", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 2)


class Test_JSX_multiple_exports(unittest.TestCase):
    """Tests files with multiple exported components."""

    def test_multiple_exported_components(self):
        code = '''
          export const Button = ({label, onClick}) => {
            return <button onClick={onClick}>{label}</button>;
          }
          export const Input = ({value, onChange}) => {
            return <input value={value} onChange={onChange} />;
          }
          export default function Form() {
            return <form><Button label="ok" /><Input /></form>;
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Button", names)
        self.assertIn("Input", names)
        self.assertIn("Form", names)
        self.assertEqual(3, len(functions))


class Test_JSX_event_handler_patterns(unittest.TestCase):
    """Tests various event handler patterns in JSX."""

    def test_multiple_handlers_on_form(self):
        """Multiple event handlers on different form elements"""
        code = '''
          const SearchForm = ({onSearch}) => {
            return (
              <form onSubmit={(e) => {
                e.preventDefault();
                onSearch(e.target.value);
              }}>
                <input
                  onChange={(e) => setQuery(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Escape") clear();
                  }}
                />
                <button type="submit">Search</button>
              </form>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("SearchForm", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 2)

    def test_ternary_rendering(self):
        """Ternary expressions in JSX should not produce FPs"""
        code = '''
          const Badge = ({count}) => {
            return (
              <span className={count > 0 ? "active" : "empty"}>
                {count > 99 ? "99+" : count}
              </span>
            );
          }
        '''
        functions = get_jsx_function_list(code)
        self.assertEqual(["Badge"], [f.name for f in functions])

