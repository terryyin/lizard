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
        # After Fix 1, attribute expressions handled by TSXTokenizer sub-tokenizer;
        # ';' injected on close, residual tag tokens emitted
        self.check_tokens(['y', ';', '<abc x={>a</abc>', '<a>', '</a>'],
                         '<abc x={y}>a</abc><a></a>')

    def test_less_than(self):
        self.check_tokens(['a', '<', '3', ' ', 'x', '>'], 'a<3 x>')

    def test_with_less_than2(self):
        self.check_tokens(['a', '<', 'b', ' ', 'and', ' ', 'c', '>', ' ', 'd'], 'a<b and c> d')

    def test_complicated_properties(self):
        # After Fix 1, ';' injected when attribute expression ends
        self.check_tokens(['data', ' ', '=>', '(', ')', ';', '<StaticQuery render={ />'],
                         '<StaticQuery render={data =>()} />')


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

    # New comprehensive TypeScript-specific test cases

    def test_generic_function_components(self):
        """Test generic function components"""
        code = '''
          const GenericComponent = <T,>(props: { items: T[] }) => {
            return (
              <ul>
                {props.items.map((item: T, index: number) => (
                  <li key={index}>{String(item)}</li>
                ))}
              </ul>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("GenericComponent", function_names)
        self.assertIn("(anonymous)", function_names)  # map callback

    def test_interface_based_props(self):
        """Test components with interface-based props"""
        code = '''
          interface UserProps {
            name: string;
            age?: number;
            onSelect?: (user: User) => void;
          }
          
          const UserComponent: React.FC<UserProps> = ({name, age, onSelect}) => {
            return (
              <div onClick={() => {
                if (onSelect) {
                  onSelect({name, age});
                }
              }}>
                {name} {age && `(${age} years old)`}
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("UserComponent", function_names)
        # Should have an anonymous function for the onClick handler
        self.assertIn("(anonymous)", function_names)

    def test_union_types_and_type_guards(self):
        """Test union types and type guards"""
        code = '''
          type Status = 'loading' | 'success' | 'error';
          
          const StatusComponent = ({status}: {status: Status}) => {
            switch (status) {
              case 'loading':
                return <div>Loading...</div>;
              case 'success':
                return <div>Success!</div>;
              case 'error':
                return <div>Error!</div>;
              default:
                return <div>Unknown</div>;
            }
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("StatusComponent", function_names)
        
        # StatusComponent should have switch complexity
        status_component = next(f for f in functions if f.name == "StatusComponent")
        self.assertGreater(status_component.cyclomatic_complexity, 1)

    def test_async_typescript_functions(self):
        """Test async functions with TypeScript"""
        code = '''
          const DataComponent = () => {
            return <button onClick={async (): Promise<void> => {
              try {
                const response = await fetch('/api/data');
                if (!response.ok) {
                  throw new Error('Failed to fetch');
                }
                const data = await response.json();
                console.log(data);
              } catch (error) {
                console.error(error);
              }
            }}>Load</button>;
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("DataComponent", function_names)
        self.assertIn("(anonymous)", function_names)  # async onClick callback
        
        # Should have at least 2 functions (main component + async callback)
        self.assertGreaterEqual(len(functions), 2)

    def test_typescript_event_handlers(self):
        """Test TypeScript event handlers with specific event types"""
        code = '''
          const FormComponent = () => {
            return (
              <form onSubmit={(e: React.FormEvent<HTMLFormElement>): void => {
                e.preventDefault();
                const formData = new FormData(e.currentTarget);
              }}>
                <input onChange={(e: React.ChangeEvent<HTMLInputElement>): void => {
                  if (e.target.value.length > 10) {
                    console.warn('Too long');
                  }
                }} />
                <button type="submit">Submit</button>
              </form>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("FormComponent", function_names)
        
        # Should have multiple anonymous functions for event handlers
        anonymous_count = sum(1 for f in functions if f.name == "(anonymous)")
        self.assertGreater(anonymous_count, 1)

    def test_optional_chaining_and_nullish_coalescing(self):
        """Test optional chaining and nullish coalescing"""
        code = '''
          interface NestedData {
            user?: {
              profile?: {
                name: string;
              };
            };
          }
          
          const ProfileComponent = ({data}: {data: NestedData}) => {
            return (
              <div onClick={() => {
                if (data?.user?.profile) {
                  console.log('Profile exists');
                } else {
                  console.log('No profile');
                }
              }}>
                {data?.user?.profile?.name ?? 'Unknown'}
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("ProfileComponent", function_names)
        self.assertIn("(anonymous)", function_names)

    def test_generic_constraints_and_conditional_types(self):
        """Test generic constraints and conditional rendering"""
        code = '''
          interface HasId {
            id: string | number;
          }
          
          const ListComponent = <T extends HasId>({
            items,
            renderItem
          }: {
            items: T[];
            renderItem: (item: T) => React.ReactNode;
          }) => {
            return (
              <ul>
                {items.map((item: T) => (
                  <li key={item.id} onClick={() => {
                    if (typeof item.id === 'string') {
                      console.log(`String ID: ${item.id}`);
                    } else {
                      console.log(`Number ID: ${item.id}`);
                    }
                  }}>
                    {renderItem(item)}
                  </li>
                ))}
              </ul>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("ListComponent", function_names)
        
        # Should have multiple anonymous functions
        anonymous_count = sum(1 for f in functions if f.name == "(anonymous)")
        self.assertGreater(anonymous_count, 1)

    def test_typescript_hooks_patterns(self):
        """Test TypeScript with React hooks patterns"""
        code = '''
          const useCustomHook = (initialValue: string) => {
            const [value, setValue] = useState<string>(initialValue);
            
            return { 
              value, 
              updateValue: (newValue: string): void => {
                if (newValue.length > 0) {
                  setValue(newValue);
                }
              }
            };
          };
          
          const HookComponent = () => {
            const { value, updateValue } = useCustomHook('initial');
            
            return <input 
              value={value} 
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                updateValue(e.target.value);
              }} 
            />;
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("useCustomHook", function_names)
        self.assertIn("HookComponent", function_names)
        # Should have anonymous functions
        self.assertIn("(anonymous)", function_names)

    def test_typescript_error_boundaries(self):
        """Test TypeScript error boundary patterns"""
        code = '''
          interface ErrorInfo {
            componentStack: string;
          }
          
          const ErrorBoundary = ({children}: {children: React.ReactNode}) => {
            const [hasError, setHasError] = useState<boolean>(false);
            
            if (hasError) {
              return (
                <div>
                  <h2>Something went wrong.</h2>
                  <button onClick={() => setHasError(false)}>Try again</button>
                </div>
              );
            }
            
            return <>{children}</>;
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("ErrorBoundary", function_names)
        self.assertIn("(anonymous)", function_names)  # onClick callback
        
        # ErrorBoundary should have conditional complexity
        error_boundary = next(f for f in functions if f.name == "ErrorBoundary")
        self.assertEqual(2, error_boundary.cyclomatic_complexity)

    def test_complex_typescript_patterns(self):
        """Test complex TypeScript patterns with mapped types and utilities"""
        code = '''
          type PartialExcept<T, K extends keyof T> = Partial<T> & Pick<T, K>;
          
          interface FormData {
            name: string;
            email: string;
            age: number;
          }
          
          const FormValidator = () => {
            return {
              validateField: <K extends keyof FormData>(
                field: K,
                value: FormData[K]
              ): boolean => {
                switch (field) {
                  case 'name':
                    return typeof value === 'string' && value.length > 0;
                  case 'email':
                    return typeof value === 'string' && value.includes('@');
                  case 'age':
                    return typeof value === 'number' && value > 0;
                  default:
                    return false;
                }
              },
              processForm: (data: PartialExcept<FormData, 'name'>) => {
                for (const [key, value] of Object.entries(data)) {
                  console.log(`Processing ${key}:`, value);
                }
              }
            };
          }
        '''
        functions = get_tsx_function_list(code)
        function_names = [f.name for f in functions]
        self.assertIn("FormValidator", function_names)

        # The generic arrow methods in the return object are detected by name
        # (validateField, processForm) rather than as anonymous
        self.assertIn("validateField", function_names)
        self.assertIn("processForm", function_names)


class Test_TSX_jsx_attribute_handlers(unittest.TestCase):
    """Tests that JSX attribute arrow handlers are correctly detected (Fix 1)."""

    def test_multi_handler_on_single_element(self):
        """Multiple arrow handlers on one element should all be detected"""
        code = '''
          const Form = () => {
            return (
              <form>
                <input
                  onChange={(e) => setVal(e.target.value)}
                  onFocus={() => setFocused(true)}
                  onBlur={() => setFocused(false)}
                />
                <button onClick={() => submit()}>Go</button>
              </form>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Form", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 4)  # 3 input handlers + 1 button handler

    def test_inline_conditional_handler(self):
        """Arrow handler with conditional body should be detected"""
        code = '''
          const Toggle = ({enabled, onToggle}) => {
            return (
              <button
                onClick={() => onToggle(!enabled)}
                className={enabled ? "active" : "inactive"}
              >
                {enabled ? "ON" : "OFF"}
              </button>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Toggle", names)
        self.assertIn("(anonymous)", names)

    def test_map_callback_in_jsx(self):
        """Array.map callback inside JSX should detect both component and callback"""
        code = '''
          const List = ({items}) => {
            return (
              <ul>
                {items.map((item, i) => (
                  <li key={i}>{item.name}</li>
                ))}
              </ul>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("List", names)
        self.assertIn("(anonymous)", names)

    def test_map_with_click_handler(self):
        """Map callback + onClick handler in list items"""
        code = '''
          const TodoList = ({todos}) => {
            return (
              <ul>
                {todos.map(todo => (
                  <li key={todo.id} onClick={() => toggle(todo.id)}>
                    {todo.text}
                  </li>
                ))}
              </ul>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("TodoList", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 2)  # map callback + onClick


class Test_TSX_component_patterns(unittest.TestCase):
    """Tests various React component patterns in TSX."""

    def test_conditional_rendering(self):
        """Conditional rendering with && and ternary"""
        code = '''
          const View = ({show, data}) => {
            return (
              <div>
                {show && <span>visible</span>}
                {data ? <Data items={data} /> : <Empty />}
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual(["View"], [f.name for f in functions])

    def test_forwardref(self):
        """React.forwardRef wrapping an arrow function"""
        code = '''
          const Input = React.forwardRef((props, ref) => {
            return <input ref={ref} value={props.value} />;
          });
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("(anonymous)", names)

    def test_memo(self):
        """React.memo wrapping a component"""
        code = '''
          const Memoized = React.memo((props) => {
            return <div>{props.value}</div>;
          });
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("(anonymous)", names)

    def test_usecallback_hook(self):
        """useCallback hook should detect both component and callback"""
        code = '''
          const App = () => {
            const handleClick = useCallback(() => {
              doSomething();
            }, []);
            return <button onClick={handleClick}>click</button>;
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("App", names)
        self.assertIn("(anonymous)", names)

    def test_context_provider(self):
        """Context provider component"""
        code = '''
          const ThemeProvider = ({children}) => {
            const [theme, setTheme] = useState("light");
            return (
              <ThemeContext.Provider value={{theme, setTheme}}>
                {children}
              </ThemeContext.Provider>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual(["ThemeProvider"], [f.name for f in functions])

    def test_render_prop(self):
        """Render prop pattern should detect both provider and consumer components"""
        code = '''
          const DataFetcher = ({render}) => {
            const [data, setData] = useState(null);
            return render(data);
          }
          const App = () => {
            return <DataFetcher render={(data) => <Display data={data} />} />;
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("DataFetcher", names)
        self.assertIn("App", names)
        self.assertIn("(anonymous)", names)  # render prop callback

    def test_fragment(self):
        """Fragment component should be detected"""
        code = '''
          const Multi = () => {
            return (
              <>
                <First />
                <Second />
              </>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual(["Multi"], [f.name for f in functions])

    def test_nested_ternary(self):
        """Nested ternary in JSX"""
        code = '''
          const Status = ({code}) => {
            return (
              <div>
                {code === 200 ? <Ok /> : code === 404 ? <NotFound /> : <Error />}
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual(["Status"], [f.name for f in functions])

    def test_children_function_pattern(self):
        """Children-as-function pattern"""
        code = '''
          const Wrapper = ({children}) => {
            return <div className="wrapper">{children}</div>;
          }
          const App = () => {
            return (
              <Wrapper>
                {(data) => <span>{data}</span>}
              </Wrapper>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Wrapper", names)
        self.assertIn("App", names)
        self.assertIn("(anonymous)", names)

    def test_spread_attrs(self):
        """Spread attributes should not break detection"""
        code = '''
          const Wrapper = (props) => {
            return <Inner {...props} extra="val" />;
          }
        '''
        functions = get_tsx_function_list(code)
        self.assertEqual(["Wrapper"], [f.name for f in functions])


class Test_TSX_style_objects_no_fp(unittest.TestCase):
    """Tests that inline style objects in JSX do NOT produce false positives."""

    def test_style_object_no_fp(self):
        """style={{...}} should not create function FPs"""
        code = '''
          const Styled = () => {
            return (
              <div style={{
                backgroundColor: "red",
                fontSize: 14,
                padding: "10px"
              }}>
                content
              </div>
            );
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertEqual(["Styled"], names)


class Test_TSX_class_with_static_fields(unittest.TestCase):
    """Tests class components with static fields don't lose method detection (Fix 4)."""

    def test_static_proptypes_then_methods(self):
        """Methods after static propTypes = {...} should be detected"""
        code = '''
          class Card extends Component {
            static propTypes = {
              title: PropTypes.string,
              onClick: PropTypes.func
            };
            static defaultProps = {
              title: "Default"
            };
            constructor(props) {
              super(props);
              this.state = {};
            }
            handleClick() {
              this.props.onClick();
            }
            render() {
              return <div onClick={() => this.handleClick()}>{this.props.title}</div>;
            }
          }
        '''
        functions = get_tsx_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("constructor", names)
        self.assertIn("handleClick", names)
        self.assertIn("render", names)
        # static field names should NOT appear as functions
        self.assertNotIn("propTypes", names)
        self.assertNotIn("defaultProps", names)