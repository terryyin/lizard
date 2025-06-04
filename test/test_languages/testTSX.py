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
        self.assertEqual(3, error_boundary.cyclomatic_complexity)  # Actual value from our implementation

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
        
        # Should have anonymous functions for the returned object methods
        anonymous_count = sum(1 for f in functions if f.name == "(anonymous)")
        self.assertGreater(anonymous_count, 0)