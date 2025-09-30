'''
Language parser for TSX
'''

from .code_reader import CodeStateMachine
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression
from .typescript import TypeScriptReader
from .typescript import JSTokenizer, Tokenizer, TypeScriptStates


class TSXReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['tsx', 'jsx']
    language_names = ['tsx', 'jsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Add support for TypeScript type annotations in JSX
        addition = addition + \
            r"|(?:<[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:<\/[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:\$\w+)" + \
            r"|(?:<\/\w+>)" + \
            r"|(?:=>)" + \
            r"|`.*?`"
        js_tokenizer = TSXTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok

    def __init__(self, context):
        super(TSXReader, self).__init__(context)
        # Use JSXTypeScriptStates for better handling of TSX specific features
        self.parallel_states = [JSXTypeScriptStates(context)]


class JSXTypeScriptStates(CodeStateMachine):
    """State machine for JSX/TSX files using composition with TypeScriptStates"""

    def __init__(self, context, inside_function=False):
        super().__init__(context)
        self.last_token = ''
        self.function_name = ''
        self.started_function = None
        self.pending_function_name = ''
        self._ts_declare = False
        self._conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                               'catch', 'case'])
        self.inside_function = inside_function  # Track if we're already inside a function

    def statemachine_before_return(self):
        # Ensure any pending function is closed - this should make the main function appear first
        if self.started_function:
            self._pop_function_from_stack()

    def _state_global(self, token):
        # Handle TypeScript declare keyword
        if token == 'declare':
            self._ts_declare = True
            return
        if token == 'function' and self._ts_declare:
            # Skip declared functions
            self._ts_declare = False
            self.next(self._skip_until_semicolon)
            return
        self._ts_declare = False

        # Handle function keyword
        if token == 'function':
            self.next(self._function_name_state)
            return

        # Handle arrow functions: look for =>
        if token == '=>':
            # Start arrow function with pending name or anonymous
            self._start_arrow_function()
            return

        # Handle control flow for cyclomatic complexity
        if token in ('if', 'switch', 'for', 'while', 'catch'):
            if self.started_function or self.inside_function:
                self.context.add_condition()
            self.next(self._expecting_condition_and_statement)
            return
        elif token in ('else', 'do', 'try', 'finally'):
            self.next(self._expecting_statement_or_block)
            return
        elif token in ('&&', '||', '?'):
            if self.started_function or self.inside_function:
                self.context.add_condition()
            return
        elif token == 'case':
            if self.started_function or self.inside_function:
                self.context.add_condition()
            return

        # Handle assignment for function names - only if not inside a function
        if token == '=' and not self.inside_function:
            # If we don't have a pending name yet, use the last token
            if not self.pending_function_name:
                self.pending_function_name = self.last_token
            return

        # Handle type annotations
        if token == ':' and not self.inside_function:
            # This could be a type annotation before assignment (e.g., const name: Type = ...)
            # Store the current token as potential function name and consume the type
            if not self.pending_function_name:
                self.pending_function_name = self.last_token
            self._consume_type_annotation()
            return

        # Handle braces
        if token == '{':
            if self.started_function or self.inside_function:
                # Function body - stay in current function
                nested_state = self.__class__(self.context, inside_function=True)
                self.sub_state(nested_state)
            else:
                # Object or block
                self.sub_state(self.__class__(self.context))
            return
        elif token == '}':
            self.statemachine_return()
            return

        # Handle parentheses - be careful with arrow function parameters
        if token == '(':
            if self.pending_function_name:
                # This might be arrow function parameters, handle in-line
                self.next(self._arrow_function_params)
            else:
                # Regular parentheses grouping or function call
                nested_state = self.__class__(self.context, inside_function=self.inside_function)
                self.sub_state(nested_state)
            return
        elif token == ')':
            self.statemachine_return()
            return

        # Handle end of statement
        if token == ';' or self.context.newline:
            if not self.inside_function:
                self.pending_function_name = ''
            self._pop_function_from_stack()

        self.last_token = token

    def _function_name_state(self, token):
        """Handle function name after 'function' keyword"""
        if token == '(':
            # Anonymous function
            self._start_function('(anonymous)')
            self.next(self._function_parameters, token)
        else:
            # Named function
            self._start_function(token)
            self.next(self._expecting_function_params)

    def _expecting_function_params(self, token):
        """Expect opening parenthesis for function parameters"""
        if token == '(':
            self.next(self._function_parameters, token)
        else:
            # Not a function, return to global state
            self.next(self._state_global, token)

    def _function_parameters(self, token):
        """Handle function parameters"""
        if token == ')':
            self.next(self._expecting_function_body)
        elif token == '(':
            # Nested parentheses in parameters
            self.sub_state(self.__class__(self.context))
        else:
            # Parameter token
            if token not in (',', ':', '=', '?', '...') and token.isalnum():
                self.context.parameter(token)

    def _expecting_function_body(self, token):
        """Expect function body (could be block or expression for arrow functions)"""
        if token == '{':
            # Block body - create new scope but stay in current function
            def callback():
                self.next(self._state_global)
            nested_state = self.__class__(self.context, inside_function=True)
            # Don't start a new function in the nested state
            self.sub_state(nested_state, callback)
        elif token == ':':
            # Type annotation for return type
            self._consume_type_annotation()
        else:
            # Expression body or other tokens - continue in global state
            self.next(self._state_global, token)

    def _start_arrow_function(self):
        """Start an arrow function with pending name or anonymous"""
        name = self.pending_function_name or '(anonymous)'

        if self.inside_function:
            # For nested functions, create and immediately close them
            # This ensures they're detected but don't interfere with the main function
            self.context.push_new_function(name)
            self.context.end_of_function()
        else:
            # For top-level functions, use normal processing
            self._start_function(name)

        self.pending_function_name = ''

    def _start_function(self, name):
        """Start a new function"""
        # Always start a new function - nested functions are separate functions
        self.context.push_new_function(name)
        # Track that we started a function for proper cleanup
        self.started_function = True
        self.function_name = name

    def _pop_function_from_stack(self):
        """End current function"""
        if self.started_function:
            self.context.end_of_function()
            self.started_function = None
            self.function_name = ''

    def _expecting_condition_and_statement(self, token):
        """Handle conditional statements with conditions"""
        if token == '(':
            # Condition in parentheses
            def callback():
                self.next(self._expecting_statement_or_block)
            nested_state = self.__class__(self.context, inside_function=self.inside_function)
            self.sub_state(nested_state, callback)
        else:
            # No parentheses, go directly to statement
            self.next(self._state_global, token)

    def _expecting_statement_or_block(self, token):
        """Handle statement or block after control flow"""
        if token == '{':
            # Block statement
            def callback():
                self.next(self._state_global)
            nested_state = self.__class__(self.context, inside_function=self.inside_function)
            self.sub_state(nested_state, callback)
        else:
            # Single statement
            self.next(self._state_global, token)

    def _consume_type_annotation(self):
        """Handle TypeScript type annotations"""
        def callback():
            # Continue with any saved token from the type annotation handler
            type_handler = self.sub_state_instance
            if hasattr(type_handler, 'saved_token') and type_handler.saved_token:
                self.next(self._state_global, type_handler.saved_token)
            else:
                self.next(self._state_global)

        type_handler = JSXTypeAnnotationHandler(self.context)
        self.sub_state_instance = type_handler  # Store reference to access saved_token
        self.sub_state(type_handler, callback)

    def _skip_until_semicolon(self, token):
        """Skip tokens until semicolon or newline (for declare statements)"""
        if token == ';' or self.context.newline:
            self.next(self._state_global)

    def _arrow_function_params(self, token):
        """Handle arrow function parameters without losing function name"""
        if token == ')':
            # End of parameters, expect => next
            self.next(self._expecting_arrow)
        elif token == '(':
            # Nested parentheses in parameters
            nested_state = self.__class__(self.context, inside_function=self.inside_function)
            self.sub_state(nested_state)
        else:
            # Parameter token - add to context if it's a valid parameter
            if token not in (',', ':', '=', '?', '...') and token.isalnum():
                # Don't add parameters yet, wait for arrow to confirm it's a function
                pass

    def _expecting_arrow(self, token):
        """Expect => after arrow function parameters"""
        if token == '=>':
            # Confirmed arrow function, start it now
            self._start_arrow_function()
        elif token == ':':
            # Type annotation for return type
            self._consume_type_annotation()
        else:
            # Not an arrow function, return to global state
            self.pending_function_name = ''
            self.next(self._state_global, token)


class JSXTypeAnnotationHandler(CodeStateMachine):
    """Handle TypeScript type annotations in JSX/TSX"""

    def __init__(self, context):
        super().__init__(context)
        self.depth = 0
        self.saved_token = None

    def _state_global(self, token):
        if token == '<':
            # Generic type
            self.depth += 1
        elif token == '>':
            self.depth -= 1
            if self.depth < 0:
                # End of type annotation
                self.saved_token = token
                self.statemachine_return()
        elif token in ('=', ';', ')', '}', '=>') and self.depth == 0:
            # End of type annotation
            self.saved_token = token
            self.statemachine_return()
        elif token == '{':
            # Object type annotation
            self.next(self._object_type)
        elif token == '(':
            # Function type annotation
            self.next(self._function_type)

    def _object_type(self, token):
        """Handle object type annotations like { width: string }"""
        if token == '}':
            self.next(self._state_global)

    def _function_type(self, token):
        """Handle function type annotations like (param: Type) => ReturnType"""
        if token == ')':
            self.next(self._state_global)


class TSXTokenizer(JSTokenizer):
    def __init__(self):
        super().__init__()

    def process_token(self, token):
        if token == "<":
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            return

        if token == "=>":
            # Special handling for arrow functions
            yield token
            return

        for tok in super().process_token(token):
            yield tok


class XMLTagWithAttrTokenizer(Tokenizer):
    def __init__(self):
        super(XMLTagWithAttrTokenizer, self).__init__()
        self.tag = None
        self.state = self._global_state
        self.cache = ['<']
        self.brace_count = 0  # Track nested braces for complex expressions
        self.arrow_function_detected = False  # Track if we've detected an arrow function

    def process_token(self, token):
        self.cache.append(token)
        if not token.isspace():
            result = self.state(token)
            if result is not None:
                if isinstance(result, list):
                    for tok in result:
                        yield tok
                else:
                    return result
        return ()

    def abort(self):
        self.stop()
        return self.cache

    def flush(self):
        tmp, self.cache = self.cache, []
        return [''.join(tmp)]

    def _global_state(self, token):
        if not isidentifier(token):
            return self.abort()
        self.tag = token
        self.state = self._after_tag

    def _after_tag(self, token):
        if token == '>':
            self.state = self._body
        elif token == "/":
            self.state = self._expecting_self_closing
        elif isidentifier(token):
            self.state = self._expecting_equal_sign
        else:
            return self.abort()

    def _expecting_self_closing(self, token):
        if token == ">":
            self.stop()
            return self.flush()
        return self.abort()

    def _expecting_equal_sign(self, token):
        if token == '=':
            self.state = self._expecting_value
        else:
            return self.abort()

    def _expecting_value(self, token):
        if token[0] in "'\"":
            self.state = self._after_tag
        elif token == "{":
            self.brace_count = 1  # Start counting braces
            self.state = self._jsx_expression
            # Don't add the closing brace automatically
            # self.cache.append("}")
            self.sub_tokenizer = TSXTokenizer()

    def _jsx_expression(self, token):
        # Handle nested braces in expressions
        if token == "{":
            self.brace_count += 1
        elif token == "}":
            self.brace_count -= 1
            if self.brace_count == 0:
                # We've found the matching closing brace
                self.state = self._after_tag
                return

        # Handle arrow functions in JSX attributes
        if token == "=>":
            self.arrow_function_detected = True
            # Explicitly yield the arrow token to ensure it's processed
            return ["=>"]

        # Handle type annotations in JSX attributes
        if token == "<":
            # This might be a TypeScript generic type annotation
            # We'll continue in the current state and let the tokenizer handle it
            pass

    def _body(self, token):
        if token == "<":
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            self.cache.pop()
            return self.flush()

        if token.startswith("</"):
            self.stop()
            return self.flush()

        if token == '{':
            self.sub_tokenizer = TSXTokenizer()
            return self.flush()


def isidentifier(token):
    try:
        return token.isidentifier()
    except AttributeError:
        return token.encode(encoding='UTF-8')[0].isalpha()
