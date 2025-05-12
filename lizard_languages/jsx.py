'''
Language parser for JSX
'''

from .javascript import JavaScriptReader
from .typescript import JSTokenizer, Tokenizer, TypeScriptStates
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates


class JSXTypeScriptStates(TypeScriptStates):
    """State machine for JSX/TSX files extending TypeScriptStates"""

    def __init__(self, context):
        super().__init__(context)
        # Initialize attributes that might be accessed later
        self._parent_function_name = None
        self.in_variable_declaration = False
        self.last_variable_name = None

    def statemachine_before_return(self):
        # Ensure the main function is closed at the end
        if self.started_function:
            self._pop_function_from_stack()
            # After popping, if current_function is not *global*, pop again to add to function_list
            if self.context.current_function and self.context.current_function.name != "*global*":
                self.context.end_of_function()

    def _state_global(self, token):
        # Handle variable declarations
        if token in ('const', 'let', 'var'):
            self.in_variable_declaration = True
            super()._state_global(token)
            return

        if self.in_variable_declaration:
            if token == '=':
                # Save the variable name when we see the assignment
                self.last_variable_name = self.last_tokens.strip()
                super()._state_global(token)
                return
            elif token == '=>':
                # We're in an arrow function with a variable assignment
                if self.last_variable_name and not self.started_function:
                    self.function_name = self.last_variable_name
                    self._push_function_to_stack()
                    self.in_variable_declaration = False
                    # Switch to arrow function state to handle the body
                    self._state = self._arrow_function
                    return
            elif token == ';' or self.context.newline:
                self.in_variable_declaration = False

        # Handle arrow function in JSX/TSX prop context
        if token == '=>' and not self.in_variable_declaration:
            if not self.started_function:
                self.function_name = '(anonymous)'
                self._push_function_to_stack()
                return

        if not self.as_object:
            if token == ':':
                self._consume_type_annotation()
                return

        # Pop anonymous function after closing '}' in TSX/JSX prop
        if token == '}' and self.started_function and self.function_name == '(anonymous)':
            self._pop_function_from_stack()

        # Continue with regular TypeScript state handling
        super()._state_global(token)

    def _arrow_function(self, token):
        self.next(self._state_global, token)


class TSXTokenizer(JSTokenizer):
    def __init__(self):
        super().__init__()

    def process_token(self, token):
        if token == "<":
            from .jsx import XMLTagWithAttrTokenizer  # Import only when needed
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            return

        if token == "=>":
            # Special handling for arrow functions
            yield token
            return

        for tok in super().process_token(token):
            yield tok


class JSXMixin:
    '''Base mixin class for JSX/TSX shared functionality'''
    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
            r"|(?:\$\w+)" + \
            r"|(?:\<\/\w+\>)" + \
            r"|(?:=>)" + \
            r"|`.*?`"
        js_tokenizer = TSXTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok

    def _expecting_func_opening_bracket(self, token):
        if token == '<':
            self.next(self._expecting_jsx)
            return
        if token == '=>':
            # Handle arrow functions in JSX attributes
            self._handle_arrow_function()
            return
        super()._expecting_func_opening_bracket(token)

    def _handle_arrow_function(self):
        # Process arrow function in JSX context
        self.context.add_to_long_function_name(" => ")

        # Store the current function
        current_function = self.context.current_function

        # Create a new anonymous function
        self.context.restart_new_function('(anonymous)')

        # Set up for the arrow function body
        def callback():
            # Return to the original function when done
            self.context.current_function = current_function

        self.sub_state(self.__class__(self.context), callback)

    def _expecting_arrow_function_body(self, token):
        if token == '{':
            # Arrow function with block body
            self.next(self._function_body)
        else:
            # Arrow function with expression body
            self.next(self._expecting_func_opening_bracket)

    def _function_body(self, token):
        if token == '}':
            # End of arrow function body
            self.context.end_of_function()
            self.next(self._expecting_func_opening_bracket)

    def _expecting_jsx(self, token):
        if token == '>':
            self.next(self._expecting_func_opening_bracket)


class JSXJavaScriptStyleLanguageStates(JavaScriptStyleLanguageStates):
    def __init__(self, context):
        super(JSXJavaScriptStyleLanguageStates, self).__init__(context)

    def _state_global(self, token):
        # Handle variable declarations
        if token in ('const', 'let', 'var'):
            # Remember that we're in a variable declaration
            self.in_variable_declaration = True
            super()._state_global(token)
            return

        if hasattr(self, 'in_variable_declaration') and self.in_variable_declaration:
            if token == '=':
                # We're in a variable assignment
                self.function_name = self.last_tokens.strip()
                super()._state_global(token)
                return
            elif token == '=>':
                # We're in an arrow function with a variable assignment
                if not self.started_function and self.function_name:
                    self._push_function_to_stack()
                self._state = self._arrow_function
                return
            elif token == ';' or self.context.newline:
                # End of variable declaration
                self.in_variable_declaration = False

        super()._state_global(token)

    def _expecting_func_opening_bracket(self, token):
        if token == ':':
            # Handle type annotations like TypeScript does
            self._consume_type_annotation()
            return
        super()._expecting_func_opening_bracket(token)

    def _consume_type_annotation(self):
        # Skip over type annotations (simplified version of TypeScript's behavior)
        def skip_until_terminator(token):
            if token in ['{', '=', ';', ')', '(', '=>']:
                self.next(self._state_global, token)
                return True
            return False

        self.next(skip_until_terminator)


class JSXReader(JavaScriptReader, JSXMixin):
    # pylint: disable=R0903

    ext = ['jsx']
    language_names = ['jsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Add support for JSX syntax patterns
        addition = addition + \
            r"|(?:<[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:<\/[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)"
        return JSXMixin.generate_tokens(source_code, addition, token_class)

    def __init__(self, context):
        super(JSXReader, self).__init__(context)
        # Use our JSXTypeScriptStates for better handling of JSX
        self.parallel_states = [JSXTypeScriptStates(context)]


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
