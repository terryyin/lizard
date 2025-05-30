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

    def __init__(self, context):
        super().__init__(context)
        self.ts_states = TypeScriptStates(context)
        self.in_variable_declaration = False
        self._pending_variable_name = None  # Track variable name through type annotation
        self.last_tokens = self.ts_states.last_tokens
        self.function_name = self.ts_states.function_name
        self.started_function = self.ts_states.started_function
        self.context = context
        self._state = self._state_global

    def statemachine_before_return(self):
        # Ensure the main function is closed at the end
        if self.ts_states.started_function:
            self.ts_states._pop_function_from_stack()
            # After popping, if current_function is not *global*, pop again to add to function_list
            if self.context.current_function and self.context.current_function.name != "*global*":
                self.context.end_of_function()

    def _state_global(self, token):
        # Handle variable declarations
        if token in ('const', 'let', 'var'):
            self.in_variable_declaration = True
            self.ts_states._state_global(token)
            self._sync_from_ts()
            return

        if self.in_variable_declaration:
            if token == '=':
                # Save the variable name when we see the assignment (only if not already set by type annotation)
                if not self._pending_variable_name:
                    self._pending_variable_name = self.ts_states.last_tokens.strip()
                self.ts_states._state_global(token)
                self._sync_from_ts()
                return
            elif token == ':':
                # Type annotation after variable name
                self._pending_variable_name = self.ts_states.last_tokens.strip()
                self.ts_states._state_global(token)
                self._sync_from_ts()
                return
            elif token == '=>':
                # Arrow function with variable assignment (with or without type annotation)
                if self._pending_variable_name and not self.ts_states.started_function:
                    self.ts_states.function_name = self._pending_variable_name
                    self.ts_states._push_function_to_stack()
                    self.in_variable_declaration = False
                    self._pending_variable_name = None
                    self._state = self._arrow_function
                    self._sync_from_ts()
                    return
            elif token == ';' or self.context.newline:
                self.in_variable_declaration = False
                self._pending_variable_name = None

        # Handle arrow function in JSX/TSX prop context
        if token == '=>' and not self.in_variable_declaration:
            if not self.ts_states.started_function:
                self.ts_states.function_name = '(anonymous)'
                self.ts_states._push_function_to_stack()
                self._sync_from_ts()
                return

        # Pop anonymous function after closing '}' in TSX/JSX prop
        if token == '}' and self.ts_states.started_function and self.ts_states.function_name == '(anonymous)':
            self.ts_states._pop_function_from_stack()
            self._sync_from_ts()

        # Continue with regular TypeScript state handling
        self.ts_states._state_global(token)
        self._sync_from_ts()

    def _arrow_function(self, token):
        self._state = self._state_global
        self._state(token)

    def next(self, state, *args, **kwargs):
        # Set the next state for this state machine
        self._state = state
        if args or kwargs:
            self._state(*args, **kwargs)

    def sub_state(self, submachine, callback=None, *args, **kwargs):
        # Delegate to TypeScriptStates' sub_state
        return self.ts_states.sub_state(submachine, callback, *args, **kwargs)

    def statemachine_return(self):
        return self.ts_states.statemachine_return()

    def _sync_from_ts(self):
        # Keep key attributes in sync for compatibility
        self.last_tokens = self.ts_states.last_tokens
        self.function_name = self.ts_states.function_name
        self.started_function = self.ts_states.started_function


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
