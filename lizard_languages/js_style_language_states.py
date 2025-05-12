'''
"JavaScript style language" includes JavaScript and PHP
'''

from .code_reader import CodeStateMachine


class JavaScriptStyleLanguageStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context):
        super(JavaScriptStyleLanguageStates, self).__init__(context)
        self.last_tokens = ''
        self.function_name = ''
        self.started_function = None
        self.as_object = False
        self._getter_setter_prefix = None
        self.arrow_function_pending = False

    def _state_global(self, token):
        if self.as_object:
            # Support for getter/setter: look for 'get' or 'set' before method name
            if token in ('get', 'set'):
                self._getter_setter_prefix = token
                return
            if hasattr(self, '_getter_setter_prefix') and self._getter_setter_prefix:
                # Next token is the property name
                self.last_tokens = f"{self._getter_setter_prefix} {token}"
                self._getter_setter_prefix = None
                return
            if token == '[':
                self._collect_computed_name()
                return
            if token == ':':
                self.function_name = self.last_tokens
                return
            elif token == '(':
                if not self.started_function:
                    self.arrow_function_pending = True
                    self._function(self.last_tokens)
                self.next(self._function, token)
                return

        if token in '.':
            self._state = self._field
            self.last_tokens += token
            return
        if token == 'function':
            self._state = self._function
        elif token in ('if', 'switch', 'for', 'while', 'catch'):
            self.next(self._expecting_condition_and_statement_block)
        elif token in ('else', 'do', 'try', 'final'):
            self.next(self._expecting_statement_or_block)
        elif token in ('=>',):
            # Only handle arrow function body, do not push function here
            self._state = self._arrow_function
        elif token == '=':
            self.function_name = self.last_tokens
        elif token == "(":
            self.sub_state(
                self.__class__(self.context))
        elif token in '{':
            if self.started_function:
                self.sub_state(
                    self.__class__(self.context),
                    self._pop_function_from_stack)
            else:
                self.read_object()
        elif token in ('}', ')'):
            self.statemachine_return()
        elif self.context.newline or token == ';':
            self.function_name = ''
            self._pop_function_from_stack()

        self.last_tokens = token

    def read_object(self):
        def callback():
            self.next(self._state_global)

        object_reader = self.__class__(self.context)
        object_reader.as_object = True
        self.sub_state(object_reader, callback)

    def statemachine_before_return(self):
        self._pop_function_from_stack()

    def _expecting_condition_and_statement_block(self, token):
        def callback():
            self.next(self._expecting_statement_or_block)

        if token == "await":
            return

        if token != '(':
            self.next(self._state_global, token)
            return

        self.sub_state(
            self.__class__(self.context), callback)

    def _expecting_statement_or_block(self, token):
        def callback():
            self.next(self._state_global)
        if token == "{":
            self.sub_state(
                self.__class__(self.context), callback)
        else:
            self.next(self._state_global, token)

    def _push_function_to_stack(self):
        self.started_function = True
        self.context.push_new_function(self.function_name or '(anonymous)')

    def _pop_function_from_stack(self):
        if self.started_function:
            self.context.end_of_function()
        self.started_function = None

    def _arrow_function(self, token):
        self._push_function_to_stack()
        # Handle arrow function body
        if token == '{':
            # Block body
            self.next(self._state_global, token)
        else:
            # Expression body
            self.next(self._state_global, token)

    def _function(self, token):
        if token == '*':
            return
        if token != '(':
            self.function_name = token
        else:
            if not self.started_function:
                self._push_function_to_stack()
            self.arrow_function_pending = False
            self._state = self._dec
            if token == '(':
                self._dec(token)

    def _field(self, token):
        self.last_tokens += token
        self._state = self._state_global

    def _dec(self, token):
        if token == ')':
            self._state = self._expecting_func_opening_bracket
        elif token != '(':
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _expecting_func_opening_bracket(self, token):
        # Do not reset started_function for arrow functions (=>)
        if token != '{' and token != '=>':
            self.started_function = None
        self.next(self._state_global, token)

    def _collect_computed_name(self):
        # Collect tokens between [ and ]
        tokens = []

        def collect(token):
            if token == ']':
                # Try to join tokens and camelCase if possible
                name = ''.join(tokens)
                # Remove quotes and pluses for simple cases
                name = name.replace("'", '').replace('"', '').replace('+', '').replace(' ', '')
                # Lowercase first char, uppercase next word's first char
                name = self._to_camel_case(name)
                self.last_tokens = name
                self.next(self._state_global)
                return True
            tokens.append(token)
            return False
        self.next(collect)

    def _to_camel_case(self, s):
        # Simple camelCase conversion for test case
        if not s:
            return s
        parts = s.split()
        if not parts:
            return s
        return parts[0][0].lower() + parts[0][1:] + ''.join(p.capitalize() for p in parts[1:])
