"""Shared JavaScript and TypeScript runtime state dispatch."""

from .code_reader import CodeStateMachine
from .javascript_bindings import JavaScriptBindingsMixin
from .javascript_functions import JavaScriptFunctionsMixin
from .javascript_objects import JavaScriptObjectsMixin
from .typescript_declarations import TypeScriptDeclarationsMixin


class TypeScriptStates(JavaScriptBindingsMixin, JavaScriptFunctionsMixin,
                       JavaScriptObjectsMixin, TypeScriptDeclarationsMixin,
                       CodeStateMachine):
    def __init__(self, context):
        super().__init__(context)
        self.last_tokens = ''
        self.function_name = ''
        self.started_function = None
        self.as_object = False
        self._getter_setter_prefix = None

        self._ts_declare = False  # Track if 'declare' was seen
        self._static_seen = False  # Track if 'static' was seen
        self._async_seen = False  # Track if 'async' was seen
        self._prev_token = ''  # Track previous token to detect method calls
        self._in_prop_value = False  # Track if inside property value (after ':')
        self._in_abstract_context = False  # Track abstract method declarations
        self._condition_kind = None

    def __call__(self, token, reader=None):
        if self._binding_candidate:
            self._binding_candidate(token)
        return super().__call__(token, reader)

    def _replay_token(self, token):
        """Process a token saved by a nested syntax reader through dispatch."""
        return self(token)

    def statemachine_before_return(self):
        # Ensure the main function is closed at the end
        if self.started_function:
            self._pop_function_from_stack()

    def _state_global(self, token):
        self._resolve_pending_bindings(token)

        if self._handle_type_declaration(token):
            return

        # Track static and async modifiers
        if token == 'static':
            self._static_seen = True
            self._prev_token = token
            return
        if token == 'async':
            self._async_seen = True
            self._prev_token = token
            return
        if token == 'new':
            # Track 'new' keyword to avoid treating constructors as functions
            self._prev_token = token
            return

        if self._handle_object_member(token):
            return

        if token == '.':
            self._state = self._field
            self.last_tokens += token
            self._prev_token = token
            return
        if token == 'function':
            if self.started_function and not self.as_object:
                self._pop_function_from_stack()
            self._state = self._function
        elif token in ('if', 'switch', 'for', 'while', 'catch'):
            self._condition_kind = token
            self.next(self._expecting_condition_and_statement_block)
        elif token in ('else', 'do', 'try', 'final'):
            self.next(self._expecting_statement_or_block)
        elif token in ('=>',):
            self._state = self._arrow_function
        elif token == '=':
            # Only set function_name for valid identifiers
            name = self.last_tokens
            if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                self.function_name = name
        elif token == "(":
            # Check if this is a method call or constructor
            if self._prev_token == '.' or self._prev_token == 'new':
                # This is a method call or constructor, not a function definition
                self.sub_state(
                    self.__class__(self.context))
            elif (self._binding_candidate
                  and self._binding_candidate.in_expression):
                self._read_binding_candidate(')')
            elif self.function_name:
                # Distinguish arrow-function definition from function call:
                #   const fn = (...) => {}   <- _prev_token is '=' or 'async'
                #   const fn = someFunc(...)  <- _prev_token is an identifier
                # In the second case, ( follows an identifier that differs
                # from function_name, so it's a call — not a definition.
                if (self.last_tokens != self.function_name
                        and self._prev_token not in ('=', 'async', '>')):
                    self.function_name = ''
                    self.sub_state(self.__class__(self.context))
                else:
                    if not self.started_function:
                        self._function(self.function_name)
                    self.next(self._function, token)
            else:
                self._read_binding_candidate(')')
        elif token == '[' and self._prev_token not in ('.', ')', ']', '}'):
            previous = self._prev_token
            operand = (previous and (previous[0].isalnum()
                                     or previous[0] in '_$#\'"`'))
            if (not operand or previous in
                    ('const', 'let', 'var', 'return', 'throw', 'yield', 'await')):
                self._read_binding_candidate(']')
        elif token == '{':
            if self.started_function:
                self.sub_state(
                    self.__class__(self.context),
                    self._pop_function_from_stack)
            else:
                self.read_object()
        elif token in ('}', ')') or (
                token == ']' and self._binding_candidate):
            self.statemachine_return()
        elif self.context.newline or token == ';':
            self.function_name = ''
            self._pop_function_from_stack()
            # Reset modifiers on newline/semicolon
            self._static_seen = False
            self._async_seen = False
            self._in_abstract_context = False
            self._in_prop_value = False
            self._prev_token = ''

        if token == '`':
            self.next(self._state_template_literal)
        if not self.as_object:
            if token == ':':
                self._consume_type_annotation()
                self._prev_token = token
                return
        if self.as_object and token == ',':
            self._in_prop_value = False
        self.last_tokens = token
        # Don't overwrite _prev_token if it's 'new' or '.' (preserve for next token)
        if self._prev_token not in ('new', '.'):
            self._prev_token = token

    def _expecting_condition_and_statement_block(self, token):
        def callback():
            self.next(self._expecting_statement_or_block)

        if token == "await":
            return

        if token != '(':
            self.next(self._state_global, token)
            return

        if self._condition_kind == 'catch':
            def catch_callback(summary):
                self.context.add_condition(summary.defaults)
                callback()

            self._read_binding_candidate(')', catch_callback, track_complexity=False)
            return

        self.sub_state(
            self.__class__(self.context), callback)

    def _expecting_statement_or_block(self, token):
        def callback():
            self._prev_token = ''
            self.next(self._state_global)
        if token == "{":
            self.sub_state(
                self.__class__(self.context), callback)
        else:
            self.next(self._state_global, token)

    def _field(self, token):
        self.last_tokens += token
        self._state = self._state_global

    def _state_template_literal(self, token):
        if token == '`':
            self.next(self._state_global)
