'''
"JavaScript style language" includes JavaScript and PHP
'''

from .code_reader import CodeStateMachine


class JavaScriptStyleLanguageStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context):
        super(JavaScriptStyleLanguageStates, self).__init__(context)
        # start from one, so global level will never count
        self.last_tokens = ''
        self.function_name = ''
        self.saved_function = None

    def _state_global(self, token):
        if token == 'function':
            self._state = self._function
        elif token in ('=>',):
            self._state = self._arrow_function
        elif token in ('=', ':'):
            self.function_name = self.last_tokens
        elif token in '.':
            self._state = self._field
            self.last_tokens += token
        else:
            if token in '{(':
                self.sub_state(
                    JavaScriptStyleLanguageStates(self.context),
                    self._pop_function_from_stack)
            elif token in ('}', ')', '*EOF*'):
                self.sm_return()
            elif self.context.newline or token == ';':
                self._pop_function_from_stack()

            self.last_tokens = token

    def before_return(self):
        self._pop_function_from_stack()

    def _push_function_to_stack(self):
        self.saved_function = self.context.current_function
        self.context.start_new_function(self.function_name or '(anonymous)')
        self.function_name = ''

    def _pop_function_from_stack(self):
        self.function_name = ''
        if self.saved_function:
            self.context.end_of_function()
        self._pop_function_and_discard()

    def _pop_function_and_discard(self):
        if self.saved_function:
            self.context.current_function = self.saved_function
            self.saved_function = None

    def _arrow_function(self, token):
        self._push_function_to_stack()
        self.next(self._state_global, token)

    def _function(self, token):
        if token != '(':
            self.function_name = token
        else:
            self._push_function_to_stack()
            self._state = self._dec
            if token == '(':
                self._dec(token)

    def _field(self, token):
        self.last_tokens += token
        self._state = self._state_global

    def _dec(self, token):
        if token == ')':
            self._state = self._state_expecting_function_opening_bracket
        elif token != '(':
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _state_expecting_function_opening_bracket(self, token):
        if token != '{':
            self._pop_function_and_discard()
        self.next(self._state_global, token)
