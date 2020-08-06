'''
Language parser for Go lang
'''

from .code_reader import CodeStateMachine


class GoLikeStates(CodeStateMachine):  # pylint: disable=R0903

    FUNC_KEYWORD = 'func'

    def _state_global(self, token):
        if token == self.FUNC_KEYWORD:
            self._state = self._function_name
            self.context.push_new_function('')
        elif token in '{':
            self.sub_state(self.statemachine_clone())
        elif token in '}':
            self.statemachine_return()

    def _function_name(self, token):
        if token != '`':
            if token == '(':
                return self.next(self._member_function, token)
            if token == '{':
                return self.next(self._expect_function_impl, token)
            self.context.add_to_function_name(token)
            self._state = self._expect_function_dec

    def _expect_function_dec(self, token):
        if token == '(':
            self.next(self._function_dec, token)
        elif token == "<":
            self.next(self._generalize, token)
        else:
            self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("<>", "_expect_function_dec")
    def _generalize(self, tokens):
        pass

    @CodeStateMachine.read_inside_brackets_then("()", '_function_name')
    def _member_function(self, tokens):
        self.context.add_to_long_function_name(tokens)

    @CodeStateMachine.read_inside_brackets_then("()", '_expect_function_impl')
    def _function_dec(self, token):
        if token not in '()':
            self.context.parameter(token)

    def _expect_function_impl(self, token):
        self.next_if(self._function_impl, token, '{')

    def _function_impl(self, _):
        def callback():
            self._state = self._state_global
            self.context.end_of_function()
        self.sub_state(self.statemachine_clone(), callback)
