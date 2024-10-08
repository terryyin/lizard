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
        elif token == 'type':
            self._state = self._type_definition
        elif token in '{':
            self.sub_state(self.statemachine_clone())
        elif token in '}':
            self.statemachine_return()

    def _type_definition(self, token):
        self._state = self._after_type_name

    def _after_type_name(self, token):
        if token == 'struct':
            self._state = self._struct_definition
        elif token == 'interface':
            self._state = self._interface_definition
        else:
            self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("{}", "_state_global")
    def _struct_definition(self, tokens):
        pass

    @CodeStateMachine.read_inside_brackets_then("{}", "_state_global")
    def _interface_definition(self, tokens):
        pass

    def _function_name(self, token):
        if token != '`':
            if token == '(':
                if len(self.context.stacked_functions) > 0\
                        and self.context.stacked_functions[-1].name != '*global*':
                    return self.next(self._function_dec, token)
                else:
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
        if token == '{' and self.last_token != 'interface':
            self.next(self._function_impl, token)

    def _function_impl(self, _):
        def callback():
            self._state = self._state_global
            self.context.end_of_function()
        self.sub_state(self.statemachine_clone(), callback)
