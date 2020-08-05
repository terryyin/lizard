'''
Language parser for Ruby-like langauges
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


def state_embedded_doc(token):
    if token == "=end":
        return True


class RubylikeStateMachine(CodeStateMachine):
    # pylint: disable=R0903
    FUNC_KEYWORD = 'def'

    def __init__(self, context):
        super(RubylikeStateMachine, self).__init__(context)

    def _state_global(self, token):
        if token in ("end", "}"):
            self.statemachine_return()
        elif token == self.FUNC_KEYWORD:
            self._state = self._def
            self.next(self._def)
        elif token == 'it':
            self._state = self._it
        elif token in ("begin", "do", "class", "module", "{", "${"
                       ) and self.last_token != ".":
            self.sub_state(self.statemachine_clone())
        elif token in ("while", "for"):
            if self.is_newline():
                self.next(self._for_while)
        elif token in ("if", "unless"):
            if self.is_newline():
                self.sub_state(self.statemachine_clone())
            else:
                self.next(self._if)
        elif token == "=begin":
            self.sub_state(state_embedded_doc)

    def is_newline(self):
        return self.context.newline or self.last_token == ";"

    def _def(self, token):
        if token == '(':
            self.context.push_new_function('(anonymous)')
            self.next(self._def_parameters)
            return
        self.context.push_new_function(token)
        self.next(self._def_continue)

    def _it(self, token):
        if token in ('do', '{'):
            self.context.push_new_function(self.last_token)
            self.next(self._def_continue)

    def _def_continue(self, token):
        def callback():
            self.context.end_of_function()
            self.next(self._state_global)
        if token == ".":
            self.context.add_to_function_name(token)
            self.next(self._def_class_method)
        elif token == "(":
            self.next(self._def_parameters)
        else:
            self.sub_state(self.statemachine_clone(), callback, token)

    def _def_class_method(self, token):
        self.context.add_to_function_name(token)
        self.next(self._def_continue)

    def _def_parameters(self, token):
        if token == ')':
            self.next(self._def_continue)
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _if(self, token):
        if self.is_newline():
            self.next(self._state_global, token)
        elif token == "then":
            self.next(self._state_global)
            self.sub_state(self.statemachine_clone())

    def _for_while(self, token):
        if self.is_newline() or token == "do":
            self.next(self._state_global)
            if token != "end":
                self.sub_state(self.statemachine_clone())


class RubylikeReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    _conditions = set(['if', 'until', 'for', 'while', 'and', 'or',
                       'elsif', 'elseif', 'rescue',
                       'ensure', 'when', '||', '&&', '?'])

    def __init__(self, context):
        super(RubylikeReader, self).__init__(context)
        self.parallel_states = [RubylikeStateMachine(context)]
