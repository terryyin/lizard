"""
Language parser for erlang
"""

import re
from lizard_languages.code_reader import CodeReader, CodeStateMachine
import pygments.token as py_token
from pygments import lex, lexers


class ErlangReader(CodeReader):
    # pylint: disable=R0903

    ext = ['erl', 'hrl', 'es', 'escript']
    language_names = ['erlang']
    _conditions = {'and', 'case', 'catch', 'if', 'not', 'or', '?', 'when'}

    def __init__(self, context):
        super(ErlangReader, self).__init__(context)
        self.parallel_states = [ErlangStates(context)]

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("%%"):
            return token[2:]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        lexer = lexers.get_lexer_by_name('erlang')
        tokens = lex(source_code, lexer=lexer)
        return map(
            lambda x: x[1],
            filter(lambda x: x[0] != py_token.Whitespace, tokens)
        )


class ErlangStates(CodeStateMachine):
    # pylint: disable=R0903

    func_name_pattern = re.compile("[a-zA-Z]+[a-zA-Z0-9_]*")

    def __init__(self, context):
        super(ErlangStates, self).__init__(context)
        self.punctuated = False

    def _state_global(self, token):
        if token == '-':
            self.punctuated = True
        elif self.func_name_pattern.match(token) and not self.punctuated:
            self.context.push_new_function(token)
            self._state = self._state_after_name
        elif token == 'end':
            self._state = self._state_nested_end
        elif token == '.' or token == ';':
            self.statemachine_return()
        else:
            self.punctuated = False

    def _state_after_name(self, token):
        if token == '(':
            self._state = self._state_start_of_params
            self.context.add_to_long_function_name(token)
            self.lbr = 1
            self.rbr = 0
        else:
            self.func_match_failed(token)

    def _state_start_of_params(self, token):
        if token == ')':
            self.rbr += 1
            if self.lbr == self.rbr:
                self._state = self._state_end_of_params
                self.context.add_to_long_function_name(" " + token)
                self.punctuated = False
                return
        if token == '(':
            self.lbr += 1
        self.context.parameter(token)

    def _state_end_of_params(self, token):
        if token == '-':
            self.punctuated = True
        elif token == '>' and self.punctuated:
            if (len(self.context.stacked_functions) <= 1 or
                    self.context.current_function.name == 'fun'):
                self.next(self._state_func_first_line, token)
        else:
            self.func_match_failed(token)

    def _state_func_first_line(self, _):
        def callback():
            self._state = self._state_global
            self.context.end_of_function()

        self.sub_state(self.statemachine_clone(), callback)
        self.punctuated = False

    def _state_nested_end(self, token):
        if token == '.' or token == ',':
            if (len(self.context.stacked_functions) > 1 and
                    self.context.stacked_functions[-1].name == 'fun'):
                self.statemachine_return()
                return

        self._state = self._state_global

    def func_match_failed(self, token):
        self.punctuated = False
        self._state = self._state_global
        curr_cyc_comp = self.context.current_function.cyclomatic_complexity - 1
        if self.context.stacked_functions:
            self.context.current_function = self.context.stacked_functions.pop()
        else:
            self.context.current_function = self.context.global_pseudo_function
        self.context.add_condition(curr_cyc_comp)
        self.next(self._state_global, token)
