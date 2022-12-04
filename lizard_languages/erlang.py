"""
Language parser for erlang
"""

import re
from lizard_languages.code_reader import CodeReader, CodeStateMachine
import pygments.token as py_token
from pygments import lex, lexers

func_name_pattern = re.compile("[a-zA-Z]+[a-zA-Z0-9_]*")


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
        return map(lambda x: x[1], filter(lambda x: x[0] != py_token.Whitespace,
                                          lex(source_code, lexer=lexers.get_lexer_by_name('erlang'))))

    @staticmethod
    def preprocess(tokens):
        return tokens


class ErlangStates(CodeStateMachine):
    # pylint: disable=R0903

    def __init__(self, context):
        super(ErlangStates, self).__init__(context)
        self.punctuated = False

    def _state_global(self, token):
        if token == '-':
            self.punctuated = True
        elif func_name_pattern.match(token) and not self.punctuated:
            self._state = self._function
            self.context.restart_new_function(token)
            self.context.add_to_long_function_name("(")
        else:
            self.punctuated = False

    def _function(self, token):
        if token == '(':
            self._state = self._dec

    def _dec(self, token):
        if token == ')':
            self._state = self._state_colon
            self.punctuated = False
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _state_colon(self, token):
        if token == '-':
            self.punctuated = True
        elif token == '>' and self.punctuated:
            self.next(self._state_first_line)
        else:
            self.punctuated = False
            self._state = self._state_global

    def _state_first_line(self, token):
        self._state = self._state_global
        self._state_global(token)
        self.context.end_of_function()






# ---------------------------------------------------


    def _state_global(self, token):
        if token == '-':
            self.punctuated = True
        elif func_name_pattern.match(token) and not self.punctuated:
            self.context.restart_new_function(token)
        elif token == '(':
            self._state = self._function
            self.context.add_to_long_function_name(token)

    def _dec(self, token):
        if token == ')':
            self._state = self._state_colon
            self.context.add_to_long_function_name(" " + token)
            self.punctuated = False
        else:
            self.context.parameter(token)








        # else:
        #     self.punctuated = False
        #
        #
        # if token == self.FUNC_KEYWORD:
        #     self._state = self._function_name
        #     self.context.push_new_function('')
        # elif token in '{':
        #     self.sub_state(self.statemachine_clone())
        # elif token in '}':
        #     self.statemachine_return()

    def _function_name(self, token):
        if token != '`':
            if token == '(':
                if len(self.context.stacked_functions) > 0 \
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
