'''
Language parser for JavaScript
'''

from lizard import CodeReader
from .script_language import ScriptLanguageMixIn
from .js_style_regex_expression import js_style_regex_expression


class SyntaxMachine(object):
    # pylint: disable=R0903
    def __init__(self, context):
        self.context = context
        self.saved_state = self._state = self._state_global
        self.last_token = None
        self.to_exit = False
        self.callback = None

    def next(self, state, token=None):
        self._state = state
        if token is not None:
            self._state(token)

    def sm_return(self):
        self.to_exit = True

    def sub_state(self, state, callback=None):
        self.saved_state = self._state
        self.callback = callback
        self.next(state)

    def __call__(self, token):
        if self._state(token):
            self.next(self.saved_state)
            if self.callback:
                self.callback()
        self.last_token = token
        if self.to_exit:
            return True

    def _state_global(self, token):
        pass


def state_embedded_doc(token):
    if token == "=end":
        return True


class RubyStateMachine(SyntaxMachine):
    # pylint: disable=R0903
    def is_newline(self):
        return self.context.newline or self.last_token == ";"

    def _state_global(self, token):
        if token == "end":
            self.sm_return()
        elif token == 'def':
            self._state = self._function
            self.next(self._function)
        elif token in ("begin", "do"):
            self.sub_state(RubyStateMachine(self.context))
        elif token in ("while", "for"):
            if self.is_newline():
                self.next(self._for_while)
        elif token in ("if", "unless"):
            if self.is_newline():
                self.sub_state(RubyStateMachine(self.context))
            else:
                self.next(self._if)
        elif token == "=begin":
            self.sub_state(state_embedded_doc)

    def _function(self, token):
        def callback():
            self.context.end_of_function()
            self.next(self._state_global)
        self.context.start_new_function(token)
        self.sub_state(RubyStateMachine(self.context), callback)

    def _if(self, token):
        if self.is_newline():
            self.next(self._state_global, token)
        elif token == "then":
            self.next(self._state_global)
            self.sub_state(RubyStateMachine(self.context))

    def _for_while(self, token):
        if self.is_newline() or token == "do":
            self.next(self._state_global)
            if token != "end":
                self.sub_state(RubyStateMachine(self.context))


class RubyReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']
    conditions = set(['if', 'until', 'for', 'while', 'and', 'or',
                      'elsif', 'rescue', 'ensure', 'when', '||', '&&', '?'])

    def __init__(self, context):
        super(RubyReader, self).__init__(context)
        self._state = RubyStateMachine(context)

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, _=''):
        return CodeReader.generate_tokens(
            source_code,
            r"|^\=begin|^\=end" +
            r"|\%[qQr]?\{(?:\\.|[^\}\\])*?\}" +
            r"|\%[qQr]?\[(?:\\.|[^\]\\])*?\]" +
            r"|\%[qQr]?\<(?:\\.|[^\>\\])*?\>" +
            _)
