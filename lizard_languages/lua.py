'''
Language parser for Lua
'''

from .rubylike import RubylikeReader, RubylikeStateMachine
from .token_pattern import compiled_token_pattern, tokens_in_span


class LuaReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['lua']
    language_names = ['lua']

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        pattern = compiled_token_pattern(
            r"|\-\-\[\[.*?\]\]" +
            r"|\[\=*\[.*?\]\=*\]" +
            r"|\-\-.*?$" +
            r"|//" +
            addition)
        return tokens_in_span(pattern, source_code, 0, len(source_code),
                              token_class, macros=False)

    def get_comment_from_token(self, token):
        if token.startswith("--"):
            return token

    def __init__(self, context):
        super(LuaReader, self).__init__(context)
        self.parallel_states = [LuaStateMachine(context)]


class LuaStateMachine(RubylikeStateMachine):
    FUNC_KEYWORD = 'function'
    PARAMETERS_ON_DEF_LINE = False
    # Lua has no `case`, and its `until` ends a `repeat` loop.
    BLOCK_KEYWORDS = ("begin", "do", "class", "module", "{", "${")
    LOOP_KEYWORDS = ("while", "for")

    def __init__(self, context):
        super(LuaStateMachine, self).__init__(context)
        self.probable_function_name = None

    def _state_global(self, token):
        if token == "=":
            self.next(self._assigning)
            return
        self.probable_function_name = token
        # Lua's ordinary identifier `it` cannot open a Ruby RSpec example.
        if token != 'it':
            super(LuaStateMachine, self)._state_global(token)

    def _for_while(self, token):
        # A Lua loop header ends at `do`; retain recovery at a line break.
        if self.is_newline() or (
                token == "do" and
                not getattr(token, 'is_member_name', False)):
            self.next(self._state_global)
            if token != "end":
                self.sub_state(self.statemachine_clone())

    def _assigning(self, token):
        if token == self.FUNC_KEYWORD:
            self._state = self._anonymous_def
            return
        self.next(self._state_global, token)

    def _anonymous_def(self, token):
        if token != '(':
            self.next(self._def, token)
            return
        self.context.push_new_function(self.probable_function_name)
        self.next(self._def_parameters)
