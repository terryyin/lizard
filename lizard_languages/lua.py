'''
Language parser for Lua
'''

from .rubylike import RubylikeReader, RubylikeStateMachine


class LuaReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['lua']
    language_names = ['lua']

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return RubylikeReader.generate_tokens(
            source_code,
            r"|\-\-\[\[.*?\]\]" +
            r"|\[\=*\[.*?\]\=*\]" +
            r"|\-\-.*?$" +
            addition)

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
        super(LuaStateMachine, self)._state_global(token)

    def _for_while(self, token):
        # A Lua loop always has `do`.  The reader only misses it when `#`,
        # the length operator, starts a comment: `for i = 1, #t do`.  Then
        # it skips the first token of the body, as it did before.
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
