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
        # Lua can start with a hash line for the interpreter. Elsewhere,
        # a hash is the length operator.
        if source_code.startswith(("#", "\ufeff#")):
            addition = r"|(?:\A|(?<=\A\ufeff))#[^\n]*" + addition
        lua_patterns = r"|--(?:\[\[.*?\]\]|[^\n]+)|\[\[.*?\]\]"
        if addition or "[=" in source_code:
            lua_patterns = (
                r"|(?:--\[=|\[=)(?P<lua_level>=*)\[.*?\]=(?P=lua_level)\]" +
                lua_patterns)
        pattern = compiled_token_pattern(
            lua_patterns +
            r"|\-\-.*?$" +
            r"|//" +
            addition)
        return tokens_in_span(
            pattern, source_code, 0, len(source_code), token_class, macros=False)

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
    BLOCK_KEYWORDS = ("do", "{", "${")
    LOOP_KEYWORDS = ("while", "for")

    # Only tokens that can change scope need the shared Ruby state.
    _STATE_TOKENS = frozenset(BLOCK_KEYWORDS + LOOP_KEYWORDS + (
        FUNC_KEYWORD, "end", "}", "if", "=begin"))

    def __init__(self, context):
        super(LuaStateMachine, self).__init__(context)
        self.probable_function_name = None

    def _state_global(self, token):
        if token == "=":
            self.next(self._assigning)
            return
        self.probable_function_name = token
        if token in self._STATE_TOKENS:
            RubylikeStateMachine._state_global(self, token)

    def _for_while(self, token):
        if token == "do":
            self.next(self._state_global)
            self.sub_state(self.statemachine_clone())
        elif token == self.FUNC_KEYWORD:
            name = self.probable_function_name if self.last_token == "=" else None
            self.sub_state(LuaLoopHeaderFunction(self.context, name))
        elif token == "=":
            self.probable_function_name = self.last_token

    def _assigning(self, token):
        if token == self.FUNC_KEYWORD:
            self._state = self._anonymous_def
            return
        # The caller records this token after the state runs.
        self._state = self._state_global
        self._state(token)

    def _anonymous_def(self, token):
        if token != '(':
            self.next(self._def, token)
            return
        self.context.push_new_function(self.probable_function_name)
        self.next(self._def_parameters)


class LuaLoopHeaderFunction(LuaStateMachine):
    """Read one function and return to its loop header."""

    def __init__(self, context, name):
        super().__init__(context)
        self.probable_function_name = name
        self.next(self._anonymous_def if name is not None else self._def)

    def statemachine_clone(self):
        return LuaStateMachine(self.context)

    def _end_def(self):
        self.context.end_of_function()
        self.statemachine_return()
