'''
Language parser for Lua
'''

from .rubylike import RubylikeReader, RubylikeStateMachine
from .token_pattern import compiled_token_pattern, tokens_in_span


class LuaReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['lua']
    language_names = ['lua']
    loops = frozenset(('if', 'for', 'while'))

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

    def use_reader_nesting(self, metric):
        self.parallel_states[0].nesting_metrics += (metric,)


class LuaStateMachine(RubylikeStateMachine):
    FUNC_KEYWORD = 'function'
    PARAMETERS_ON_DEF_LINE = False
    # Lua has no `case`, and its `until` ends a `repeat` loop.
    BLOCK_KEYWORDS = ("do", "{", "${")
    LOOP_KEYWORDS = ("while", "for")

    # Only tokens that can change scope need the shared Ruby state.
    _STATE_TOKENS = frozenset(BLOCK_KEYWORDS + LOOP_KEYWORDS + (
        FUNC_KEYWORD, "end", "}", "if", "=begin"))

    def __init__(self, context, nesting_metrics=()):
        super(LuaStateMachine, self).__init__(context)
        self.probable_function_name = None
        self.nesting_metrics = nesting_metrics
        self.nd_depth = self.ns_depth = 0

    def statemachine_clone(self):
        clone = LuaStateMachine(self.context, self.nesting_metrics)
        clone.nd_depth, clone.ns_depth = self.nd_depth, self.ns_depth
        return clone

    def _state_global(self, token):
        if token == "=":
            self.next(self._assigning)
            return
        self.probable_function_name = token
        if token in self._STATE_TOKENS:
            if token == "do":
                self._open_control_body(counts_for_nd=False)
            elif token == "if" and not self.follows_value():
                self._open_control_body()
            else:
                RubylikeStateMachine._state_global(self, token)

    def _if(self, token):
        if token == "then" and not self.is_newline():
            self.next(self._state_global)
            self._open_control_body()
        else:
            super()._if(token)

    def _for_while(self, token):
        if token == "do":
            self.next(self._state_global)
            self._open_control_body()
        elif token == self.FUNC_KEYWORD:
            name = self.probable_function_name if self.last_token == "=" else None
            self.sub_state(LuaLoopHeaderFunction(
                self.context, name, self.nesting_metrics))
        elif token == "=":
            self.probable_function_name = self.last_token

    def _open_control_body(self, counts_for_nd=True):
        # A standalone do scopes statements but adds only a structure.
        body = self.statemachine_clone()
        function = self.context.current_function
        if counts_for_nd and "max_nesting_depth" in self.nesting_metrics:
            body.nd_depth += 1
            function.max_nesting_depth = max(function.max_nesting_depth, body.nd_depth)
        if "max_nested_structures" in self.nesting_metrics:
            body.ns_depth += 1
            function.max_nested_structures = max(function.max_nested_structures, body.ns_depth)
        self.sub_state(body)

    def _def_body(self, token):
        body = LuaStateMachine(self.context, self.nesting_metrics)
        self.sub_state(body, self._end_def, token)

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

    def __init__(self, context, name, nesting_metrics):
        super().__init__(context, nesting_metrics)
        self.probable_function_name = name
        self.next(self._anonymous_def if name is not None else self._def)

    def _end_def(self):
        self.context.end_of_function()
        self.statemachine_return()
