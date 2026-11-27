'''
Language parser for Lua
'''

import re

from .rubylike import RubylikeReader, RubylikeStateMachine
from .token_pattern import compiled_token_pattern, tokens_in_span


class LuaReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['lua']
    language_names = ['lua']

    _control_flow_keywords = {'if', 'elseif', 'until', 'for', 'while'}

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        lua_patterns = r"|--(?:\[\[.*?\]\]|[^\n]+)|\[\[.*?\]\]"
        if "[=" in source_code:
            lua_patterns = _leveled_brackets(source_code, addition) + lua_patterns
        # Lua can start with a hash line for the interpreter. Elsewhere,
        # a hash is the length operator.
        if source_code.startswith(("#", "\ufeff#")):
            addition = r"|(?:\A|(?<=\A\ufeff))\#[^\n]*" + addition
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

    def _def_continue(self, token):
        if token == ":" and self.last_token != ")":
            self.context.add_to_function_name(token)
            self.next(self._def_class_method)
        else:
            super()._def_continue(token)

    def _def_parameters(self, token):
        if token != ")":
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)
        self.next(self._def_body)

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


def _leveled_brackets(source_code, addition):
    if not addition:
        return r"|(?:--\[=|\[=)(?P<lua_level>=*)\[.*?\]=(?P=lua_level)\]"
    # Use literal levels so Lua's pattern cannot renumber caller captures.
    levels = sorted(set(re.findall(r"(?=\[(=+)\[)", source_code)))
    return "".join(rf"|(?:--)?\[{level}\[.*?\]{level}\]" for level in levels)
