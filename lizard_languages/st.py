'''
Language parser for Structured Text.
'''

import re
import itertools
from .code_reader import CodeStateMachine, CodeReader


class StCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("(*") or token.startswith("//"):
            return token[2:]


# pylint: disable=R0903
class StReader(CodeReader, StCommentsMixin):
    ''' This is the reader for Structured Text. '''

    ext = ["st", "typ", "var"]
    language_names = ['st']
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __init__(self, context):
        super(StReader, self).__init__(context)
        self.parallel_states = (
            StNestingStates(context),
            StDeclarationStates(context)
        )

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Add pattern for floating point literals to the token generation
        addition = r"|(?:\d*\.\d+(?:[eE][-+]?\d+)?)" + \
                  r"|(?:\d+\.(?:\d+)?(?:[eE][-+]?\d+)?)" + \
                  addition
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        """Handle compiler pragmas like #IF, #INCLUDE, etc."""
        for token in tokens:
            macro = self.macro_pattern.match(token)
            if macro:
                directive = macro.group(1).lower()
                if directive in ("if", "ifdef", "elif"):
                    self.context.add_condition()
                elif directive == "include":
                    yield "#include"
                    yield macro.group(2) or "\"\""
                for _ in macro.group(2).splitlines()[1:]:
                    yield "\n"
            else:
                yield token


class StNestingStates(CodeStateMachine):
    """Track nesting levels in Structured Text."""

    def _state_global(self, token: str):
        low = token.lower()

        # Control flow structures
        if low in ("if", "case", "for", "while", "repeat"):
            self.context.add_bare_nesting()
        elif low in ("end_if", "end_case", "end_for", "end_while", "until", "end_repeat"):
            self.context.pop_nesting()

        # Actions
        elif low == "action":
            self.context.add_bare_nesting()
        elif low == "end_action":
            self.context.pop_nesting()

        # Program and function blocks
        elif low in ("program", "function", "function_block"):
            self.context.add_bare_nesting()
        elif low in ("end_program", "end_function", "end_function_block"):
            self.context.pop_nesting()


class StDeclarationStates(CodeStateMachine):
    """Track VAR ... END_VAR blocks for declarations."""

    def _state_global(self, token: str):
        low = token.lower()
        if low == "var":
            self._state = self._in_var_block
            self.context.add_bare_nesting()
        elif low == "end_var":
            self.context.pop_nesting()
            self._state = self._state_global

    def _in_var_block(self, token: str):
        low = token.lower()
        if low == "end_var":
            self.context.pop_nesting()
            self._state = self._state_global
        else:
            # Treat as declaration inside VAR block
            self.context.parameter(token)
