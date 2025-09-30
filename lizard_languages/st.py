'''
Language parser for Structured Text.
'''

import re
# import itertools
from .code_reader import CodeStateMachine, CodeReader


class StCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("(*") or token.startswith("//"):
            return token[2:]


class StReader(CodeReader, StCommentsMixin):
    ''' This is the reader for Structured Text. '''

    ext = ["st"]
    language_names = ['st']
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    # track block starters
    _conditions = set([
        'if', 'elsif', 'case', 'for', 'while', 'repeat',
        'IF', 'ELSIF', 'CASE', 'FOR', 'WHILE', 'REPEAT'
    ])

    _functions = set([
        'FUNCTION_BLOCK', 'FUNCTION', 'ACTION'
    ])

    _blocks = set([
        'IF', 'FOR', 'WHILE', 'CASE', 'REPEAT',
    ])

    _ends = set([
        'END',
    ])

    # Nesting Depth
    loops = [
        'if', 'case', 'for', 'while', 'repeat',
        'IF', 'CASE', 'FOR', 'WHILE', 'REPEAT'
    ]
    bracket = 'END'

    def __init__(self, context):
        super(StReader, self).__init__(context)
        self.parallel_states = (
            StStates(context, self),
        )

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):

        # Capture everything until end of logical line, where lines may be continued with \ at the end.”
        _until_end = r'(?:\\\n|[^\n])*'
        block_endings = '|'.join(f'END_{_}' for _ in StReader._blocks)
        addition = (
            r'(?i)'  # case-insensitive
            r'//' + _until_end + r'|'    # line comment
            r'\(\*' + _until_end + r'|'  # block comment  (* ... *)
            r'OR|'
            r'AND|'
            r'XOR|'
            r'NOT|'
            r'ELSE\s+IF|'
            + block_endings + addition
        )

        return CodeReader.generate_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        """Handle compiler pragmas like #IF, #INCLUDE, etc."""
        for token in tokens:
            macro = self.macro_pattern.match(token)
            if macro:
                directive = macro.group(1).lower()
                if directive in ("if", "ifdef", "ifndef", "elif"):
                    self.context.add_condition()
                elif directive == "include":
                    yield "#include"
                    yield macro.group(2) or "\"\""
                for _ in macro.group(2).splitlines()[1:]:
                    yield "\n"
            else:
                # ST normalization: collapse END_* into END
                upper_tok = token.upper()
                if upper_tok.startswith("END_"):
                    yield "END"
                    continue

                # Eliminate whitespace, keep line breaks
                if not token.isspace() or token == '\n':
                    yield token


class StStates(CodeStateMachine):
    """Track Structured Text State."""

    def __init__(self, context, reader):
        super().__init__(context)
        self.reader = reader
        self.last_token = None

    def __call__(self, token, reader=None):
        if self._state(token):
            self.next(self.saved_state)
            if self.callback:
                self.callback()
        self.last_token = token
        if self.to_exit:
            return True

    def _state_global(self, token):
        token_upper = token.upper()

        if token_upper in StReader._functions and self.context.current_function.top_nesting_level < 0:
            self._state = self._function_name
        elif token_upper in StReader._blocks:
            self.context.add_bare_nesting()
        elif token in StReader._ends:
            self.context.pop_nesting()

    def reset_state(self, token=None):
        self._state = self._state_global
        if token is not None:
            self._state_global(token)

    def _function_name(self, token):
        self.context.restart_new_function(token)
        self._state = self._function

    def _function(self, token):
        self.context.add_bare_nesting()
        self.reset_state(token)
