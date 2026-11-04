"""F# tokens from Pygments' lexer, with fragments and split operators
joined, `#` directive lines dropped and each line's first code token
marked with its column."""

from pygments import lex, lexers
import pygments.token as py_token
from .pygments_fragments import join_fragments

OPERATOR_CHARS = frozenset('!%&*+-./:<=>?@^|~')


def is_operator(token):
    '''Whether the token is made of operator characters only.'''
    return bool(token) and all(char in OPERATOR_CHARS for char in token)


class LineStart(str):
    '''The first code token of a line, at `column`.'''

    column = 0

    def __new__(cls, value, column):
        token = super().__new__(cls, value)
        token.column = column
        return token


def generate_tokens(source_code, is_comment):
    '''`is_comment(token)` tells comments, which start no line.'''
    lexer = lexers.get_lexer_by_name('fsharp', stripnl=False)
    return _mark_line_starts(join_fragments(
        lex(source_code, lexer=lexer),
        joins=lambda _run, value: is_operator(value),
        comment_types=(py_token.String.Doc,),
        keep_spaces=True), is_comment)


def _mark_line_starts(tokens, is_comment):
    column, at_line_start, directive = 0, True, False
    for token in tokens:
        if token == '\n':
            column, at_line_start, directive = 0, True, False
            yield token
        elif directive:
            continue
        elif token.isspace():
            column += len(token)
        else:
            if at_line_start and not is_comment(token):
                if token == '#':
                    directive = True
                    continue
                token, at_line_start = LineStart(token, column), False
            if '\n' in token:
                column = len(token) - token.rfind('\n') - 1
            else:
                column += len(token)
            yield token
