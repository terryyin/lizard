'''
Language parser for Apple Swift
'''

import re

from .clike import CCppCommentsMixin
from .code_reader import CodeReader
from .kotlin_states import KotlinStates
from .swift import SwiftReplaceLabel


# Raw strings may end in extra quotes: `"""a""""` holds `a"`.
_RAW_STRING = r'""".*?""""*'
_QUOTED_NAME = r"`[^`\r\n]+`"
_KOTLIN_TRIVIA = re.compile(
    r'//[^\n]*|' + _RAW_STRING + r'|"(?:\\.|[^"\\])*"|'
    r"'(?:\\.|[^'\\])*'|" + _QUOTED_NAME + r"|/\*", re.S)
_COMMENT_DELIMITER = re.compile(r'/\*|\*/')


def _flatten_nested_comments(source):
    """Preserve positions while hiding inner closers from the shared lexer."""
    if '/*' not in source:
        return source
    pieces = []
    start = 0
    offset = 0
    while True:
        match = _KOTLIN_TRIVIA.search(source, offset)
        if match is None:
            break
        offset = match.end()
        if match.group() != '/*':
            continue
        depth = 1
        for delimiter in _COMMENT_DELIMITER.finditer(source, match.end()):
            depth += 1 if delimiter.group() == '/*' else -1
            if depth == 0:
                end = delimiter.end()
                pieces.append(source[start:match.end()])
                pieces.append(source[match.end():end - 2].replace('*/', '* '))
                pieces.append('*/')
                start = end
                offset = end
                break
        else:
            break
    pieces.append(source[start:])
    return ''.join(pieces)


class KotlinReader(CodeReader, CCppCommentsMixin, SwiftReplaceLabel):
    # pylint: disable=R0903

    ext = ['kt', 'kts']
    language_names = ['kotlin']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Kotlin uses 'when' expressions, not case
    _ternary_operators = {'?:'}  # Elvis operator
    # `flow.catch { }`, `Promise::catch`, `fun catch()` and
    # `fun <T> catch()` name a member, also across a line break.
    _BEFORE_MEMBER_NAME = frozenset(('.', '::', 'fun'))
    _MEMBER_ACCESS_SPANS_LINES = True
    _TYPE_PARAMETERS_BEFORE_NAME = True

    def __init__(self, context):
        super(KotlinReader, self).__init__(context)
        self.parallel_states = [KotlinStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CodeReader.generate_tokens(
            _flatten_nested_comments(source_code),
            "|" + _RAW_STRING +
            "|" + _QUOTED_NAME +
            r"|\w+\?" +
            r"|\w+\!!" +
            r"|\?\?" +
            r"|\?:" +
            addition
        )
