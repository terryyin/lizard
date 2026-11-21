'''
Language parser for Ruby
'''

import re

from .rubylike import RubylikeReader
from .js_style_regex_expression import js_style_regex_expression
from .script_language import ScriptLanguageMixIn


class MyToken(str):
    def __new__(cls, value, *_):
        val = value.group(0) if hasattr(value, 'group') else value
        return super(MyToken, cls).__new__(cls, val)

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start() if hasattr(value, 'start') else 0


class _MethodName(str):
    """A keyword that names a method, `m.end(0)` or `def self.for(user)`:
    neither the reader nor a metric reads it as a structure."""

    is_member_name = True


# The words that the reader or a metric reads as a structure.
_KEYWORDS = frozenset((
    'begin', 'end', 'do', 'then', 'def', 'class', 'module', 'it',
    'if', 'unless', 'else', 'elsif', 'elseif', 'elif', 'case', 'when',
    'while', 'until', 'for', 'foreach', 'and', 'or', 'rescue', 'ensure',
    'try', 'catch', 'finally', 'except', 'switch', 'with', 'return',
    'yield'))
_NAME_BEFORE = frozenset(('.', 'def'))


def _delimited(opening, closing, one_line):
    '''A % literal from its opening to its closing delimiter. A pair of
    the same delimiters nests, as in `%w(a (b) c)` and `%r{#{a}}`. Real code
    nests at most two levels deep; this allows three. Another delimiter,
    as in `%w|a b|`, ends the literal on its line, so a `%` in a regex that
    the reader misses, such as `/^%w/`, cannot run far. Only a `%r` regex
    with such a delimiter can span lines, as an `x` flag regex often does.'''
    opening, closing = re.escape(opening), re.escape(closing)
    if opening == closing:
        line_end = r"\n" if one_line else ""
        return (opening + r"(?:\\.|[^" + closing + r"\\" + line_end +
                r"])*" + closing)
    body = r"(?:\\.|[^" + opening + closing + r"\\])*"
    for _ in range(3):
        body = (r"(?:\\.|[^" + opening + closing + r"\\]|" +
                opening + body + closing + r")*")
    return opening + body + closing


def _any_delimited(one_line):
    return "(?:" + "|".join(
        _delimited(pair[0], pair[-1], one_line)
        for pair in ('{}', '()', '[]', '<>', '!', '|', '/', '"', "'")) + ")"


_PERCENT_LITERAL = (r"\%(?:[qQwWiIsx]?" + _any_delimited(True) +
                    r"|r" + _any_delimited(False) + r"[a-z]*)")


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

    @staticmethod
    def preprocess(tokens):
        """Drop spaces, and mark a keyword right after '.', '&.' or 'def'
        on the same line as the name of a method."""
        keywords, name_before = _KEYWORDS, _NAME_BEFORE
        name_follows = False
        for token in tokens:
            if token.isspace() and token != '\n':
                continue
            if name_follows and token in keywords:
                name_follows = False
                yield _MethodName(token)
                continue
            name_follows = token in name_before
            yield token

    @staticmethod
    @js_style_regex_expression(language='ruby')
    def generate_tokens(source_code, addition='', token_class=None):
        def process_source(source, _, matcher):
            return ScriptLanguageMixIn.generate_common_tokens(
                source,
                r"|^\=begin|^\=end" +
                r"|\w+:" +
                r"|\$\w+" +
                r"|\.+" +
                r"|:?\@{0,2}\w+\??\!?" +
                r"|" + _PERCENT_LITERAL +
                _, matcher)
        matcher = MyToken
        bracket_stack = []
        source = source_code
        while source is not None:
            for token in process_source(source, addition, matcher):
                if token == "{":
                    bracket_stack.append("{")
                elif token == "}":
                    if bracket_stack:
                        if bracket_stack.pop() == '#{':
                            source = '"'+source[token.begin + 1:]
                            yield token
                            break
                elif token.startswith('"'):
                    first, sep, _ = token.partition('#{')
                    if sep:
                        yield first + '"'
                        yield '${'  # because #will be regarded as comments
                        bracket_stack.append(sep)
                        source = source[token.begin + token.find(sep)+2:]
                        break
                yield token
            else:
                source = None
