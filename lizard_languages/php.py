'''
Language parser for PHP
'''

import re
from functools import lru_cache
from itertools import compress

from .code_reader import CodeReader, MemberName, _STRUCTURE_KEYWORDS
from .clike import CCppCommentsMixin
from .php_states import MEMBER_ACCESS, PHPLanguageStates
from .token_pattern import compiled_token_pattern

_BEFORE_MEMBER_NAME = MEMBER_ACCESS | {'function'}
_DECLARATION_KEYWORDS = {'class', 'trait', 'use', 'function'}
_COMMENT_STARTS = ('//', '/*', '#')
_OPENING_TAG = re.compile(r"<\?(?:php)?")
_EXPLICIT_OPENING_TAG = r"<\?(?:=|(?i:php)(?=[ \t\r\n]|\Z))"
_TEMPLATE_OPENING_TAG = re.compile(_EXPLICIT_OPENING_TAG + r"|<\?")
_TOKEN_SPAN = re.compile(r".*", re.S)
_LABEL_START = r'a-zA-Z_\x80-\U0010ffff'
_LABEL_CHAR = _LABEL_START + '0-9'
_DOC_OPENER = (
    rf"""[bB]?<<<[ \t]*(?P<doc_quote>['"]?)(?P<doc_label>[{_LABEL_START}][{_LABEL_CHAR}]*)(?P=doc_quote)"""
    r"(?:\r\n|[\r\n])"
)
_INTERPOLATION = r"\{(?=\$)|\$\{"
_LINE_COMMENT_END = r"[^\r\n]*?(?=\?>|[\r\n]|\Z)"


def _quoted_prefix(quote, group):
    # Match ordinary strings entirely in the tokenizer. Stop at an
    # interpolation opener so the scanner can skip its PHP expression.
    text = rf"[^{quote}\\{{$]*"
    return rf"{quote}{text}(?:(?:\\.|\{{(?!\$)|\$(?!\{{)){text})*(?:{quote}|(?P<{group}>{_INTERPOLATION}))"


_STRING_TOKENS = '|'.join((
    r"[bB]?'[^'\\]*(?:\\.[^'\\]*)*'",
    r"[bB]?" + _quoted_prefix('"', 'double_quote'),
    _quoted_prefix('`', 'backtick'),
    _DOC_OPENER,
))
_INTERPOLATION_TOKENS = re.compile(
    r"/\*.*?\*/|(?://|\#(?!\[))" + _LINE_COMMENT_END + r"|\?>|[{}]|" + _STRING_TOKENS, re.S
)
_QUOTED_ENDS = {
    name: re.compile(rf"{quote}|{_INTERPOLATION}|\\.", re.S)
    for name, quote in (('double_quote', '"'), ('backtick', '`'))
}


@lru_cache(maxsize=128)
def _doc_end(label, interpolate):
    closing = rf"(?<=[\r\n])[ \t]*{re.escape(label)}(?![{_LABEL_CHAR}])"
    if interpolate:
        closing += rf"|{_INTERPOLATION}|\\[^\r\n]"
    return re.compile(closing)


def _string_end(source, opener):
    kind = opener.lastgroup
    pos = opener.end()
    if kind == 'doc_label':
        boundaries = _doc_end(opener['doc_label'], opener['doc_quote'] != "'")
    else:
        boundaries = _QUOTED_ENDS[kind]
        pos = _expression_end(source, pos)
    while pos is not None and (boundary := boundaries.search(source, pos)):
        pos = boundary.end()
        if boundary[0] in ('{', '${'):
            pos = _expression_end(source, pos)
        elif not boundary[0].startswith('\\'):
            return pos
    return None


def _expression_end(source, pos):
    depth = 1
    while True:
        for match in _INTERPOLATION_TOKENS.finditer(source, pos):
            if match.lastindex:
                pos = _string_end(source, match)
                if pos is None:
                    return None
                break
            token = match[0]
            if token == '{':
                depth += 1
            elif token == '}':
                depth -= 1
                if depth == 0:
                    return match.end()
            elif token == '?>':
                tag = _TEMPLATE_OPENING_TAG.search(source, match.end())
                # A bare tag depends on the runtime short_open_tag setting.
                if tag is None or tag[0] == '<?':
                    return None
                pos = tag.end()
                break
        else:
            return None


def _adjacent_php_code(tokens, index, step):
    index += step
    while 0 <= index < len(tokens) and (tokens[index] == '\n' or tokens[index].startswith(_COMMENT_STARTS)):
        index += step
    return index


class _AttributeStart(str):
    starts_attribute = True


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'foreach', 'while', 'catch', 'match'}
    _logical_operators = {'&&', '||'}  # PHP also has 'and', 'or' with different precedence
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    # Operators that contain '?' but are not the ternary operator
    _non_ternary_question_operators = r"|(?:\?\?=)|(?:\?\?)|(?:\?->)|(?:\?:)"

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        variables = r"|(?:\$\w+)"
        # PHP comparisons must not use the shared generic angle-bracket rule.
        operators = PHPReader._non_ternary_question_operators + r"|<<=|<=|<"
        # A closing tag ends a line comment, but not a string or block comment.
        comments = r"|(?P<attribute_start>\#)(?=\[)|(?://|\#)" + _LINE_COMMENT_END + r"|\?>"
        pattern = compiled_token_pattern(addition + variables +
                                         rf"""|(?=[bB'"`<])(?:{_STRING_TOKENS})""" + operators + comments)
        legacy_pattern = None
        current_pos = 0
        while tag := _OPENING_TAG.search(source_code, current_pos):
            if current_pos < tag.start():
                yield '"' + source_code[current_pos:tag.start()] + '"'
            pos = tag.end()
            while pos is not None:
                for match in pattern.finditer(source_code, pos):
                    group = match.lastgroup
                    if group in ('doc_label', 'double_quote', 'backtick'):
                        end = _string_end(source_code, match)
                        if end is None:
                            if legacy_pattern is None:
                                legacy_pattern = compiled_token_pattern(
                                    addition + variables + r"|(?:<<<(?P<quote>\w+).*?(?P=quote))"
                                    + operators + r"|(?://|\#)[^\n]*?(?=\?>|\n|\Z)|\?>")
                            match = legacy_pattern.match(source_code, match.start())
                            end = match.end()
                        elif token_class:
                            match = _TOKEN_SPAN.match(source_code, match.start(), end)
                        yield token_class(match) if token_class else source_code[match.start():end]
                        pos = end
                        break
                    token = match[0]
                    if token == '?>':
                        current_pos = match.end()
                        pos = None
                        break
                    if token_class:
                        yield token_class(match)
                    elif group == 'attribute_start':
                        yield _AttributeStart(token)
                    else:
                        yield token
                else:
                    return
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [PHPLanguageStates(context)]

    def preprocess(self, tokens):
        # `$promise->catch($f)` and `function catch()` name a member.
        tokens = self.mark_member_names(tokens, _BEFORE_MEMBER_NAME)
        keywords = _STRUCTURE_KEYWORDS | _DECLARATION_KEYWORDS | self.control_flow_keywords | self.case_keywords
        for index in compress(range(len(tokens)), map(keywords.__contains__, tokens)):
            before = _adjacent_php_code(tokens, index, -1)
            if before >= 0 and tokens[before] in ('(', ','):
                after = _adjacent_php_code(tokens, index, 1)
                if after < len(tokens) and tokens[after] == ':':
                    tokens[index] = MemberName(tokens[index])
        return tokens
