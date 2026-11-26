'''
Language parser for PHP
'''

import re
from functools import lru_cache
from itertools import compress, product

from .code_reader import CodeReader, MemberName, _STRUCTURE_KEYWORDS, _previous_code
from .clike import CCppCommentsMixin
from .php_states import MEMBER_ACCESS, PHPLanguageStates
from .token_pattern import compiled_token_pattern

_BEFORE_MEMBER_NAME = MEMBER_ACCESS | {'function'}
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


_WORD_OPERATORS = {
    ''.join(letters): operator
    for word, operator in (('and', '&&'), ('or', '||'))
    for letters in product(*zip(word, word.upper()))
}
_NAME_PREFIX = MEMBER_ACCESS | {'\\'}
_COMMENT_STARTS = ('//', '/*', '#')


def _adjacent_php_code(tokens, index, step):
    index += step
    while 0 <= index < len(tokens) and (tokens[index] == '\n' or tokens[index].startswith(_COMMENT_STARTS)):
        index += step
    return index


def _is_operator_name(tokens, index):
    if isinstance(tokens[index], MemberName):
        return True
    before = _adjacent_php_code(tokens, index, -1)
    if before >= 0 and tokens[before] in _NAME_PREFIX:
        return True
    after = _adjacent_php_code(tokens, index, 1)
    if after < len(tokens) and tokens[after] in ('=', ':', ';'):
        # A constant, named argument, or enum case can use a keyword.
        return True
    if before >= 0 and tokens[before] == '&':
        before = _adjacent_php_code(tokens, before, -1)
    if before >= 0 and tokens[before].lower() == 'function':
        # A member called `function` can be the left operand.
        prefix = _adjacent_php_code(tokens, before, -1)
        return prefix < 0 or tokens[prefix] not in _NAME_PREFIX
    return False


class _UnparsedString(str):
    """The lexer used the legacy boundary for a string with an uncertain end."""


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'foreach', 'while', 'catch', 'match'}
    _logical_operators = {'&&', '||'}
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
        comments = r"|(?://|\#)" + _LINE_COMMENT_END + r"|\?>"
        # An adjacent backslash can make a word part of a qualified name.
        qualified_word = rf"|(?P<qualified_word>(?ai:and|or))(?=\\[{_LABEL_START}])"
        pattern = compiled_token_pattern(addition + variables + qualified_word +
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
                            yield token_class(match) if token_class else _UnparsedString(match[0])
                        elif token_class:
                            yield token_class(_TOKEN_SPAN.match(source_code, match.start(), end))
                        else:
                            yield source_code[match.start():end]
                        pos = end
                        break
                    token = match[0]
                    if token == '?>':
                        current_pos = match.end()
                        pos = None
                        break
                    if token_class:
                        yield token_class(match)
                    elif group == 'qualified_word':
                        yield MemberName(token)
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
        keywords = _STRUCTURE_KEYWORDS | self.control_flow_keywords | self.case_keywords | _WORD_OPERATORS.keys()
        tokens = [token for token in tokens if not token.isspace() or token == '\n']
        fallback_start = None
        for index in compress(range(len(tokens)), map(keywords.__contains__, tokens)):
            token = tokens[index]
            if token in _WORD_OPERATORS:
                if fallback_start is None:
                    fallback_start = next((i for i, item in enumerate(tokens) if isinstance(item, _UnparsedString)),
                                          len(tokens))
                # Legacy token boundaries can expose template text as code.
                if index < fallback_start and not _is_operator_name(tokens, index):
                    tokens[index] = _WORD_OPERATORS[token]
                continue
            before = _previous_code(tokens, index, True)
            if before >= 0 and tokens[before] in _BEFORE_MEMBER_NAME:
                tokens[index] = MemberName(token)
        return tokens
