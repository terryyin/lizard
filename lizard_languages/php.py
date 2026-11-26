'''
Language parser for PHP
'''

import re
from functools import lru_cache
from itertools import compress, product

from .code_reader import CodeReader, MemberName, _STRUCTURE_KEYWORDS
from .clike import CCppCommentsMixin
from .php_states import MEMBER_ACCESS, PHPLanguageStates
from .token_pattern import compiled_token_pattern

_BEFORE_MEMBER_NAME = MEMBER_ACCESS | {'function'}
_DECLARATION_KEYWORDS = {'class', 'trait', 'use', 'function'}
_OPENING_TAG = re.compile(r"<\?(?:php)?")
_EXPLICIT_OPENING_TAG = r"<\?(?:=|(?i:php)(?=[ \t\r\n]|\Z))"
_EXPLICIT_TAG = re.compile(_EXPLICIT_OPENING_TAG)
_TEMPLATE_OPENING_TAG = re.compile(_EXPLICIT_OPENING_TAG + r"|<\?")
_TOKEN_SPAN = re.compile(r".*", re.S)
_LABEL_START = r'a-zA-Z_\x80-\U0010ffff'
_LABEL_CHAR = _LABEL_START + '0-9'
_PHP_NAME_CHAR = re.compile(rf"[{_LABEL_CHAR}]")
_WORD_BOUNDARY = rf"(?![{_LABEL_CHAR}]|\\[{_LABEL_START}])"
_DECIMAL_DIGITS = r"[0-9](?:_?[0-9])*"
_NUMBER = (
    r"0[xX][0-9a-fA-F](?:_?[0-9a-fA-F])*|0[bB][01](?:_?[01])*|0[oO][0-7](?:_?[0-7])*"
    rf"|(?:{_DECIMAL_DIGITS}(?:\.(?:{_DECIMAL_DIGITS})?)?|\.{_DECIMAL_DIGITS})(?:[eE][+-]?{_DECIMAL_DIGITS})?"
)
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


def _string_end(source, opener, short_tags=None):
    kind = opener.lastgroup
    pos = opener.end()
    if kind == 'doc_label':
        boundaries = _doc_end(opener['doc_label'], opener['doc_quote'] != "'")
    else:
        boundaries = _QUOTED_ENDS[kind]
        pos = _expression_end(source, pos, short_tags)
    while pos is not None and (boundary := boundaries.search(source, pos)):
        pos = boundary.end()
        if boundary[0] in ('{', '${'):
            pos = _expression_end(source, pos, short_tags)
        elif not boundary[0].startswith('\\'):
            return pos
    return None


def _expression_end(source, pos, short_tags):
    depth = 1
    while True:
        for match in _INTERPOLATION_TOKENS.finditer(source, pos):
            if match.lastindex:
                pos = _string_end(source, match, short_tags)
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
                tags = _EXPLICIT_TAG if short_tags is False else _TEMPLATE_OPENING_TAG
                tag = tags.search(source, match.end())
                # A bare tag depends on the runtime short_open_tag setting.
                if tag is None or (short_tags is None and tag[0] == '<?'):
                    return None
                pos = tag.end()
                break
        else:
            return None


class _WordOperator(str):
    """A short-circuit word with a symbolic spelling for metrics."""

    is_word_operator = True


_WORD_OPERATORS = {
    ''.join(letters): _WordOperator(operator)
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


class _WordCandidate(str):
    """A word whose source boundaries allow it to be an operator."""


class _OpeningGroup(str):
    """An opening parenthesis with its preceding PHP code token."""

    def __new__(cls, previous):
        token = super().__new__(cls, '(')
        token.php_leading_token = previous
        return token


class _AttributeStart(str):
    starts_attribute = True


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
        comments = r"|(?P<attribute_start>\#)(?=\[)|(?://|\#)" + _LINE_COMMENT_END + r"|\?>"
        word = rf"|(?P<operator_word>(?ai:and|or)){_WORD_BOUNDARY}"
        compact_number = (rf"|(?<![{_LABEL_CHAR}])(?P<compact_number>{_NUMBER})"
                          rf"(?=(?ai:and|or){_WORD_BOUNDARY})")
        suffix = rf"""|(?=[bB'"`<])(?:{_STRING_TOKENS})""" + operators + comments
        word_pattern = compiled_token_pattern(addition + variables + word + compact_number + suffix)
        plain_pattern = compiled_token_pattern(addition + variables + suffix)
        legacy_pattern = None
        uncertain_until = 0
        number_end = -1
        current_pos = 0
        while tag := _OPENING_TAG.search(source_code, current_pos):
            if current_pos < tag.start():
                yield '"' + source_code[current_pos:tag.start()] + '"'
            # A bare opening tag can be an XML processing instruction.
            pattern = word_pattern if _EXPLICIT_TAG.match(source_code, tag.start()) else plain_pattern
            pos = tag.end()
            while pos is not None:
                for match in pattern.finditer(source_code, pos):
                    group = match.lastgroup
                    if group in ('doc_label', 'double_quote', 'backtick'):
                        end = _string_end(source_code, match)
                        if end is None:
                            if match.start() >= uncertain_until:
                                # Keep legacy token boundaries. Classify words again
                                # after both possible interpretations of the string.
                                ends = (_string_end(source_code, match, mode) for mode in (False, True))
                                uncertain_until = max((end for end in ends if end is not None),
                                                      default=len(source_code))
                            if legacy_pattern is None:
                                legacy_pattern = compiled_token_pattern(
                                    addition + variables + r"|(?:<<<(?P<quote>\w+).*?(?P=quote))"
                                    + operators + r"|(?://|\#)[^\n]*?(?=\?>|\n|\Z)|\?>")
                            match = legacy_pattern.match(source_code, match.start())
                            end = match.end()
                            yield token_class(match) if token_class else match[0]
                        elif token_class:
                            yield token_class(_TOKEN_SPAN.match(source_code, match.start(), end))
                        else:
                            yield source_code[match.start():end]
                        pos = end
                        break
                    if group == 'compact_number':
                        if match.start() < uncertain_until:
                            match = plain_pattern.match(source_code, match.start())
                            yield token_class(match) if token_class else match[0]
                            pos = match.end()
                            break
                        number_end = match.end()
                    token = match[0]
                    if token == '?>':
                        current_pos = match.end()
                        pos = None
                        break
                    if token_class:
                        yield token_class(match)
                    elif group == 'attribute_start':
                        yield _AttributeStart(token)
                    elif group == 'operator_word' and match.start() >= uncertain_until:
                        start = match.start()
                        # A numeric literal can touch an operator. A PHP
                        # identifier, including its non-ASCII marks, cannot.
                        if start == number_end or not _PHP_NAME_CHAR.match(source_code, start - 1):
                            token = _WordCandidate(token)
                        yield token
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
        keywords = (_STRUCTURE_KEYWORDS | _DECLARATION_KEYWORDS | self.control_flow_keywords
                    | self.case_keywords | _WORD_OPERATORS.keys() | {'('})
        tokens = [token for token in tokens if not token.isspace() or token == '\n']
        for index in compress(range(len(tokens)), map(keywords.__contains__, tokens)):
            token = tokens[index]
            if token == '(':
                before = _adjacent_php_code(tokens, index, -1)
                if before >= 0:
                    tokens[index] = _OpeningGroup(tokens[before])
                continue
            if token in _WORD_OPERATORS:
                if isinstance(token, _WordCandidate) and not _is_operator_name(tokens, index):
                    tokens[index] = _WORD_OPERATORS[token]
                continue
            before = _adjacent_php_code(tokens, index, -1)
            if before < 0:
                continue
            prefix = tokens[before]
            if prefix in _BEFORE_MEMBER_NAME:
                tokens[index] = MemberName(tokens[index])
            elif prefix in ('(', ','):
                after = _adjacent_php_code(tokens, index, 1)
                if after < len(tokens) and tokens[after] == ':':
                    tokens[index] = MemberName(tokens[index])
        return tokens
