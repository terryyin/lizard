"""Tokenize Perl's slash operators and quote-like literals."""

import re
from itertools import repeat
from typing import NamedTuple

from .token_pattern import compiled_token_pattern


_REGEX_FLAGS = {
    '/': re.compile(r'[msixpodualngc]*'),
    'm': re.compile(r'[msixpodualngc]*'),
    'qr': re.compile(r'[msixpodualn]*'),
    's': re.compile(r'[msixpodualngcer]*'),
    'tr': re.compile(r'[cdsr]*'),
    'y': re.compile(r'[cdsr]*'),
}
_PAIRED_DELIMITERS = {'(': ')', '[': ']', '{': '}', '<': '>'}
_TWO_PART_QUOTES = frozenset(('s', 'tr', 'y'))
_SPACE = re.compile(r'\s*')
_COMMENT_SPACE = re.compile(r'\s+(?:#[^\n]*\s*)*')
_LITERAL = re.compile(r'.+', re.DOTALL)
_HEREDOC = re.compile(
    r"""<<(?P<indent>~?)(?:(?P<bare>\\?\w+)|[ \t]*(?P<quote>['"`])(?P<quoted>[^\n]*?)(?P=quote))""")
_READLINE = re.compile(r'<<>>|<(?:\$?[\w:]+)?>')
_FORMAT_SPACE = r'\s*(?:\#[^\n]*\n\s*)*'
_FORMAT_NAME = r"(?:::|')?(?:\w+(?:::|'))*\w+"
_FORMAT_HEADER = re.compile(
    rf"format(?:(?=[\s#]){_FORMAT_SPACE}{_FORMAT_NAME})?{_FORMAT_SPACE}=[ \t\r]*(?:\#[^\n]*)?\n")
_FORMAT_LINE = re.compile(r'[^\n]*')
_FORMAT_END = re.compile(r'\.[ \t\r]*')
_FORMAT_FIELD = re.compile(r'[@^]')
_FORMAT_STATEMENT_START = frozenset(('', ';', '{', '}', ':'))
_SIGILS = frozenset(('$', '@', '%', '&', '$#'))
NAME_PREFIXES = _SIGILS | frozenset(('->', '::'))
_TERM_PREFIX = frozenset((
    'if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach', 'when', 'given',
    'and', 'or', 'xor', 'not', 'return', 'split', 'grep', 'map', 'print', 'say',
    'die', 'warn', 'defined', 'length', 'scalar', 'eval', 'push', 'unshift',
))
_TERM_END = frozenset((')', ']', '}', '++', '--'))
_TOKEN_ADDITION = (
    r"|\#[^\n]*"
    r"|\$\#(?=\$|\w|\{)|\$\$(?=[\[\]])|\$(?:\^[A-Z]|[\[\]!@?/'\"`\\<>|&+.,;:\-=~*%])"
    r"|`(?:\\.|[^`\\])*`"
    r"|(?P<perl_quote><(?=<(?!=)|[>$\w])|//=?|/=?|(?:q[qwxr]?|tr|[msy])\b)"
    r"|(?P<perl_format>format\b)"
)


class FormatBoundary(str):
    """Opaque picture text next to a value expression or the closing dot."""

    def __new__(cls, text, after_value):
        token = super().__new__(cls, text)
        token.after_value = after_value
        return token


class FormatValue(FormatBoundary):
    """A picture followed by an executable value expression."""


class FormatEnd(FormatBoundary):
    """The closing dot of a format declaration."""


class _EvaluatedReplacement(NamedTuple):
    start: int
    stop: int
    quote_end: int


def perl_tokens(source, addition, token_class, documentation):
    pattern = compiled_token_pattern(
        r'|(?P<perl_documentation>' + documentation + ')' + addition + _TOKEN_ADDITION)
    return _tokens_in_span(source, pattern, token_class, (0, len(source)))


def _tokens_in_span(source, pattern, token_class, span, *, value_depth=None):
    previous = before_previous = ''
    stop = span[1]
    matches = pattern.finditer(source, *span)
    heredocs = []
    while (match := next(matches, None)) is not None:
        if match.lastgroup == 'perl_documentation':
            yield from repeat('\n', match[0].count('\n'))
            continue

        token = match[0]
        if token == '\n' and (heredocs or value_depth == 0):
            end = _skip_heredocs(source, match.end(), stop, heredocs)
            yield from repeat('\n', source.count('\n', match.start(), end))
            if value_depth == 0:
                return end
            matches = pattern.finditer(source, end, stop)
            heredocs.clear()
            continue

        if value_depth is not None:
            # Perl resumes picture scanning at a newline outside braces and brackets.
            value_depth += (token in ('{', '[')) - (token in ('}', ']'))

        if (match.lastgroup == 'perl_format' and previous in _FORMAT_STATEMENT_START
                and (header := _FORMAT_HEADER.match(source, match.start(), stop))):
            end = yield from _format_tokens(source, pattern, token_class, header)
            matches = pattern.finditer(source, end, stop)
            before_previous, previous = previous, ';'
            continue

        literal_end = None
        if match.lastgroup == 'perl_quote':
            literal_end = _quote_end(source, match, previous, before_previous, heredocs)
            if isinstance(literal_end, _EvaluatedReplacement):
                yield from _evaluated_tokens(source, pattern, token_class, match.start(), literal_end)
                matches = pattern.finditer(source, literal_end.quote_end, stop)
                before_previous, previous = previous, None
                continue
        if not (token.isspace() or token.startswith('#')):
            before_previous, previous = previous, token if literal_end is None else None
        if literal_end is not None and literal_end != match.end():
            # The regular expression only matched the opener. Resume after the literal.
            token = source[match.start():literal_end]
            matches = pattern.finditer(source, literal_end, stop)
            if token_class:
                match = _LITERAL.match(source, match.start(), literal_end)
        yield token_class(match) if token_class else token
    return stop


def _format_tokens(source, pattern, token_class, header):
    start, picture, stop = header.start(), header.end(), header.endpos
    after_value = False
    while picture < stop:
        end = _FORMAT_LINE.match(source, picture, stop).end()
        next_line = min(end + 1, stop)
        if _FORMAT_END.fullmatch(source, picture, end):
            yield _format_text(source, token_class, (start, end), FormatEnd, after_value=after_value)
            return end
        if source[picture] == '#':
            # A picture comment at the start of a span must stay out of the
            # Perl comment processor, which also handles Lizard directives.
            if start == picture:
                yield '\n'
                start = next_line
        elif _FORMAT_FIELD.search(source, picture, end):
            yield _format_text(source, token_class, (start, end), FormatValue, after_value=after_value)
            after_value = True
            yield '\n'
            next_line = yield from _tokens_in_span(source, pattern, token_class, (next_line, stop), value_depth=0)
            start = next_line
        picture = next_line
    if start < stop:
        yield _format_text(source, token_class, (start, stop))
    return stop


def _format_text(source, token_class, span, boundary=None, *, after_value=False):
    if token_class:
        return token_class(_LITERAL.match(source, *span))
    start, end = span
    text = source[start:end]
    return boundary(text, after_value) if boundary else text


def _evaluated_tokens(source, pattern, token_class, start, replacement):
    # Emit one token for the substitution itself, then lex only the /e code.
    if token_class:
        yield token_class(_LITERAL.match(source, start, replacement.start))
    else:
        yield source[start:replacement.start]
    yield from _tokens_in_span(source, pattern, token_class, (replacement.start, replacement.stop))


def _quote_end(source, match, previous, before_previous, heredocs):
    token = match[0]
    if token == '<':
        if _expects_term_after(previous, before_previous):
            readline = _READLINE.match(source, match.start(), match.endpos)
            if readline:
                return readline.end()
            return _heredoc_opener_end(source, match.start(), match.endpos, heredocs)
        return None
    if token.startswith('/'):
        return (_delimited_quote_end(source, '/', match.start(), match.endpos)
                if _expects_term_after(previous, before_previous) else None)
    return _quote_operator_end(source, match, previous)


def _heredoc_opener_end(source, start, stop, heredocs):
    heredoc = _HEREDOC.match(source, start, stop)
    if heredoc:
        label = heredoc['bare'].lstrip('\\') if heredoc['bare'] else heredoc['quoted']
        heredocs.append((label, bool(heredoc['indent'])))
        return heredoc.end()
    return None


def _quote_operator_end(source, match, previous):
    if previous in NAME_PREFIXES or previous in ('package', 'sub'):
        return None
    if match[0] == 's' and previous == '-' and source[match.start() - 1] == '-':
        return None  # The -s file test.

    stop = match.endpos
    start = _SPACE.match(source, match.end(), stop).end()
    delimiter = source[start:min(start + 1, stop)]
    if not delimiter or delimiter.isalnum() or delimiter in '_)]}':
        return None
    if source.startswith('=>', start) or (delimiter == '#' and start != match.end()):
        return None
    return _delimited_quote_end(source, match[0], start, stop)


def _expects_term_after(token, previous):
    if token is None:
        return False
    if not token:
        return True
    if token[0].isalnum() or token[0] == '_':
        return token in _TERM_PREFIX and previous not in NAME_PREFIXES
    return token not in _TERM_END and token[0] not in "\"'`" and not (
        token.startswith('$') and token not in _SIGILS)


def _delimited_quote_end(source, operator, start, stop):
    end = _skip_delimited(source, start, stop)
    if end is None:
        return None
    if operator in _TWO_PART_QUOTES:
        if source[start] in _PAIRED_DELIMITERS:
            # Whitespace before a hash makes it a comment. An adjacent
            # hash can still be the replacement delimiter.
            spacing = _COMMENT_SPACE.match(source, end, stop)
            replacement = spacing.end() if spacing else end
        else:
            replacement = end - 1
        end = _skip_delimited(source, replacement, stop)
        if end is None:
            return None

    flags = _REGEX_FLAGS.get(operator)
    quote_end = flags.match(source, end, stop).end() if flags else end
    if operator == 's' and 'e' in source[end:quote_end]:
        return _EvaluatedReplacement(replacement + 1, end - 1, quote_end)
    return quote_end


def _skip_delimited(source, start, stop):
    if start == stop:
        return None
    opening = source[start]
    closing = _PAIRED_DELIMITERS.get(opening, opening)
    depth = 1
    index = start + 1
    while index < stop:
        char = source[index]
        if char == closing:
            depth -= 1
            if depth == 0:
                return index + 1
        elif char == '\\':
            index += 1
        elif char == opening:
            depth += 1
        index += 1
    return None


def _skip_heredocs(source, start, stop, heredocs):
    for label, indented in heredocs:
        start = _skip_heredoc(source, start, stop, label, indented)
    return start


def _skip_heredoc(source, start, stop, label, indented):
    while start < stop:
        newline = source.find('\n', start, stop)
        end = stop if newline == -1 else newline
        line = source[start:end].rstrip('\r')
        start = min(end + 1, stop)
        if (line.lstrip(' \t') if indented else line) == label:
            break
    return start
