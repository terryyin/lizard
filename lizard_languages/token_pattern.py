"""Shared source tokenizer.

The pattern is compiled once for each addition. A reader that splits a file
into spans reuses that pattern instead of compiling it for every span.
"""

import re
from collections import deque
from functools import reduce
from operator import or_


_COMPILED_PATTERNS = {}
# A type argument list holding a `?` (`<?>`, `<? extends T>`, `Map<K, ? super V>`, `List<int?>`) is one
# token, so its `?` is not read as a conditional. Each `?` must sit where a type argument can hold one:
# a wildcard, or a nullable mark right after its type. `a < b ? c > d` is a comparison and a ternary.
# A type can be annotated (`@Nullable V`), be an array (`Object[]`) or have its own type arguments
# (`? extends List<?>`, `List<int>?`), up to _NESTING levels in all. Only a list with a `?` of its own
# is joined: in `Map<K, List<?>>` the inner `<?>` is the token.
_NESTING = 3
# An annotation name is read to its end, so `@AX.Y` cannot be retried as `@A X.Y` or `@AX .Y`.
_ANNOTATIONS = r"(?:@[\w.]+(?![\w.])\s*)*"


def _type(nesting):
    arguments = r"(?:\s*" + _type_arguments(nesting) + r")?" if nesting else ""
    return _ANNOTATIONS + r"(?:(?:in|out)\s+)?[\w.]+" + arguments + r"(?:\s*\[\s*\])*\??"


def _type_arguments(nesting, check=""):
    type_ = _type(nesting - 1)
    # A type reads its own annotations, so they are only read once and cannot be retried.
    argument = r"\s*(?:" + _ANNOTATIONS + r"\?|" + type_ + r")(?:\s+(?:extends|super)\s+" + type_ + r")?\s*"
    return r"\<" + check + argument + r"(?:," + argument + r")*\>"


def _nested_list(nesting):
    if nesting == 1:
        return r"\<[^<>]*\>"
    return r"\<[^<>]*(?:" + _nested_list(nesting - 1) + r"[^<>]*)*\>"


# Checked right after the `<`, so most comparisons fail at once: a `?` follows, past any nested lists.
# The case without a nested list comes first and alone, so `re` can skip to the `?` or the `<` after a scan.
_TYPE_TEXT = r"[\w\s.,@\[\]]*"
_NESTED_LIST = _nested_list(_NESTING - 1)
_OWN_QUESTION_MARK = r"(?=" + _TYPE_TEXT + r"\?|" + \
    _TYPE_TEXT + _NESTED_LIST + _TYPE_TEXT + r"(?:" + _NESTED_LIST + _TYPE_TEXT + r")*\?)"
_WILDCARD_ARGUMENTS = _type_arguments(_NESTING, _OWN_QUESTION_MARK)

_FLAG_PATTERN = re.compile(r'\(\?[aiLmsux]+\)')
_FLAG_VALUES = {
    'a': re.A,  # ASCII-only matching
    'i': re.I,  # Ignore case
    'L': re.L,  # Locale dependent
    'm': re.M,  # Multi-line
    's': re.S,  # Dot matches all
    'u': re.U,  # Unicode matching
    'x': re.X   # Verbose
}


def _addition_key(addition):
    letters = ''.join(opt[2:-1] for opt in _FLAG_PATTERN.findall(addition))
    flags = reduce(or_, (_FLAG_VALUES[letter] for letter in letters), 0)
    return _FLAG_PATTERN.sub('', addition), flags


def compiled_token_pattern(addition=''):
    key = _addition_key(addition)
    pattern = _COMPILED_PATTERNS.get(key)
    if pattern is None:
        pattern = _compile_token_pattern(*key)
        _COMPILED_PATTERNS[key] = pattern
    return pattern


def _compile_token_pattern(addition, flags):
    # DO NOT put any sub groups in the regex. Good for performance
    combined_symbols = [
        "<<=", ">>=", "||", "&&", "===", "!==",
        "==", "!=", "<=", ">=", "->", "=>",
        "++", "--", '+=', '-=',
        "+", "-", '*', '/',
        '*=', '/=', '^=', '&=', '|=', "..."
    ]
    return re.compile(
        r"(?:" +
        r"\/\*.*?\*\/" +
        addition +
        r"|(?:\d+\')+\d+" +
        r"|0x(?:[0-9A-Fa-f]+\')+[0-9A-Fa-f]+" +
        r"|0b(?:[01]+\')+[01]+" +
        r"|\w+" +
        r"|\"(?:\\.|[^\"\\])*\"" +
        r"|\'(?:\\.|[^\'\\])*?\'" +
        r"|\/\/[^\n]*" +
        r"|\#" +
        r"|:=|::|\*\*" +
        r"|" + _WILDCARD_ARGUMENTS +
        r"|" + r"|".join(re.escape(s) for s in combined_symbols) +
        r"|\\\n" +
        r"|\n" +
        r"|[^\S\n]+" +
        r"|.)", re.M | re.S | flags)


def _token_text(match):
    return match.group(0)


class BracketDirective(str):
    """A directive whose lexer tokens show how many brackets remain open."""

    def __new__(cls, text, bracket_depth=0):
        token = super().__new__(cls, text)
        token.bracket_depth = bracket_depth
        return token


_BRACKET_DEPTH = {'[': 1, ']': -1}


def tokens_in_span(pattern, source, start, end, token_class=None,
                   macros=True):
    """With `macros`, a lone '#' starts a preprocessor line kept as one
    token. Lexer rules can name a hash group `standalone_hash` to keep it
    separate. Languages without a preprocessor can disable grouping with False."""
    return tokens_from_matches(pattern.finditer(source, start, end), token_class, macros)


def tokens_from_matches(matches, token_class=None, macros=True):
    """Yield tokens from a stream of lexical matches."""
    if token_class is None:
        token_class = _token_text
    macro = ""
    for match in matches:
        token = token_class(match)
        if macro:
            if "\\\n" in token or "\n" not in token:
                macro += token
            else:
                yield macro
                yield token
                macro = ""
        elif token == "#" and macros and match.lastgroup != "standalone_hash":
            macro = token
        else:
            yield token
    if macro:
        yield macro


def rust_tokens_from_matches(matches, token_class, raw_doc_match):
    """Group Rust attributes while retaining the ordinary token path for other readers."""
    tokens = _RustTokens(matches, token_class, raw_doc_match)
    attribute_depth = 0
    macro = ""
    for match, token in tokens:
        if token == "#" and not macro:
            inspect = getattr(token, 'inspect_attribute', None)
            if inspect is not None:
                opaque = inspect(tokens, tokens.pending)
                if opaque is not None:
                    attribute_depth += yield from _bracket_directive(tokens, attribute_depth, opaque)
                    continue
                token = '#'
        if attribute_depth:
            attribute_depth += _BRACKET_DEPTH.get(token, 0)
            yield token
            continue
        if macro:
            if "\\\n" in token or "\n" not in token:
                macro += token
            else:
                yield macro
                yield token
                macro = ""
        elif token == "#" and match.lastgroup != "standalone_hash":
            macro = token
        else:
            yield token
    if macro:
        yield macro


class _RustTokens:
    def __init__(self, matches, token_class, raw_doc_match):
        self.matches = iter(matches)
        self.token_class = token_class
        self.raw_doc_match = raw_doc_match
        self.pending = deque()
        self.opaque = False

    def __iter__(self):
        return self

    def __next__(self):
        if self.pending:
            return self.pending.popleft()

        match = next(self.matches)
        literal = self.raw_doc_match(match) if self.opaque else None
        if literal is not None:
            # Resume at the real delimiter, even when an ordinary match crossed it.
            rest = match.re.finditer(match.string, literal.end(), match.endpos)
            restart = getattr(self.matches, 'restart', None)
            if restart is None:
                self.matches = rest
            else:
                restart(rest)
            match = literal
        return match, self.token_class(match)


def _bracket_directive(tokens, outer_depth, opaque):
    """Read attribute tokens up to the line or closing bracket."""
    parts = ['#']
    depth = 0
    multiline = False
    while True:
        tokens.opaque = opaque
        entry = next(tokens, None)
        tokens.opaque = False
        if entry is None:
            break
        _, token = entry
        if token == '\n' and not opaque:
            yield BracketDirective(''.join(parts), depth)
            yield token
            return depth
        parts.append(token)
        multiline = multiline or '\n' in token
        depth += _BRACKET_DEPTH.get(token, 0)
        if token == ']' and outer_depth and depth == 0:
            # The nested doc is complete. The rest of this line belongs to its parent.
            opaque = False
        if token == ']' and outer_depth + depth == 0:
            if outer_depth or multiline:
                yield BracketDirective(''.join(parts), depth)
            else:
                yield from _attribute_line_tail(tokens, parts)
            return depth
    yield BracketDirective(''.join(parts), depth)
    return depth


def _attribute_line_tail(tokens, parts):
    """Keep the existing directive tokens for ordinary one-line attributes."""
    trailing_code = False
    for _, token in tokens:
        if "\\\n" not in token and '\n' in token:
            yield _attribute_token(parts, trailing_code)
            yield token
            return
        parts.append(token)
        if not token.isspace() and not token.startswith('//'):
            trailing_code = True
    yield _attribute_token(parts, trailing_code)


def _attribute_token(parts, trailing_code):
    text = ''.join(parts)
    return text if trailing_code else BracketDirective(text)
