"""Shared source tokenizer.

The pattern is compiled once for each addition. A reader that splits a file
into spans reuses that pattern instead of compiling it for every span.
"""

import re
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
_TYPE_TEXT = r"[\w\s.,@\[\]]*"
_OWN_QUESTION_MARK = r"(?=" + _TYPE_TEXT + r"(?:" + _nested_list(_NESTING - 1) + _TYPE_TEXT + r")*\?)"
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
    until_end = r"(?:\\\n|[^\n])*"
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
        r"|\/\/" + until_end +
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


def tokens_in_span(pattern, source, start, end, token_class=None,
                   macros=True):
    """With `macros`, a lone '#' starts a preprocessor line kept as one
    token; languages without a preprocessor keep '#' as its own token."""
    if token_class is None:
        token_class = _token_text
    macro = ""
    for match in pattern.finditer(source, start, end):
        token = token_class(match)
        if macro:
            if "\\\n" in token or "\n" not in token:
                macro += token
            else:
                yield macro
                yield token
                macro = ""
        elif token == "#" and macros:
            macro = token
        else:
            yield token
    if macro:
        yield macro
