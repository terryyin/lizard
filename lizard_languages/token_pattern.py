"""Shared source tokenizer.

The pattern is compiled once for each addition. A reader that splits a file
into spans reuses that pattern instead of compiling it for every span.
"""

import re
from functools import reduce
from operator import or_


_COMPILED_PATTERNS = {}
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
        r"|\<(?=(?:[^<>?]*\?)+[^<>]*\>)(?:[\w\s,.?]|(?:extends))+\>" +
        r"|" + r"|".join(re.escape(s) for s in combined_symbols) +
        r"|\\\n" +
        r"|\n" +
        r"|[^\S\n]+" +
        r"|.)", re.M | re.S | flags)


def _token_text(match):
    return match.group(0)


def tokens_in_span(pattern, source, start, end, token_class=None):
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
        elif token == "#":
            macro = token
        else:
            yield token
    if macro:
        yield macro
