'''
generate token with javascript style regular expression.
'''

import re

from .js_style_regex_context import (
    REGEX_LITERAL,
    REGEX_START_MARKER,
    REGEX_TOKEN,
    REGEX_TOKEN_PUNCT,
    mark_regex_literal_starts,
)

__all__ = (
    'REGEX_LITERAL',
    'REGEX_TOKEN',
    'js_style_regex_expression',
    'mark_regex_literal_starts',
)


def _unmark_regex_token(token):
    if REGEX_START_MARKER not in token:
        return token
    cleaned = token.replace(REGEX_START_MARKER, '/')
    if type(token) is str:
        return cleaned
    begin = getattr(token, 'begin', 0)
    replacement = type(token)(cleaned)
    replacement.begin = begin
    return replacement


def js_style_regex_expression(func=None, *, language='js'):
    def decorator(generate_tokens):
        def generate_tokens_with_regex(source_code, addition='', token_class=None):
            # Recognize literals before strings or JSX can consume their contents.
            marked = mark_regex_literal_starts(source_code, language=language)
            if marked is source_code:
                addition += r"|" + REGEX_TOKEN_PUNCT
                return generate_tokens(marked, addition, token_class)
            addition += r"|" + REGEX_TOKEN
            return _yield_unmarked(generate_tokens(marked, addition, token_class))
        return generate_tokens_with_regex

    if func is not None:
        return decorator(func)
    return decorator


def _yield_unmarked(tokens):
    marker = REGEX_START_MARKER
    findall = re.findall
    for token in tokens:
        if not isinstance(token, str):
            yield _unmark_regex_token(token)
            continue
        if marker in token:
            token = token.replace(marker, '/')
        stripped = token.lstrip()
        if stripped.startswith('/') and not token.startswith('/'):
            prefix = token[:len(token) - len(stripped)]
            yield from findall(r"\n|[^\S\n]+", prefix)
            yield stripped
        else:
            yield token
