'''
generate token with javascript style regular expression.
'''

import re

from .js_style_regex_context import (
    REGEX_LITERAL,
    REGEX_START_MARKER,
    REGEX_TOKEN,
    mark_regex_literal_starts,
)

# Re-export the shared literal grammar and marked token pattern for callers
# such as balanced template scanning.
__all__ = (
    'REGEX_LITERAL',
    'REGEX_TOKEN',
    'js_style_regex_expression',
    'mark_regex_literal_starts',
)


def _unmark_regex_token(token):
    if not isinstance(token, str) or REGEX_START_MARKER not in token:
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
            addition += r"|" + REGEX_TOKEN
            for token in generate_tokens(marked, addition, token_class):
                token = _unmark_regex_token(token)
                stripped = token.lstrip() if isinstance(token, str) else token
                if (isinstance(token, str) and stripped.startswith('/')
                        and not token.startswith('/')):
                    # Keep whitespace/newlines observable to the metric counters.
                    prefix = token[:len(token) - len(stripped)]
                    yield from re.findall(r"\n|[^\S\n]+", prefix)
                    yield stripped
                else:
                    yield token
        return generate_tokens_with_regex

    if func is not None:
        return decorator(func)
    return decorator
