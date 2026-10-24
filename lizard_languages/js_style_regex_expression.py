'''
generate token with javascript style regular expression.
'''

import re


# Shared with balanced template scanning: preserve the tokenizer's existing
# expression-opening punctuation rule for distinguishing regex from division.
REGEX_LITERAL = (r"/(?![/*])(?:\\[^\r\n]|\[(?:\\[^\r\n]|[^\\\]\r\n])*\]"
                 r"|[^/\\[\r\n])+/[a-z]*")
REGEX_TOKEN = r"(?:\A|(?<=[=,({[?:!&|;]))\s*" + REGEX_LITERAL


def js_style_regex_expression(func):
    def generate_tokens_with_regex(source_code, addition='', token_class=None):
        # Recognize literals before strings or JSX can consume their contents.
        # Expression-opening punctuation distinguishes them from division.
        addition += r"|" + REGEX_TOKEN
        for token in func(source_code, addition, token_class):
            stripped = token.lstrip()
            if stripped.startswith('/') and not token.startswith('/'):
                # Keep whitespace/newlines observable to the metric counters.
                prefix = token[:len(token) - len(stripped)]
                yield from re.findall(r"\n|[^\S\n]+", prefix)
                yield stripped
            else:
                yield token
    return generate_tokens_with_regex
