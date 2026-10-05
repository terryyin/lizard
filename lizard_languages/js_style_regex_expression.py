'''
generate token with javascript style regular expression.
'''

import re


def js_style_regex_expression(func):
    def generate_tokens_with_regex(source_code, addition='', token_class=None):
        # Recognize literals before strings or JSX can consume their contents.
        # Expression-opening punctuation distinguishes them from division.
        regex = (r"/(?![/*])(?:\\[^\r\n]|\[(?:\\[^\r\n]|[^\\\]\r\n])*\]"
                 r"|[^/\\[\r\n])+/[a-z]*")
        addition += r"|(?:\A|(?<=[=,({[?:!&|;]))\s*" + regex
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
