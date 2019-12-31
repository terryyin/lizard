'''
generate token with javascript style regular expression.
'''

import re


def js_style_regex_expression(func):
    def generate_tokens_with_regex(source_code, _=""):
        regx_regx = r"\/(\S*?[^\s\\]\/)+?(igm)*"
        regx_pattern = re.compile(regx_regx)
        word_pattern = re.compile(r'\w+')
        tokens = func(source_code, r"|"+regx_regx)
        leading_by_word = False
        for token in tokens:
            if leading_by_word and regx_pattern.match(token):
                for subtoken in func(token, _):
                    yield subtoken
            else:
                yield token
            if not token.isspace():
                leading_by_word = word_pattern.match(token)
    return generate_tokens_with_regex
