'''
generate token with javascript style regular expression.
'''

import re


def js_style_regex_expression(func):
    def generate_tokens_with_regex(source_code, addition='', token_class=None):
        regx_regx = r"\/(\S*?[^\s\\]\/)+?(igm)*"
        regx_pattern = re.compile(regx_regx)
        tokens = list(func(source_code, addition, token_class))
        result = []
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if token == '/':
                # Check if this could be a regex pattern
                is_regex = False
                if i == 0:
                    is_regex = True
                elif i > 0:
                    prev_token = tokens[i-1].strip()
                    if prev_token and prev_token[-1] in '=,({[?:!&|;':
                        is_regex = True

                if is_regex:
                    # This is likely a regex pattern start
                    regex_tokens = [token]
                    i += 1
                    while i < len(tokens) and not tokens[i].endswith('/'):
                        regex_tokens.append(tokens[i])
                        i += 1
                    if i < len(tokens):
                        regex_tokens.append(tokens[i])
                        i += 1
                        # Check for regex flags
                        if i < len(tokens) and re.match(r'^[igm]+$', tokens[i]):
                            regex_tokens.append(tokens[i])
                            i += 1
                    combined = ''.join(regex_tokens)
                    if regx_pattern.match(combined):
                        result.append(combined)
                    else:
                        result.extend(regex_tokens)
                else:
                    # This is a division operator
                    result.append(token)
            else:
                result.append(token)
            i += 1
        return result
    return generate_tokens_with_regex
