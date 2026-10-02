'''
generate token with javascript style regular expression.
'''

import re


# A regular expression literal: escapes, character classes (where a slash
# does not end the literal) and anything else up to the closing slash, with
# the flags. It never spans lines and is never empty ("//" is a comment).
_REGEX_LITERAL = re.compile(
    r"/(?:\\.|\[(?:\\.|[^\]\\\n])*\]|[^/\\\[\n])+/\w*")

# What a regular expression literal can follow; after anything else, like an
# identifier, a number or a closing bracket, the slash is a division.
_BEFORE_REGEX_CHARACTERS = '=,({[?:!&|;'
_BEFORE_REGEX_TOKENS = frozenset((
    '=>', 'return', 'typeof', 'instanceof', 'in', 'of', 'new', 'delete',
    'void', 'throw', 'case', 'do', 'else', 'yield', 'await'))


def _can_start_regex(previous):
    return (previous is None or
            previous in _BEFORE_REGEX_TOKENS or
            previous[-1] in _BEFORE_REGEX_CHARACTERS)


def js_style_regex_tokens(generate_tokens, source_code, addition='',
                          token_class=None):
    '''
    Generate the tokens of generate_tokens with every JavaScript regular
    expression literal as one token.

    The literal is read from the source code and not put together from the
    tokens, because the tokenizer takes its content for something else: a "#"
    starts a preprocessor line, "//" a comment and a quote a string, and the
    code after the literal is lost. After a literal the tokenizer starts again
    from the character that follows it.

    generate_tokens must yield the source code in consecutive pieces.
    '''
    start = 0
    previous = None
    while start is not None:
        position, start = start, None
        for token in generate_tokens(
                source_code[position:], addition, token_class):
            if token == '/' and _can_start_regex(previous):
                literal = _REGEX_LITERAL.match(source_code, position)
                if literal:
                    previous = literal.group(0)
                    yield token_class(literal) if token_class else previous
                    start = literal.end()
                    break
            yield token
            position += len(token)
            if not (token.isspace() or token.startswith(('//', '/*'))):
                previous = token


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
                    continue
                else:
                    # This is a division operator
                    result.append(token)
            else:
                result.append(token)
            i += 1
        return result
    return generate_tokens_with_regex
