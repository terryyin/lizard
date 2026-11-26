"""Keep the line breaks of lexical documentation in the token stream."""

from itertools import repeat
from operator import itemgetter

from .script_language import ScriptLanguageMixIn


class _Documentation(str):
    pass


def tokens_ignoring_documentation(source, addition, token_class, documentation,
                                  may_have_documentation, *, start=0):
    """Match complete documentation sections alongside normal tokens."""
    # Both paths use the same compiled lexer. Only sources with possible
    # documentation need the match conversion and newline substitution.
    addition = r'|(?P<documentation>' + documentation + ')' + addition
    if not may_have_documentation:
        return ScriptLanguageMixIn.generate_common_tokens(source, addition, token_class, start=start)

    code_token = token_class if token_class is not None else itemgetter(0)

    def token_from_match(match):
        if match.lastgroup == 'documentation':
            return _Documentation(match[0])
        return code_token(match)

    tokens = ScriptLanguageMixIn.generate_common_tokens(source, addition, token_from_match, start=start)
    return _replace_documentation(tokens)


def _replace_documentation(tokens):
    for token in tokens:
        if type(token) is _Documentation:
            yield from repeat('\n', token.count('\n'))
        else:
            yield token
