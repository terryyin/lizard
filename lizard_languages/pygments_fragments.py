"""Lizard tokens from a Pygments token stream, with its fragments joined."""

from pygments import lex, lexers
import pygments.token as py_token


def lex_keeping_newlines(source_code, language):
    '''Pygments tokens of `source_code` in `language`, keeping leading and
    trailing newlines so Lizard's line numbers stay right.'''
    return lex(source_code,
               lexers.get_lexer_by_name(language, stripnl=False))


def _joins_nothing(_run, _value):
    return False


def _token_group(ttype, comment_types):
    if ttype in py_token.Comment or \
            any(ttype in comment_type for comment_type in comment_types):
        return py_token.Comment
    if ttype in py_token.String:
        return py_token.String
    return None


_OPERATOR = object()


def join_fragments(tokens, joins=_joins_nothing, comment_types=(),
                   keep_spaces=False):
    '''Join Pygments' comment and string fragments into one token each,
    with `comment_types` read as comments, and join operator runs:
    `joins(run, value)` says whether `value` extends the operator `run`
    ('' to start one). Whitespace becomes one '\\n' per newline, followed
    by the rest of the run when `keep_spaces`.'''
    pending, kind = None, None
    for ttype, value in tokens:
        group = _token_group(ttype, comment_types)
        if group is None and kind is _OPERATOR and joins(pending, value):
            pending += value
            continue
        if pending is not None and (group is None or group != kind):
            yield pending
            pending, kind = None, None
        if group is not None:
            pending, kind = (pending or '') + value, group
        elif joins('', value):
            pending, kind = value, _OPERATOR
        elif value.isspace():
            for _ in range(value.count('\n')):
                yield '\n'
            rest = value[value.rfind('\n') + 1:]
            if keep_spaces and rest:
                yield rest
        else:
            yield value
    if pending is not None:
        yield pending
