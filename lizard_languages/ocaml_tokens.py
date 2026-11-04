"""OCaml tokens from Pygments' lexer, with its fragments joined."""

from pygments import lex, lexers
import pygments.token as py_token


def generate_tokens(source_code):
    return _merge_fragments(
        lex(source_code, lexer=lexers.get_lexer_by_name('ocaml')))


def _token_group(ttype):
    if ttype in py_token.Comment:
        return py_token.Comment
    if ttype in py_token.String:
        return py_token.String
    return None


def _merge_fragments(tokens):
    '''Join Pygments' comment and string fragments into one token each,
    split whitespace into newlines, and join adjacent '|' '|' into '||'.'''
    pending, kind = None, None
    for ttype, value in tokens:
        group = _token_group(ttype)
        if value == '|' and kind == '|':
            pending, kind = '||', None
            continue
        if pending is not None and (group is None or group != kind):
            yield pending
            pending, kind = None, None
        if group is not None or value == '|':
            pending, kind = (pending or '') + value, group or '|'
        elif value.isspace():
            for _ in range(value.count('\n')):
                yield '\n'
        else:
            yield value
    if pending is not None:
        yield pending
