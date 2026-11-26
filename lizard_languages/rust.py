'''
Language parser for Rust lang
'''

import re

from .token_pattern import BracketDirective, compiled_token_pattern, rust_tokens_from_matches
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


# Rust punctuation is ASCII. U+200E and U+200F are whitespace in Rust.
_NON_ASCII_IDENTIFIER_CHAR = r'[^\x00-\x7f\s\u200e\u200f]'
_RAW_LITERAL_BODY = (
    r'(?P<rust_raw_hashes>#{0,255})"[^"]*'
    r'(?:"(?!(?P=rust_raw_hashes))[^"]*)*"(?P=rust_raw_hashes)'
    r'(?:(?:[A-Za-z_]|' + _NON_ASCII_IDENTIFIER_CHAR +
    r')(?:[A-Za-z_0-9]|' + _NON_ASCII_IDENTIFIER_CHAR + r')*)?'
)
_RAW_STRING_TOKEN = (
    # Reject prefixes within names and suffixes, including raw C suffixes.
    r'''|(?=[br])(?<![A-Za-z0-9_'"#])(?<!''' + _NON_ASCII_IDENTIFIER_CHAR + r')b?r' +
    _RAW_LITERAL_BODY
)
_RAW_DOC_LITERAL = re.compile(r'(?:cr|(?<=#)b?r)' + _RAW_LITERAL_BODY)


class RustReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['rs']
    language_names = ['rust']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'where'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Rust match arms are counted via `=>` in RustStates
    # Note: '?' in Rust is the error propagation operator, not ternary
    _ternary_operators = {'?'}

    # After one of these, `||` opens a closure without parameters.
    _CLOSURE_OPENERS = {'(', '[', '{', ',', ';', ':', '=', '=>', '->',
                        '&&', '||', '|', 'move', 'async', 'return'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [RustStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        if token_class is None and '#' in source_code:
            token_class = _rust_token_text
        addition = (
            (_MACRO_HASH_TOKEN if '#' in source_code else '') +
            # char literals, before lifetimes, which also start with `'`
            r"|(?:'(?:\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^'\\\n])')"
            r"|(?:'\w+\b)")  # lifetimes, labels
        if 'r"' in source_code or 'r#' in source_code:
            addition += _RAW_STRING_TOKEN
            if token_class is None:
                token_class = _rust_token_text
        source_code = _flatten_nested_comments(source_code)
        if token_class is _rust_token_text and '#' in source_code:
            pattern = compiled_token_pattern(addition)
            return rust_tokens_from_matches(pattern.finditer(source_code), token_class, _raw_doc_match)
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        """Normalize an empty closure's `||` to one non-logical pipe token."""
        openers = self._CLOSURE_OPENERS
        last = None
        for token in tokens:
            if token.isspace():
                if token == '\n':
                    yield token
                continue
            if token == '||' and last in openers:
                yield '|'
            else:
                yield token
            if token[0] != '/':  # comments do not end an expression
                last = token


def _rust_token_text(match):
    token = match.group(0)
    if match.lastgroup == 'rust_raw_hashes':
        # Extensions identify string tokens by the first quote.
        return '"' + token
    if token == '#' and match.lastgroup != 'standalone_hash':
        return RustAttributeHash(token)
    return token


def _raw_doc_match(match):
    if match.group() in ('cr', 'br', 'r'):
        return _RAW_DOC_LITERAL.match(match.string, match.start(), match.endpos)
    return None


class RustStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'fn'
    _SIGNATURE_CLOSERS = {'(': ')', '[': ']', '{': '}', '<': '>'}

    def __init__(self, context, in_match_arms=False):
        super().__init__(context)
        self._forgive_enclosing = (context.forgive
                                   and context.current_function is not context.global_pseudo_function)
        self._in_match_arms = in_match_arms
        self._seen_match_arm = False
        self._match_subject_nesting = 0
        # Paren depth of each `if` still waiting for its body, innermost last.
        self._if_body_depths = []
        self._else_pending = False
        self._paren_depth = 0

    def _state_global(self, token):
        if token == '=>':
            if self._in_match_arms:
                if self._seen_match_arm:
                    self.context.add_condition()
                self._seen_match_arm = True
            return
        if token == 'match':
            self._match_subject_nesting = 0
            self._state = self._match_subject
            return
        if self._count_let_else(token):
            return
        super()._state_global(token)

    def _function_name(self, token):
        if token == '(':
            # A bare function type has no name between `fn` and `(`.
            self._fold_signature()
            self.next(self._state_global, token)
        else:
            super()._function_name(token)

    def _skip_type_parameters(self, token):
        if token == '{':
            # Comparisons in a const argument do not close type parameters.
            self._open_signature_group(token, in_type_parameters=True)
        else:
            super()._skip_type_parameters(token)

    def _expect_function_impl(self, token):
        if token == '{':
            super()._expect_function_impl(token)
        elif token in ('[', '(', '<'):
            self._open_signature_group(token)
        elif token == '!' and (self.last_token.isidentifier() or self.last_token.startswith('r#')):
            self.next(self._after_type_macro_bang)
        elif token == ';':
            self._fold_signature()
            if not self._forgive_enclosing:
                self.context.forgive = False
            self.next(self._state_global)
        else:
            self._fall_back_on_hidden_delimiters(token)

    def _after_type_macro_bang(self, token):
        if token == '{':
            self._open_signature_group(token)
        else:
            self.next(self._expect_function_impl, token)

    def _open_signature_group(self, token, in_type_parameters=False):
        ends = [self._SIGNATURE_CLOSERS[token]]
        after_group = self._skip_type_parameters if in_type_parameters else self._expect_function_impl

        def read_group(token):
            expected = ends[-1]
            if token == expected:
                ends.pop()
                if not ends:
                    self.next(after_group)
            elif token in ('[', '(', '{') or (token == '<' and expected == '>'):
                # Angle tokens can be operators inside a const block or macro.
                ends.append(self._SIGNATURE_CLOSERS[token])
            elif not in_type_parameters or isinstance(token, BracketDirective):
                self._fall_back_on_hidden_delimiters(token)

        self.next(read_group)

    def _fall_back_on_hidden_delimiters(self, token):
        if isinstance(token, BracketDirective):
            if token.bracket_depth:
                self._skip_split_attribute(token.bracket_depth)
            return
        if token[0] == '#':
            hidden_delimiters = True
        else:
            # In an un-hashed raw string the first quote ends the literal.
            hidden_delimiters = (token[0] == '"' and self.last_token in ('r', 'br', 'cr')
                                 and token.find('"', 1) != len(token) - 1)
        if hidden_delimiters:
            # Keep looking for the body if the tokenizer hid part of the signature.
            self.next(super()._expect_function_impl)

    def _skip_split_attribute(self, depth):
        after_attribute = self._state

        def read_attribute(token):
            nonlocal depth
            if isinstance(token, BracketDirective):
                depth += token.bracket_depth
            elif token == '[':
                depth += 1
            elif token == ']':
                depth -= 1
            if depth == 0:
                self.next(after_attribute)

        self.next(read_attribute)

    def _fold_signature(self):
        self.fold_into_enclosing_function()
        # The enclosing scope already counted the opening `fn` token.
        self.context.current_function.nloc -= 1
        self.context.current_function.token_count -= 1

    def _function_impl(self, token):
        # A full function body consumes the pending forgiveness.
        self._forgive_enclosing = False
        super()._function_impl(token)

    def _count_let_else(self, token):
        """Count `let ... else`. The `else` of an `if` is not another condition.

        Brace bodies are separate state machines, so an `if` and its `else`
        are both seen here with the body omitted. The body is the `{` that
        returns to the parenthesis depth where that `if` began.
        """
        if token != 'else':
            self._else_pending = False
        if token in '([':
            self._paren_depth += 1
        elif token in ')]' and self._paren_depth:
            self._paren_depth -= 1
        if token == 'else':
            if not self._else_pending:
                self.context.add_condition()
            self._else_pending = False
            return True
        if token == 'if':
            self._if_body_depths.append(self._paren_depth)
            return False
        if (token == '{' and self._if_body_depths
                and self._paren_depth == self._if_body_depths[-1]):
            self._if_body_depths.pop()
            self.sub_state(self.statemachine_clone(), self._if_body_closed)
            return True
        return False

    def _if_body_closed(self):
        self._else_pending = True

    def _match_subject(self, token):
        if token in '([':
            self._match_subject_nesting += 1
            return
        if token in ')]':
            if self._match_subject_nesting:
                self._match_subject_nesting -= 1
            return
        if token != '{' or self._match_subject_nesting:
            return
        self.sub_state(
            RustStates(self.context, in_match_arms=True),
            self._end_match)

    def _end_match(self):
        self.next(self._state_global)


_CHAR_LITERAL = re.compile(
    r"'(?:\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^'\\\n])'")
_RAW_STRING = re.compile(r'b?r(#*)"')


def _flatten_nested_comments(source):
    """Rust block comments nest: `/* a /* b */ c */` is one comment.
    The shared tokenizer ends a block comment at its first `*/`, so a nested
    one (e.g. `src/**/foo.rs` in a doc comment) leaks the rest of the comment
    into the code. Break the inner `*/`s so the comment is read whole.
    Strings, raw strings, char literals and line comments are skipped, so a
    `/*` inside them does not open a comment."""
    if '/*' not in source:
        return source
    out = []
    i, n = 0, len(source)
    while i < n:
        two = source[i:i + 2]
        if two == '//':
            end = source.find('\n', i)
            end = n if end < 0 else end
        elif two == '/*':
            depth, j = 1, i + 2
            while j < n and depth:
                if source.startswith('/*', j):
                    depth, j = depth + 1, j + 2
                elif source.startswith('*/', j):
                    depth, j = depth - 1, j + 2
                else:
                    j += 1
            if depth:  # unterminated: leave it to the tokenizer
                out.append(source[i:])
                break
            inner = source[i + 2:j - 2]
            out.append('/*' + inner.replace('*/', '* /') + '*/')
            i = j
            continue
        elif source[i] == '"' or (
                source[i] in 'br' and (i == 0 or not (
                    source[i - 1].isalnum() or source[i - 1] == '_'))
                and _RAW_STRING.match(source, i)):
            raw = _RAW_STRING.match(source, i) if source[i] != '"' else None
            if raw:
                close = '"' + raw.group(1)
                end = source.find(close, raw.end())
                end = n if end < 0 else end + len(close)
            else:
                j = i + 1
                while j < n and source[j] != '"':
                    j += 2 if source[j] == '\\' else 1
                end = min(j + 1, n)
        elif source[i] == "'":
            match = _CHAR_LITERAL.match(source, i)
            end = match.end() if match else i + 1
        else:
            end = i + 1
        out.append(source[i:end])
        i = end
    return ''.join(out)


# A hash followed by a name or repetition can be a standalone macro token.
# Keep the tokenizer's fallback around raw identifiers and raw strings.
_MACRO_HASH_TOKEN = (
    r"""|(?<![r"#])(?P<standalone_hash>#)"""
    r"""(?=[^\S\n]*(?!(?:b|c)?r#*")(?:\(|[^\W\d]))"""
)


_GROUP_END = {'(': ')', '[': ']', '{': '}'}
_CLOSERS = frozenset(_GROUP_END.values())
_RUST_SPACE = '\u200e\u200f'


class RustAttributeHash(str):
    """A hash that can introduce an attribute."""

    @staticmethod
    def inspect_attribute(tokens, pending):
        seen = []

        def remember():
            for entry in tokens:
                seen.append(entry)
                yield entry

        recorded = remember()
        significant = _significant(recorded)
        opaque = _inspect(significant, recorded, seen, tokens)
        pending.extendleft(reversed(seen))
        return opaque


def _inspect(significant, recorded, seen, tokens):
    opener = _next_text(significant)
    if opener == '!':
        opener = _next_text(significant)
    if opener != '[':
        return None
    # Keep earlier grouping for ordinary attributes. A doc can span trivia
    # before its opening bracket because the lexer confirms the whole prefix.
    group_brackets = all(
        match.group() == '!' or (token.isspace() and '\n' not in token)
        for match, token in seen[:-1])

    name = _name(significant)
    if name == 'doc' and _next_text(significant) == '=':
        return True
    if name == 'cfg_attr' and _next_text(significant) == '(':
        try:
            spans = _configured_docs(recorded, seen, tokens)
        finally:
            tokens.opaque = False
        if spans and _next_text(significant) == ']':
            seen[:] = _hide_doc_values(seen, spans)
    return False if group_brackets else None


def _configured_docs(recorded, seen, tokens):
    """Find doc values after each cfg predicate, matching literal boundaries."""
    groups = [')']
    cfg_fields = {1: None}
    doc_depth = None
    value_start = None
    spans = []
    for index, (match, _) in enumerate(recorded, len(seen)):
        token = match.group()
        depth = len(groups)
        field = cfg_fields.get(depth)
        if depth in cfg_fields and token in (',', ')'):
            if doc_depth == depth:
                spans.append((value_start, index))
                value_start = doc_depth = None
                tokens.opaque = False
            if token == ')':
                del cfg_fields[depth]
                if depth == 1:
                    return spans
            else:
                cfg_fields[depth] = index + 1
        elif field is not None and doc_depth is None and token in ('=', '('):
            prefix = _significant(seen[field:index])
            name = _name(prefix)
            if name == 'doc' and token == '=' and _next_text(prefix) is None:
                value_start = index + 1
                doc_depth = depth
                tokens.opaque = True
            elif name == 'cfg_attr' and token == '(' and _next_text(prefix) is None:
                cfg_fields[depth + 1] = None
        if token in _GROUP_END:
            groups.append(_GROUP_END[token])
        elif token in _CLOSERS:
            if token != groups.pop():
                return None
    return None


def _hide_doc_values(tokens, spans):
    cursor = 0
    for start, end in spans:
        yield from tokens[cursor:start]
        yield from _opaque_lines(tokens[start:end])
        cursor = end
    yield from tokens[cursor:]


def _opaque_lines(tokens):
    """Hide a doc value while retaining its physical line boundaries."""
    line = []
    for entry in tokens:
        if entry[0].group() == '\n':
            yield from _opaque_line(line)
            line = []
            yield entry
        else:
            line.append(entry)
    yield from _opaque_line(line)


def _opaque_line(line):
    if any(not token.isspace() for _, token in line):
        yield line[0][0], '"' + ''.join(token for _, token in line)
    else:
        yield from line


def _significant(tokens):
    for match, token in tokens:
        text = match.group()
        if not (text.isspace() or text in _RUST_SPACE or text.startswith(('//', '/*'))):
            yield match, token


def _name(tokens):
    entry = next(tokens, None)
    if entry is None:
        return None
    match = entry[0]
    text = match.group()
    if text.startswith('r#') and text[2:].isidentifier():
        return text[2:]
    if text != 'r' or not match.string.startswith('#', match.end()):
        return text

    marker = next(tokens, None)
    identifier = next(tokens, None)
    if (marker is not None and identifier is not None and
            marker[0].group() == '#' and match.end() == marker[0].start() and
            marker[0].end() == identifier[0].start()):
        return identifier[0].group()
    return None


def _next_text(tokens):
    entry = next(tokens, None)
    return entry[0].group() if entry is not None else None
