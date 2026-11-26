'''
Language parser for Rust lang
'''

import re
from collections import deque

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .token_pattern import compiled_token_pattern, tokens_from_matches


# Rust punctuation is ASCII. U+200E and U+200F are whitespace in Rust.
_NON_ASCII_IDENTIFIER_CHAR = r'[^\x00-\x7f\s\u200e\u200f]'
_RAW_STRING_TOKEN = (
    # Reject prefixes within names and suffixes, including raw C suffixes.
    r'''|(?=[br])(?<![A-Za-z0-9_'"#])(?<!''' + _NON_ASCII_IDENTIFIER_CHAR + r')'
    r'b?r(?P<rust_raw_hashes>#{0,255})"[^"]*'
    r'(?:"(?!(?P=rust_raw_hashes))[^"]*)*"(?P=rust_raw_hashes)'
    r'(?:(?:[A-Za-z_]|' + _NON_ASCII_IDENTIFIER_CHAR +
    r')(?:[A-Za-z_0-9]|' + _NON_ASCII_IDENTIFIER_CHAR + r')*)?'
)


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
        addition = (
            # char literals, before lifetimes, which also start with `'`
            r"|(?:'(?:\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^'\\\n])')"
            r"|(?:'\w+\b)")  # lifetimes, labels
        if 'r"' in source_code or 'r#' in source_code:
            addition += _RAW_STRING_TOKEN
            if token_class is None:
                token_class = _rust_token_text
        source_code = _flatten_nested_comments(source_code)
        if 'quote' not in source_code:
            return CodeReader.generate_tokens(source_code, addition, token_class)
        return _quote_tokens(source_code, addition, token_class)

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
    return token


class RustStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'fn'

    def __init__(self, context, in_match_arms=False):
        super().__init__(context)
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


_QUOTE_MACROS = {'quote', 'quote_spanned'}
_TOKEN_TREE_DEPTH = {'(': 1, '[': 1, '{': 1, ')': -1, ']': -1, '}': -1}
# These prefixes can remain split into tokens. Their text can contain braces.
_UNRECOGNIZED_RAW_START = re.compile(r'(?:\bcr|#b?r)#*"')


def _quote_tokens(source, addition, token_class):
    """Keep the token count of quote input without parsing it as Rust code."""
    matches = compiled_token_pattern(addition).finditer(source)
    raw_starts = {match.start() for match in _UNRECOGNIZED_RAW_START.finditer(source)}
    lookahead = _QuoteLookahead(matches, raw_starts) if raw_starts else None

    observe_macro = _quote_macro_tracker()
    frames = []
    depth = 0
    pending_quote = None

    def read_token(match):
        nonlocal depth, pending_quote
        token = match.group()
        text = token if token_class is None else token_class(match)
        if token.isspace() or token.startswith(('//', '/*')):
            return text

        change = _TOKEN_TREE_DEPTH.get(token, 0)
        if frames:
            opening_depth, evaluate_span = frames[-1]
            if depth == opening_depth + 1:
                if change == -1:
                    frames.pop()
                    depth -= 1
                    return _quote_data(text)
                if evaluate_span and token == '=>':
                    frames[-1] = (opening_depth, False)
                    return _quote_data(text)
            if not evaluate_span:
                depth += change
                return _quote_data(text)

        if pending_quote and change == 1 and (
                lookahead is None or not lookahead.has_ambiguous_raw(match)):
            frames.append((depth, pending_quote == 'quote_spanned'))
            depth += 1
            pending_quote = None
            return _quote_data(text)
        if frames:
            depth += change
        pending_quote = observe_macro(token)
        return text

    return tokens_from_matches(matches if lookahead is None else lookahead, read_token)


def _quote_macro_tracker():
    bindings = _QuoteBindings()
    previous = before_previous = None
    qualifier = ()
    use_tokens = None
    macro_use = False

    def observe(token):
        nonlocal previous, before_previous, qualifier, use_tokens, macro_use
        if use_tokens is not None:
            if token == ';':
                bindings.import_names(use_tokens)
                use_tokens = None
            else:
                use_tokens.append(token)
            return None

        macro = None
        if token == '!' and before_previous != '$':
            macro = bindings.resolve(qualifier + (previous,))
        elif token == 'use' and previous != '+':
            use_tokens = []
        elif previous == '!' and before_previous == 'macro_rules':
            bindings.define_macro(token)
        elif token == 'macro_use' and previous == '[' and before_previous == '#':
            macro_use = True
        elif token == '{':
            bindings.open_scope(previous if before_previous == 'mod' else None, macro_use)
            macro_use = False
        elif token == '}':
            bindings.close_scope()
        elif token == ';':
            if macro_use and previous == 'quote' and before_previous == 'crate':
                bindings.import_quote_crate()
            macro_use = False

        if token == '::':
            qualifier = qualifier + (previous,) if previous and previous.isidentifier() else ()
        elif previous != '::':
            qualifier = ()
        before_previous, previous = previous, token
        return macro

    return observe


class _QuoteBindings:
    """Resolve quote imports visible in this file."""

    def __init__(self):
        self._imports = [{}]
        self._local_macros = [set()]
        self._modules = []
        self._module_paths = set()
        self._exports = {}

    def resolve(self, path):
        if len(path) == 1:
            return self._resolve_bare(path[0])

        current = self._module_path()
        roots = {'crate': (), 'self': current, 'super': current[:-1]}
        if path[0] in roots:
            return self._exports.get(roots[path[0]] + path[1:])
        if (len(path) == 2 and path[0] == 'quote' and path[1] in _QUOTE_MACROS
                and (current + ('quote',)) not in self._module_paths
                and ('quote',) not in self._module_paths):
            return path[1]
        return self._exports.get(current + path, self._exports.get(path))

    def _resolve_bare(self, name):
        if any(name in scope for scope in self._local_macros):
            return None
        start = self._modules[-1][0] if self._modules else 0
        for scope in reversed(self._imports[start:]):
            if name in scope:
                return scope[name]
        return None

    def import_names(self, tokens):
        for path, name in _import_paths(tokens):
            macro = self.resolve(path)
            self._imports[-1][name] = macro
            if self._modules and self._modules[-1][0] == len(self._imports) - 1:
                self._exports[self._module_path() + (name,)] = macro

    def define_macro(self, name):
        self._local_macros[-1].add(name)

    def import_quote_crate(self):
        self._imports[-1].update((name, name) for name in _QUOTE_MACROS)

    def open_scope(self, module, macro_use):
        self._imports.append({})
        self._local_macros.append(set())
        if module:
            self._modules.append((len(self._imports) - 1, module, macro_use))
            self._module_paths.add(self._module_path())

    def close_scope(self):
        if len(self._imports) == 1:
            return
        macros = self._local_macros.pop()
        self._imports.pop()
        if self._modules and self._modules[-1][0] == len(self._imports):
            _, _, macro_use = self._modules.pop()
            if macro_use:
                self._local_macros[-1].update(macros)

    def _module_path(self):
        return tuple(module for _, module, _ in self._modules)


def _import_paths(tokens):
    if '{' not in tokens:
        prefix, entries = [], [tokens]
    else:
        group_start = tokens.index('{')
        if tokens[-1] != '}' or '{' in tokens[group_start + 1:]:
            return
        prefix, entries = tokens[:group_start], [[]]
        for token in tokens[group_start + 1:-1]:
            if token == ',':
                entries.append([])
            else:
                entries[-1].append(token)

    for entry in entries:
        if not entry:
            continue
        if 'as' in entry:
            rename = entry.index('as')
            name, entry = entry[-1], entry[:rename]
        else:
            name = entry[-1]
        path = tuple(token for token in prefix + entry if token != '::')
        if path and all(part.isidentifier() for part in path) and name != '_':
            yield path, name


def _quote_data(token):
    # Like a string token, quoted text supplies data to the macro.
    return token if token.startswith(('"', "'")) else '"' + token


class _QuoteLookahead:
    """Look for an ambiguous literal inside this invocation using the same matches."""

    def __init__(self, matches, raw_starts):
        self._matches = matches
        self._pending = deque()
        self._raw_starts = raw_starts
        self._last_raw_start = max(raw_starts)

    def __iter__(self):
        return self

    def __next__(self):
        return self._pending.popleft() if self._pending else next(self._matches)

    def has_ambiguous_raw(self, opener):
        if opener.end() > self._last_raw_start:
            return False
        depth = 1
        for match in self._peek():
            start = match.start()
            if start in self._raw_starts and not _raw_c_boundaries_agree(match.string, start):
                return True
            depth += _TOKEN_TREE_DEPTH.get(match.group(), 0)
            if not depth or start >= self._last_raw_start:
                break
        return False

    def _peek(self):
        yield from self._pending
        for match in self._matches:
            self._pending.append(match)
            yield match


_RAW_C_PREFIX = re.compile(r'cr(#*)"')
_COOKED_STRING = re.compile(r'"(?:\\.|[^"\\])*"', re.S)


def _raw_c_boundaries_agree(source, start):
    literal = _RAW_C_PREFIX.match(source, start)
    if not literal:
        return False
    closing = source.find('"', literal.end())
    if closing == -1 or not source.startswith(literal.group(1), closing + 1):
        return False
    cooked = _COOKED_STRING.match(source, literal.end() - 1)
    return cooked is not None and cooked.end() == closing + 1


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
