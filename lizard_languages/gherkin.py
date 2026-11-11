'''
Language reader for Gherkin feature files.

Counts written Background / Scenario / Outline blocks as functions. Feature and
Rule are containers only. Doc strings and table rows stay opaque.
'''

import re

from .code_reader import CodeReader


_BLOCK_KINDS = (
    'Background',
    'Scenario Outline',
    'Scenario Template',
    'Scenario',
    'Example',
)
_OUTLINE_KINDS = frozenset(('Scenario Outline', 'Scenario Template'))
_DECL_PATTERN = re.compile(
    r'^\s*(' + '|'.join(_BLOCK_KINDS) + r')(?:\s*(:.*)?|\s*)$'
)
_CONTAINER_PATTERN = re.compile(r'^\s*(Feature|Rule)\s*:')
_EXAMPLES_PATTERN = re.compile(r'^\s*(Examples|Scenarios)\s*:')
_TAG_PATTERN = re.compile(r'^\s*@\S')
_DOC_DELIM_PATTERN = re.compile(r'^\s*("""|```)')
# Emitted between flushed prefix lines and the declaration so the new function
# opens after tags reach global counters and before the header is counted.
_OPEN_FUNCTION = object()


class GherkinCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('#'):
            return token[1:]


class GherkinReader(CodeReader, GherkinCommentsMixin):
    '''Reader for English Gherkin `.feature` sources.'''

    ext = ['feature']
    language_names = ['gherkin']

    _control_flow_keywords = set()
    _logical_operators = set()
    _case_keywords = set()
    _ternary_operators = set()

    def __init__(self, context):
        super(GherkinReader, self).__init__(context)
        self.in_function = False
        self.in_outline = False
        self.doc_delimiter = None
        self._held_lines = []
        self._deferred_name = None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        if not source_code:
            return iter(())
        tokens = []
        for raw in source_code.splitlines(True):
            if raw.endswith('\r\n'):
                content, newline = raw[:-2], True
            elif raw.endswith('\n') or raw.endswith('\r'):
                content, newline = raw[:-1], True
            else:
                content, newline = raw, False
            if content:
                tokens.append(content)
            if newline:
                tokens.append('\n')
        if token_class is not None:
            return (token_class(token) for token in tokens)
        return iter(tokens)

    def preprocess(self, tokens):
        for token in tokens:
            if token == '\n':
                if self._held_lines:
                    self._held_lines.append('\n')
                else:
                    yield '\n'
                continue
            for emitted in self._handle_line(token):
                if emitted is _OPEN_FUNCTION:
                    self._start_function(self._deferred_name)
                    self._deferred_name = None
                    continue
                yield emitted
        if self._held_lines:
            self._end_function()
            for emitted in self._emit_held():
                yield emitted

    def eof(self):
        self._end_function()

    def _handle_line(self, line):
        if self.doc_delimiter is not None:
            return self._handle_doc_line(line)

        stripped = line.lstrip()
        if not stripped:
            return ()

        if stripped.startswith('#'):
            comment = '#' + stripped[1:]
            if self._held_lines:
                self._held_lines.append(comment)
                return ()
            return (comment,)

        delim_match = _DOC_DELIM_PATTERN.match(line)
        if delim_match and self.in_function:
            self.doc_delimiter = delim_match.group(1)
            return self._after_structural_flush(line)

        if _TAG_PATTERN.match(line):
            self._held_lines.append(line)
            return ()

        if _EXAMPLES_PATTERN.match(line) and self.in_outline:
            return self._emit_held() + (line,)

        if _CONTAINER_PATTERN.match(line):
            return self._finish_before_structure() + (line,)

        declaration = _DECL_PATTERN.match(line)
        if declaration:
            return self._open_declaration(line, declaration)

        return self._after_structural_flush(line)

    def _handle_doc_line(self, line):
        stripped = line.lstrip()
        if stripped.startswith(self.doc_delimiter):
            self.doc_delimiter = None
        if not stripped:
            return ()
        return (line,)

    def _open_declaration(self, line, match):
        kind = match.group(1)
        title = (match.group(2) or '').strip()
        if title.startswith(':'):
            title = title[1:].strip()
        name = kind if not title else '%s: %s' % (kind, title)
        emitted = self._finish_before_structure()
        self._deferred_name = name
        self.in_outline = kind in _OUTLINE_KINDS
        return emitted + (_OPEN_FUNCTION, line)

    def _after_structural_flush(self, line):
        if not self._held_lines:
            return (line,)
        # Orphan held tags before ordinary content: keep them outside the
        # predecessor by ending it first when they cannot decorate Examples.
        if self.in_outline:
            return self._emit_held() + (line,)
        return self._finish_before_structure() + (line,)

    def _finish_before_structure(self):
        self._end_function()
        return self._emit_held()

    def _emit_held(self):
        held = self._held_lines
        self._held_lines = []
        return tuple(held)

    def _start_function(self, name):
        self.context.restart_new_function(name)
        self.context.current_function.nloc = 0
        self.context.current_function.token_count = 0
        self.in_function = True

    def _end_function(self):
        if self.in_function:
            self.context.end_of_function()
            self.in_function = False
        self.in_outline = False
        self.doc_delimiter = None
