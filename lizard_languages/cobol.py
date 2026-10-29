'''
Language parser for fixed-format COBOL.
'''

from .code_reader import CodeReader
from .cobol_states import (
    DIVISION_STARTERS,
    PARAGRAPH,
    SECTION,
    CobolStates,
)


# COBOL words may contain hyphens; keep them as one token (no capturing groups).
_COBOL_WORD = r'[A-Za-z0-9]+(?:-[A-Za-z0-9]+)+'
_UNTIL_END = r'(?:\\\n|[^\n])*'


def preprocess_fixed_format(source_code):
    """Strip sequence area and apply column-7 indicators before tokenization.

    Columns 1-6 are ignored. Column 7: ``*``/``/``/``D``/``d`` → comment;
    space → code; ``-`` → continuation (join so continued tokens do not invent
    decisions).
    """
    if not source_code:
        return source_code

    out = []
    for raw_line in source_code.splitlines(True):
        if raw_line.endswith('\r\n'):
            content, ending = raw_line[:-2], '\r\n'
        elif raw_line.endswith('\n'):
            content, ending = raw_line[:-1], '\n'
        elif raw_line.endswith('\r'):
            content, ending = raw_line[:-1], '\r'
        else:
            content, ending = raw_line, ''

        # Ignore identification area (cols 73+) when present.
        if len(content) > 72:
            content = content[:72]

        if len(content) < 7:
            out.append(content + ending)
            continue

        indicator = content[6]
        body = content[7:]

        if indicator in ('*', '/', 'D', 'd'):
            out.append('*>' + body + ending)
        elif indicator == '-':
            # Continuation content traditionally starts in Area B (col 12).
            cont = content[11:] if len(content) > 11 else body
            if out:
                prev = out[-1]
                if prev.endswith('\r\n'):
                    out[-1] = prev[:-2]
                elif prev.endswith('\n') or prev.endswith('\r'):
                    out[-1] = prev[:-1]
            out.append(cont + ending)
        else:
            # Space (code) or any other indicator: emit Area A/B body.
            out.append(body + ending)

    return ''.join(out)


class CobolCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('*>'):
            return token[2:]
        if token.startswith('*'):
            return token[1:]


class CobolReader(CodeReader, CobolCommentsMixin):
    '''Reader for fixed-format COBOL.'''

    ext = ['cbl', 'cob', 'cobol']
    language_names = ['cobol']

    # Canonical uppercase forms; __init__ folds both cases for the shared
    # counter (tokens keep source casing under (?i) matches). Conditional
    # PERFORM is counted via UNTIL / TIMES / VARYING, not PERFORM itself.
    _control_flow_keywords = {
        'IF', 'ELSE IF', 'ELSEIF', 'UNTIL', 'TIMES', 'VARYING', 'AT END',
    }
    _logical_operators = {'AND', 'OR'}
    _case_keywords = {'WHEN'}
    _ternary_operators = set()

    def __init__(self, context):
        super(CobolReader, self).__init__(context)
        self.parallel_states = [CobolStates(context)]
        self.conditions = (
            self.conditions
            | {c.upper() for c in self.conditions}
            | {c.lower() for c in self.conditions})

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        source_code = preprocess_fixed_format(source_code)
        addition = (
            r'(?i)|'
            r'\*>' + _UNTIL_END + r'|'
            r'ELSE\s+IF|'
            r'WHEN\s+OTHER|'
            r'AT\s+END|'
            r'END\s+IF|'
            r'END\s+EVALUATE|'
            r'END\s+PERFORM|'
            r'END\s+SEARCH|'
            + _COBOL_WORD
            + addition
        )
        return CodeReader.generate_tokens(
            source_code, addition=addition, token_class=token_class)

    def preprocess(self, tokens):
        """Recognize Area-A headers while newlines/spaces are still visible.

        ``line_counter`` drops ``\\n`` before the state machine runs, so
        paragraph/section detection must happen here.
        """
        line = []
        for token in tokens:
            if token == '\n':
                for emitted in self._emit_line(line):
                    yield emitted
                yield '\n'
                line = []
            else:
                line.append(token)
        for emitted in self._emit_line(line):
            yield emitted

    def _emit_line(self, line_tokens):
        leading = 0
        idx = 0
        while idx < len(line_tokens) and line_tokens[idx].isspace():
            leading += len(line_tokens[idx])
            idx += 1
        body = [t for t in line_tokens[idx:] if not t.isspace()]
        if not body:
            return

        # Area A: first non-space within columns 8-11 → leading < 4.
        if leading < 4 and self._is_name_token(body[0]):
            if len(body) >= 3 and body[1].upper() == 'SECTION' and body[2] == '.':
                yield SECTION
                yield body[0]
                for token in body[3:]:
                    yield token
                return
            if len(body) >= 2 and body[1] == '.':
                # Paragraph header, or DIVISION / PROGRAM-ID style headers.
                # Leave DIVISION and PROGRAM-ID as ordinary tokens for the SM.
                first_upper = body[0].upper()
                if first_upper not in DIVISION_STARTERS and \
                        first_upper != 'PROGRAM-ID':
                    yield PARAGRAPH
                    yield body[0]
                    for token in body[2:]:
                        yield token
                    return

        for token in body:
            yield token

    @staticmethod
    def _is_name_token(token):
        return bool(token) and token != '.' and token[0].isalnum()

    def eof(self):
        for state in self.parallel_states:
            state.finish()
