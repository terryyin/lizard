"""Indentation nesting for cognitive complexity, and counter selection."""

from lizard_languages.python import PythonReader

from .cognitive_brace import _BraceCounter
from .cognitive_counter import BODY, _Counter, _EXPRESSION_FRAMES, _Frame
from .cognitive_profile import (
    _EXPRESSION_BREAKERS, _GATED_LANGUAGES, _PROFILES, _PYTHON, _Profile,
    _is_word)


class _IndentCounter(_Counter):
    """Nesting from indentation, through the nesting level lizard's Python
    reader derives from it (Python, GDScript)."""

    def _process(self, token):
        state = self.state
        if token == '\\\n':
            state.continued = True
            return
        statement_start = self._track_newline()
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if token in self.logical_ops:
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS or token in ('if', 'else', 'lambda'):
            state.logical.pop(state.pdepth, None)
        if token in ('(', '[', '{'):
            self._open_bracket()
        elif token in (')', ']', '}'):
            self._close_bracket()
            frames = state.frames
            while frames and frames[-1].kind in _EXPRESSION_FRAMES and \
                    frames[-1].pdepth > state.pdepth:
                frames.pop()
            while state.pending_ifs and state.pending_ifs[-1] > state.pdepth:
                state.pending_ifs.pop()    # comprehension filter: no increment
        if statement_start:
            self._statement_keyword(token)
        else:
            self._expression_keyword(token)

    def _track_newline(self):
        """Close the blocks the indentation closed; True at a statement start."""
        state = self.state
        line = self.context.current_line
        new_line = line != state.line and not state.continued
        state.continued = False
        state.line = line
        statement_start = state.after_async
        state.after_async = False
        if new_line and not state.pdepth:
            level = self.context.current_nesting_level
            frames = state.frames
            while frames and (frames[-1].kind in _EXPRESSION_FRAMES or
                              frames[-1].level >= level):
                frames.pop()
            state.logical.clear()
            statement_start = True
        return statement_start

    def _statement_keyword(self, token):
        profile = self.profile
        if token == 'async':
            self.state.after_async = True
        elif token in profile.ifs or token in profile.loops or \
                token in profile.catches or \
                (token in profile.switches and self._match_is_keyword()):
            self._structural()
            self._push(token)
        elif token in profile.else_ifs or token == 'else':
            self._hybrid()
            self._push(token)

    def _expression_keyword(self, token):
        state = self.state
        if token == 'if':
            if state.pdepth:
                state.pending_ifs.append(state.pdepth)   # ternary or comprehension
            else:
                self._structural()
                self._push_expression('ternary')
        elif token == 'else' and state.pending_ifs and \
                state.pending_ifs[-1] == state.pdepth:
            state.pending_ifs.pop()
            self._structural()
            self._push_expression('ternary')
        elif token == 'for' and state.pending_ifs and \
                state.pending_ifs[-1] == state.pdepth:
            state.pending_ifs.pop()     # ``[x for x in y if c for z in w]``
        elif token == 'lambda':
            self._push_expression('lambda')

    def _match_is_keyword(self):
        return getattr(self.reader, '_keyword_match', True)

    def _push(self, kind):
        self.state.frames.append(
            _Frame(kind, BODY, level=self.context.current_nesting_level))

    def _push_expression(self, kind):
        self.state.frames.append(_Frame(kind, BODY, pdepth=self.state.pdepth))


def _counter_for(reader):
    if isinstance(reader, PythonReader):
        return _IndentCounter(reader, _PYTHON.for_reader(reader), gated=True)
    name = reader.language_names[0]
    profile = _PROFILES.get(name, _Profile()).for_reader(reader)
    return _BraceCounter(reader, profile, gated=name in _GATED_LANGUAGES)
