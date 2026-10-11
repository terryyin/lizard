"""Tokens that increment cognitive complexity in brace languages."""

from .cognitive_counter import BODY, BODY_PENDING, HEAD, _IF_LIKE
from .cognitive_profile import (
    _EXPRESSION_BREAKERS, _NOT_TERNARY_NEXT, _NULLABLE_TYPE_FOLLOWERS,
    _is_word)


class _BraceActions(object):
    """Keyword increments, lookaheads, and languages without brace blocks."""

    def _keyword_actions(self, profile):
        actions = {'{': self._open_block, '}': self._close_block,
                   ';': self._semicolon, 'else': self._else}
        for tokens, action in ((profile.ifs, self._if),
                               (profile.else_ifs, self._else_if),
                               (profile.loops, self._loop),
                               (profile.do_likes, self._do),
                               (profile.switches, self._switch),
                               (profile.catches, self._catch),
                               (profile.gotos, self._fundamental)):
            for token in tokens:
                actions[token] = action
        if profile.ternary:
            actions['?'] = self._question
        return actions

    def _if(self):
        self._structural()
        self._push('if', HEAD)

    def _else_if(self):
        self._hybrid()
        self._push('else if', HEAD)

    def _else(self):
        if self._in_switch_body():
            return                      # Kotlin ``when { else -> }``
        frames = self.state.frames
        while frames and frames[-1].kind == 'ternary':
            frames.pop()
        # ``if (a) b else c`` without ';' (Kotlin, Scala, R ...)
        if frames and frames[-1].kind in _IF_LIKE and \
                frames[-1].phase == BODY and not frames[-1].braced:
            frames.pop()
        self._hybrid()
        self._push('else', BODY_PENDING)

    def _loop(self):
        if self.state.do_tail:
            self.state.do_tail = False  # the ``while`` of ``do {} while``
        else:
            self._structural()
            self._push('loop', HEAD)

    def _do(self):
        self._structural()
        self._push('do', BODY_PENDING)

    def _switch(self):
        self._structural()
        self._push('switch', HEAD)

    def _catch(self):
        self._structural()
        self._push('catch', HEAD)

    def _question(self):
        state = self.state
        if state.prev not in ('<', ',', '?'):
            state.pending_ternary = 1
            state.pending_ternary_line = self.context.current_line

    # -- one and two token lookaheads ----------------------------------------------

    def _resolve_pending_ternary(self, token):
        state = self.state
        if state.pending_ternary == 1:
            state.pending_ternary = 0
            if token in _NOT_TERNARY_NEXT:
                return
            if self.profile.nullable_types:
                if self.context.current_line != state.pending_ternary_line:
                    return
                if _is_word(token):     # ``T? x`` or ``c ? x : y``
                    state.pending_ternary = 2
                    return
        else:
            state.pending_ternary = 0
            if token in _NULLABLE_TYPE_FOLLOWERS:
                return
        self._structural()
        self._push('ternary', BODY)

    def _resolve_pending_logical(self, token):
        """C++: ``T&& x = ...`` and ``auto&& x : v`` are not logical ands."""
        state = self.state
        if not state.pending_logical_word and _is_word(token):
            state.pending_logical_word = True
            return True
        operator = state.pending_logical
        state.pending_logical = None
        if not (state.pending_logical_word and token in ('=', ':')):
            self._logical_operator(operator)
        state.pending_logical_word = False
        return False

    def _scan_lambda(self, token):
        """Detect lambda bodies, which nest without an increment."""
        state = self.state
        if state.lambda_scan is None:
            if token == ']':
                state.lambda_scan = 'after_capture'
            elif not self._in_switch_body():        # ``case 1 -> {`` is not
                state.lambda_scan = 'after_params'
            return False
        if state.lambda_scan == 'after_capture':
            if token == '(':
                state.lambda_scan = 'params'
                state.lambda_pdepth = state.pdepth - 1
                return False
            state.lambda_scan = None
            if token == '{':
                self._open_lambda()
                return True
            return False
        if state.lambda_scan == 'params':
            if token == ')' and state.pdepth == state.lambda_pdepth:
                state.lambda_scan = 'after_params'
            return False
        # after_params: ``mutable``, ``noexcept``, ``-> T`` ... then ``{``
        if token == '{':
            state.lambda_scan = None
            self._open_lambda()
            return True
        if token in (';', ',', ')', ']', '}', '?', ':', '(', '='):
            state.lambda_scan = None
        return False

    def _open_lambda(self):
        self._open_braced_body(self._push('lambda', BODY))

    # -- languages without brace-delimited blocks -------------------------------

    def _process_flat(self, token):
        state = self.state
        profile = self.profile
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if token in self.logical_ops:
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS:
            state.logical.pop(state.pdepth, None)
        if token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
        if state.pending_ternary:
            state.pending_ternary = 0
            if token not in _NOT_TERNARY_NEXT:
                self._structural()
        if token in profile.ifs:
            if state.prev != 'else':    # ``ELSE IF`` is one hybrid increment
                self._structural()
        elif token in profile.else_ifs or token == 'else' or \
                token in profile.loops or token in profile.do_likes or \
                token in profile.switches or token in profile.catches:
            self._structural()
        elif token in profile.gotos:
            self._fundamental()
        elif token == '?' and profile.ternary:
            state.pending_ternary = 1
