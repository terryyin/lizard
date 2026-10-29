'''
State machine for COBOL PROCEDURE DIVISION paragraphs.
'''

from .code_reader import CodeStateMachine


# Synthetic tokens emitted by CobolReader.preprocess (newlines never reach
# the state machine).
PARAGRAPH = '@COBOL_PARAGRAPH'
SECTION = '@COBOL_SECTION'

DIVISION_STARTERS = {
    'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE', 'ID',
}


class CobolStates(CodeStateMachine):
    '''Paragraph-as-Function mapping for PROCEDURE DIVISION.'''

    def __init__(self, context):
        super(CobolStates, self).__init__(context)
        self.program_id = None
        self.in_procedure = False
        self.in_function = False
        self.waiting_program_id = False
        self._current_division = None

    def finish(self):
        self._end_function()

    def statemachine_before_return(self):
        self.finish()

    def _end_function(self):
        if self.in_function:
            self.context.end_of_function()
            self.in_function = False

    def _start_function(self, name):
        self._end_function()
        self.context.restart_new_function(name)
        self.in_function = True

    def _ensure_preamble_function(self):
        if not self.in_function and self.in_procedure:
            name = self.program_id if self.program_id else '(procedure)'
            self._start_function(name)

    def reset_state(self, token=None):
        self._state = self._state_global
        if token is not None:
            self._state_global(token)

    def _state_global(self, token):
        token_upper = token.upper() if token else token

        if self.waiting_program_id:
            if token != '.':
                self.program_id = token
                self.waiting_program_id = False
            return

        if token == PARAGRAPH:
            if self.in_procedure:
                self.next(self._paragraph_name)
            return

        if token == SECTION:
            if self.in_procedure:
                self._end_function()
            self.next(self._skip_section_name)
            return

        if token_upper == 'PROGRAM-ID':
            self.waiting_program_id = True
            return

        if token_upper in DIVISION_STARTERS:
            self._current_division = token_upper
            self.next(self._expect_division_keyword)
            return

        if self.in_procedure and token not in ('.',):
            self._ensure_preamble_function()

    def _paragraph_name(self, token):
        self._start_function(token)
        self.reset_state()

    def _skip_section_name(self, token):
        # Name already consumed; just return to global.
        self.reset_state()

    def _expect_division_keyword(self, token):
        if token.upper() == 'DIVISION':
            self.next(self._after_division_keyword)
            return
        # e.g. PROCEDURE used as something else — uncommon after Area-A filter.
        saved = self._current_division
        self._current_division = None
        self.reset_state()
        if self.in_procedure:
            self._start_function(saved)

    def _after_division_keyword(self, token):
        # Optional period terminator from unrewritten DIVISION lines.
        if token == '.':
            self._apply_division_change()
            self.reset_state()
            return
        self._apply_division_change()
        self.reset_state(token)

    def _apply_division_change(self):
        division = (self._current_division or '').upper()
        self._current_division = None
        if division == 'PROCEDURE':
            self._end_function()
            self.in_procedure = True
        else:
            if self.in_procedure:
                self._end_function()
            self.in_procedure = False
