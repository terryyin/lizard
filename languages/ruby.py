'''
Language parser for JavaScript
'''

from lizard import CodeReader
from .script_language import ScriptLanguageMixIn


class RubyStateMachine(object):
    # pylint: disable=R0903
    def __init__(self, context):
        self.context = context
        self.saved_state = self._state = self._state_global

    def _state_global(self, token):
        if token == 'def':
            self._state = self._function
            self.next(self._function)
        elif token == "end":
            return True

    def _function(self, token):
        self.context.start_new_function(token)
        self.next(self._impl)

    def _impl(self, token):
        if token == "end":
            self.context.end_of_function()
            self.next(self._state_global)
        elif token in ("begin", "do"):
            self.sub_state(RubyStateMachine(self.context))

    def next(self, state, token=None):
        self._state = state
        if token is not None:
            self._state(token)

    def sub_state(self, state):
        self.saved_state = self._state
        self.next(state)

    def __call__(self, token):
        if self._state(token):
            self.next(self.saved_state)
            return True


class RubyReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

    def __init__(self, context):
        super(RubyReader, self).__init__(context)
        self._state = RubyStateMachine(context)
