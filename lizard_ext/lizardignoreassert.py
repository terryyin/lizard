'''
This is an extension of lizard, that ignores the CCN within
the assertion.
'''
from lizard_languages.code_reader import CodeStateMachine
from .extension_base import ExtensionBase


class _IgnoredAnd(str):
    is_ignored_condition = True


_IGNORED_AND = _IgnoredAnd("&&")


class LizardExtension(ExtensionBase):  # pylint: disable=R0903

    def __init__(self):
        super(LizardExtension, self).__init__(None)
        self.conditions = set()
        self._ignored_and = False

    def __call__(self, tokens, reader=None):
        self._start_file(reader)
        self.conditions = reader.conditions
        self._ignored_and = False
        for token in tokens:
            self._state(token)
            if self._ignored_and:
                self._ignored_and = False
                token = _IGNORED_AND
            yield token

    def _state_global(self, token):
        if token in ("assert", "static_assert"):
            self._state = self.in_assertion

    @CodeStateMachine.read_inside_brackets_then("()", "_state_global")
    def in_assertion(self, token):
        if token in ("&&", "||", "?", "??") and token in self.conditions \
                and not getattr(token, 'is_optional', False):
            self.context.add_condition(-1)
            self._ignored_and = token == "&&"
