'''Base stuff for extensions'''
from lizard_languages.code_reader import CodeStateMachine


class ExtensionBase(CodeStateMachine):
    '''Base class for all lizard extensions'''

    # pylint: disable=W0221
    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            self._state(token)
            yield token
