"""Base stuff for extensions"""
from lizard_languages.code_reader import CodeStateMachine


class ExtensionBase(CodeStateMachine):
    """Base class for all lizard extensions"""

    def __call__(self, tokens, reader=None):
        self._start_file(reader)
        for token in tokens:
            self._state(token)
            yield token

    def _start_file(self, reader):
        """One extension object reads all the files, and a file can end in
        any state: each file starts in the global state again."""
        CodeStateMachine.__init__(self, reader.context)
