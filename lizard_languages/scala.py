'''
Language parser for Scala
'''
from .clike import CLikeReader
from .swift import SwiftStates
__author__ = 'David Baum'


class ScalaReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['scala']
    language_names = ['scala']
    _conditions = set(['if', 'else', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', 'do'])

    def __init__(self, context):
        super(ScalaReader, self).__init__(context)
        self.parallel_states = [ScalaStates(context)]


class ScalaStates(SwiftStates):  # pylint: disable=R0903
    def _state_global(self, token):
        if token == 'def':
            self._state = self._function_name
        if token == 'main':
            self._function_name(token)
