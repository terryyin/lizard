'''
Language parser for GDSCript
'''
from .python import PythonReader, PythonStates


class GDScriptReader(PythonReader):
    # pylint: disable=R0903

    ext = ['gd']
    language_names = ['GDScript']
    
    # Separated condition categories
    _control_flow_keywords = {'if', 'elif', 'for', 'while', 'catch', 'do'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(GDScriptReader, self).__init__(context)
        self.parallel_states = [GDScriptStates(context, self)]


class GDScriptStates(PythonStates):  # pylint: disable=R0903
    def _state_global(self, token):
        if token == 'func':
            self._state = self._function
