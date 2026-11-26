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
    _logical_operators = {'&&', '||', 'and', 'or'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(GDScriptReader, self).__init__(context)
        self.parallel_states = [GDScriptStates(context, self)]


class GDScriptStates(PythonStates):  # pylint: disable=R0903
    def _state_global(self, token):
        if token == 'func':
            self._state = self._function

    def _function(self, token):
        context = self.context
        previous = context.current_function
        super()._function(token)
        function = context.current_function
        if function is not previous and function.name.count('.') > 1:
            enclosing = context.last_function
            if enclosing is not None:
                # The nearest enclosing function already has the full prefix.
                function.name = function.long_name = enclosing.name_in_space + token
