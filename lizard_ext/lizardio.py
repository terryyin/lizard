'''
Fan in and Fan out
'''

from lizard_languages import get_reader_for

from .extension_base import ExtensionBase


class LizardExtension(ExtensionBase):
    '''
        Runs the Lizard Extension for calculation of fan-in and fan-out
        metric in the source code.

        Currently supported languages:
            - CLike:
                - Fan-In:          The count of functions this function is called.
                - Fan-Out:         The count of function called in this function.
                - General Fan-Out: The count of functions called in this function.
            - Python
                - Fan-In:          The count of functions this function is called.
                - Fan-Out:         The count of functions called in this function.
                - General Fan-Out: The count of different functions called in this function.

        Metric definition:
            Fan-In:          The grade of units/entities dependent on this code part inside the unit.
            Fan-Out:         The grade of units/entities this code part depends on.
            General Fan-Out: A more general grade of units/entities this code part depends on.

        Limitation:
            - Metric definition depends on language: Functional languages this metric is different
              calculated than object-oriented languages (because of language design).
            - Metric calculation depends on reader extension: Only CodeReader with implemented
              function `external_dependencies` are used to calculate the fan-out metric.
            - Exact metric calculation depends on not-existent knowledge: Programming languages can
              own a lot of features which are not known at calculation time, e.g.:
                - defines/templates (C/C++)
                - compiler/interpreter introduced hidden function calls (SPARC, x86, PowerPC)
                - different language versions (python {2.7/3.11/...}, C{99/11/..}
            - Cross file calculation is no problem on simple fan-out, but for
    '''

    FUNCTION_INFO = {
        "fan_in": {
            "caption": " fan_in ",
            "average_caption": " avg_fan_in "},
        "fan_out": {
            "caption": " fan_out ",
            "average_caption": " avg_fan_out "},
        "general_fan_out": {
            "caption": " general_fan_out ",
            "average_caption": " avg_general_fan_out "}
    }

    def __init__(self, context=None):
        super(LizardExtension, self).__init__(context)

    def _state_global(self, token):
        if not hasattr(self.context.current_function, 'tokens'):
            self.context.current_function.tokens = list()
        self.context.current_function.tokens.append(token)

    def cross_file_process(self, file_information_map):
        for _file in file_information_map:
            try:
                self._calculate_dependencies(_file)
            except (AttributeError, TypeError, ValueError):
                pass
            yield _file

    def _calculate_dependencies(self, fileinfo) -> None:
        """Compute Fan-Out"""
        try:
            _reader = get_reader_for(fileinfo.filename)
            _all_dependencies = []
            if not hasattr(_reader, 'external_dependencies'):
                for _function in fileinfo.function_list:
                    _function.general_fan_out = -1
                    _function.fan_out = -1
                    _function.fan_in = -1
            else:
                for _function in fileinfo.function_list:
                    _dependencies = _reader.external_dependencies(_function.tokens)
                    _function.general_fan_out = len(set(_dependencies))
                    _function.fan_out = len(_dependencies)
                    _all_dependencies.append(_dependencies)
                for _function in fileinfo.function_list:
                    _internal_calls = [x for x in _all_dependencies if x == _function.name]
                    _function.fan_in = len(_internal_calls)
        except Exception as e:
            print(e)
