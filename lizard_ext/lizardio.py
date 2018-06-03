'''
Fan in and Fan out (again)
'''
from collections import Counter
from .extension_base import ExtensionBase


def get_all_indices(value, token_list):
    indices = []
    idx = -1
    while True:
        try:
            idx = token_list.index(value, idx + 1)
            indices.append(idx)
        except ValueError:
            break
    return indices


class LizardExtension(ExtensionBase):
    '''
        Runs the Lizard Extension for the calculation of structural
        fan-in and fan-out of every procedures in the source code.

        Explanation of terms:
        'ExtensionBase': Base class for all lizard extensions

        Calculates:
                'fan_in':                     Number of structural fan-in
                'fan_out':		              Number of structural fan-out
                                              in the local scope
                'general_fan_out'             Number of structural fan-out
                                              in the global scope
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
        self.all_methods = {}
        super(LizardExtension, self).__init__(context)

    def _state_global(self, token):
        if not hasattr(self.context.current_function, 'tokens'):
            self.context.current_function.tokens = list()
        self.context.current_function.tokens.append(token)

    def cross_file_process(self, fileinfos):
        for fileinfo in fileinfos:
            try:
                new_funcs = {f.unqualified_name: f for f in
                             fileinfo.function_list}
                self.all_methods.update(new_funcs)
                self._add_to_fan_outs(new_funcs.keys())
                self._add_to_general_fan_out()
                self._add_to_fan_ins(fileinfo.function_list)
            except (AttributeError, TypeError, ValueError):
                pass
            yield fileinfo

    def _add_to_fan_outs(self, keys):
        for other_func in self.all_methods.values():
            intersect = Counter(keys) & Counter(other_func.tokens)
            other_func.fan_out += len(list(intersect.elements()))

    def _add_to_general_fan_out(self):
        structures = set(['if', 'else', 'elif', 'for', 'foreach', 'while',
                          'do', 'try', 'catch', 'switch', 'finally',
                          'except', 'with'])
        punctuations = set(['(', ')', '{', '}'])
        for other_func in self.all_methods.values():
            bracket_indexes = get_all_indices('(', other_func.tokens)
            for idx in bracket_indexes[1:]:
                # Suggestion ?
                if other_func.tokens[idx - 1] not in (structures |
                                                      punctuations):
                    other_func.general_fan_out += 1

    def _add_to_fan_ins(self, new_funcs):
        for func in new_funcs:
            for name in func.tokens:
                if name in self.all_methods:
                    self.all_methods[name].fan_in += 1
