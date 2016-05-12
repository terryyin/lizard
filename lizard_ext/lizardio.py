'''
Fan in and Fan out (again)
'''
from .extension_base import ExtensionBase


class LizardExtension(ExtensionBase):
    '''
    '''

    FUNCTION_REGRESSION_CAPTION = ["  fan_in  ", "  fan_out  ", " general_fan_out "]
    FUNCTION_INFO_PART = ["fan_in", "fan_out", "general_fan_out"]

    def __init__(self, context=None):
        self.all_methods = {}
        super(LizardExtension, self).__init__(context)

    def _state_global(self, token):
        if not hasattr(self.context.current_function, 'tokens'):
            self.context.current_function.tokens = set()
        self.context.current_function.tokens.add(token)

    def reduce(self, fileinfo):
        new_funcs = {f.unqualified_name: f for f in fileinfo.function_list}
        self.all_methods.update(new_funcs)
        self._add_to_fan_outs(new_funcs.keys())
        self._add_to_fan_ins(fileinfo.function_list)

    def _add_to_fan_outs(self, keys):
        for other_func in self.all_methods.values():
            other_func.fan_out += len(other_func.tokens.intersection(keys))

    def _add_to_fan_ins(self, new_funcs):
        for func in new_funcs:
            for name in func.tokens:
                if name in self.all_methods:
                    self.all_methods[name].fan_in += 1
