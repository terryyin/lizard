'''
Fan in and Fan out (again)
'''
from .extension_base import ExtensionBase


class LizardExtension(ExtensionBase):

    FUNCTION_CAPTION = ["  fan_in  ", "  fan_out  "]
    FUNCTION_INFO_PART = ["fan_in", "fan_out"]

    def _state_global(self, token):
        if not hasattr(self.context.current_function, 'tokens'):
            self.context.current_function.tokens = set()
        self.context.current_function.tokens.add(token)

    @staticmethod
    def reduce(fileinfo):
        all_func_names = (f.unqualified_name for f in fileinfo.function_list)
        all_func_names = set(all_func_names)
        for func in fileinfo.function_list:
            func.fan_out = len(func.tokens.intersection(all_func_names))
