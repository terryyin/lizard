"""
The file information builder (the "context" language readers drive) and the
nesting stack it uses to track namespaces and functions while reading.
"""
from lizard_core.function_info import BARE_NESTING, Namespace, FunctionInfo, \
    FileInformation


class NestingStack(object):

    def __init__(self):
        self.nesting_stack = []
        self.pending_function = None

    def with_namespace(self, name):
        return ''.join([x.name_in_space for x in self.nesting_stack] + [name])

    def add_bare_nesting(self):
        self.nesting_stack.append(self._create_nesting())

    def add_namespace(self, token):
        self.pending_function = None
        self.nesting_stack.append(Namespace(token))

    def start_new_function_nesting(self, function):
        self.pending_function = function

    def _create_nesting(self):
        tmp = self.pending_function
        self.pending_function = None
        if tmp:
            return tmp
        return BARE_NESTING

    def pop_nesting(self):
        self.pending_function = None
        if self.nesting_stack:
            return self.nesting_stack.pop()

    @property
    def current_nesting_level(self):
        return len(self.nesting_stack)

    @property
    def last_function(self):
        funs = [f for f in self.nesting_stack if isinstance(f, FunctionInfo)]
        return funs[-1] if funs else None


class FileInfoBuilder(object):
    '''
    The builder is also referred as "context" in the code,
    because each language readers use this builder to build
    source file and function information and the builder keep
    the context information that's needed for the building.
    '''

    def __init__(self, filename):
        self.fileinfo = FileInformation(filename, 0)
        self.current_line = 0
        self.forgive = False
        self.forgive_global = False
        self.newline = True
        self.global_pseudo_function = FunctionInfo('*global*', filename, 0)
        self.current_function = self.global_pseudo_function
        self.stacked_functions = []
        self._nesting_stack = NestingStack()

    def __getattr__(self, attr):
        # delegating to _nesting_stack
        return getattr(self._nesting_stack, attr)

    def decorate_nesting_stack(self, decorate_class):
        self._nesting_stack = decorate_class(self._nesting_stack)
        return self._nesting_stack

    def pop_nesting(self):
        nest = self._nesting_stack.pop_nesting()
        if isinstance(nest, FunctionInfo):
            endline = self.current_function.end_line
            self.end_of_function()
            self.current_function = (
                    self._nesting_stack.last_function or
                    self.global_pseudo_function)
            self.current_function.end_line = endline

    def add_nloc(self, count):
        self.fileinfo.nloc += count
        self.current_function.nloc += count
        self.current_function.end_line = self.current_line
        self.newline = count > 0

    def try_new_function(self, name):
        self.current_function = FunctionInfo(
            self.with_namespace(name),
            self.fileinfo.filename,
            self.current_line)
        self.current_function.top_nesting_level = self.current_nesting_level

    def confirm_new_function(self):
        self.start_new_function_nesting(self.current_function)
        self.current_function.cyclomatic_complexity = 1

    def restart_new_function(self, name):
        self.try_new_function(name)
        self.confirm_new_function()

    def push_new_function(self, name):
        self.stacked_functions.append(self.current_function)
        self.restart_new_function(name)

    def add_condition(self, inc=1):
        self.current_function.cyclomatic_complexity += inc

    def add_to_long_function_name(self, app):
        self.current_function.add_to_long_name(app)

    def add_to_function_name(self, app):
        self.current_function.add_to_function_name(app)

    def parameter(self, token):
        self.current_function.add_parameter(token)

    def end_of_function(self):
        if not self.forgive:
            if self.current_function.name != '*global*' or not self.forgive_global:
                self.fileinfo.function_list.append(self.current_function)
        self.forgive = False
        if self.stacked_functions:
            self.current_function = self.stacked_functions.pop()
        else:
            self.current_function = self.global_pseudo_function
