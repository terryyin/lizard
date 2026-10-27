"""
The analysis data model: nesting levels, namespaces, functions and their
metrics, per-file information, and the aggregated result over all files.
"""
import re
import itertools


class Nesting(object):  # pylint: disable=R0903
    '''
    Nesting represent one level of nesting in any programming language.
    '''
    @property
    def name_in_space(self):
        return ''


BARE_NESTING = Nesting()


class Namespace(Nesting):  # pylint: disable=R0903

    def __init__(self, name):
        self.name = name

    @property
    def name_in_space(self):
        return self.name + "::" if self.name else ''


class FunctionInfo(Nesting):  # pylint: disable=R0902

    def __init__(self, name, filename, start_line=0, ccn=1):
        self.cyclomatic_complexity = ccn
        self.nloc = 1
        self.token_count = 1  # the first token
        self.name = name
        self.long_name = name
        self.start_line = start_line
        self.end_line = start_line
        self.full_parameters = []
        self.filename = filename
        self.top_nesting_level = -1
        self.fan_in = 0
        self.fan_out = 0
        self.general_fan_out = 0
        self.max_nesting_depth = 0  # Initialize max_nesting_depth to 0
        self.forgiven_metrics = set()

    @property
    def name_in_space(self):
        return self.name + "."

    @property
    def unqualified_name(self):
        '''
        name without qualification like namespaces or classes.
        Just the bare name without '::'.
        '''
        return self.name.split('::')[-1]

    location = property(lambda self:
                        f" {self.name}@{self.start_line}-{self.end_line}@{self.filename}")

    @property
    def parameter_count(self):
        # Binding patterns can occupy formal slots without introducing names.
        return len(self.parameters) + sum(
            parameter.strip() in ('{}', '[]')
            for parameter in self.full_parameters)

    @property
    def parameters(self):
        # Exclude empty tokens as parameters. These can occur in languages
        # allowing a trailing comma on the last parameter in an function
        # argument list.
        # Regex matches the parameter name, then optionally:
        # - a default value given after an '=' sign
        # - a type annotation given after a ':'
        matches = [re.search(r'([\w$]+)(\s=.*)?(\s:.*)?$', f)
                   for f in self.full_parameters]
        return [m.group(1) for m in matches if m]

    @property
    def length(self):
        return self.end_line - self.start_line + 1

    def add_to_function_name(self, app):
        self.name += app
        self.long_name += app

    def add_to_long_name(self, app):
        if self.long_name:
            if self.long_name[-1].isalpha() and app[0].isalpha():
                self.long_name += ' '
        self.long_name += app

    def add_parameter(self, token):
        self.add_to_long_name(" " + token)

        if not self.full_parameters:
            self.full_parameters.append(token)
        elif token == ",":
            self.full_parameters.append('')
        else:
            self.full_parameters[-1] += " " + token


class FileInformation(object):  # pylint: disable=R0903

    def __init__(self, filename, nloc, function_list=None):
        self.filename = filename
        self.nloc = nloc
        self.function_list = function_list or []
        self.token_count = 0

    average_nloc = property(lambda self: self.functions_average("nloc"))
    average_token_count = property(
        lambda self: self.functions_average("token_count"))
    average_cyclomatic_complexity = property(
        lambda self: self.functions_average("cyclomatic_complexity"))
    CCN = property(
        lambda self:
        sum(fun.cyclomatic_complexity for fun in self.function_list))
    ND = property(  # pylint: disable=C0103
        lambda self:
        sum(fun.max_nesting_depth for fun in self.function_list))

    def functions_average(self, att):
        summary = sum(getattr(fun, att) for fun in self.function_list)
        return summary / len(self.function_list) if self.function_list else 0


class AllResult(object):
    def __init__(self, result):
        self.result = list(file_info for file_info in result if file_info)
        self.all_fun = list(itertools.chain(*(file_info.function_list
                                            for file_info in self.result)))

    def function_count(self):
        return len(self.all_fun) or 1

    def nloc_in_functions(self):
        return sum([f.nloc for f in self.all_fun]) or 1

    def as_fileinfo(self):
        return FileInformation(
                    "",
                    sum([f.nloc for f in self.result]),
                    self.all_fun)
