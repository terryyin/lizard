#!/usr/bin/env python
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  author: terry@odd-e.com
#  Website: www.lizard.ws
#
"""
lizard is a simple code complexity analyzer without caring about the C/C++
header files or Java imports.
For more information visit http://www.lizard.ws
"""
from __future__ import print_function
import sys
import itertools
import re
import os
from fnmatch import fnmatch
import hashlib
if sys.version[0] == '2':
    from future_builtins import map, filter  # pylint: disable=W0622, F0401

VERSION = "1.8.12"

DEFAULT_CCN_THRESHOLD = 15
DEFAULT_WHITELIST = "whitelizard.txt"


def analyze(paths, exclude_pattern=None, threads=1, extensions=None):
    '''
    returns an iterator of file infomation that contains function
    statistics.
    '''
    exclude_pattern = exclude_pattern or []
    extensions = extensions or []
    files = get_all_source_files(paths, exclude_pattern)
    file_analyzer = FileAnalyzer(extensions)
    return map_files_to_analyzer(files, file_analyzer, threads)


def create_command_line_parser(prog=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(prog=prog)
    parser.add_argument('paths', nargs='*', default=['.'],
                        help='list of the filename/paths.')
    parser.add_argument('--version', action='version', version=VERSION)
    parser.add_argument("-V", "--verbose",
                        help="Output in verbose mode (long function name)",
                        action="store_true",
                        dest="verbose",
                        default=False)
    parser.add_argument("-C", "--CCN",
                        help='''Threshold for cyclomatic complexity number
                        warning. The default value is %d.
                        Functions with CCN bigger than it will generate warning
                        ''' % DEFAULT_CCN_THRESHOLD,
                        type=int,
                        dest="CCN",
                        default=DEFAULT_CCN_THRESHOLD)
    parser.add_argument("-a", "--arguments",
                        help="Limit for number of parameters",
                        type=int, dest="arguments", default=100)
    parser.add_argument("-w", "--warnings_only",
                        help='''Show warnings only, using clang/gcc's warning
                        format for printing warnings.
                        http://clang.llvm.org/docs/UsersManual.html#cmdoption-fdiagnostics-format
                        ''',
                        action="store_true",
                        dest="warnings_only",
                        default=False)
    parser.add_argument("-i", "--ignore_warnings",
                        help='''If the number of warnings is equal or less
                        than the number,
                        the tool will exit normally, otherwize it will generate
                        error. Useful in makefile for legacy code.''',
                        type=int,
                        dest="number",
                        default=0)
    parser.add_argument("-x", "--exclude",
                        help='''Exclude files that match this pattern. * matches
                        everything,
                        ? matches any single characoter, "./folder/*" exclude
                        everything in the folder recursively. Multiple patterns
                        can be specified. Don't forget to add "" around the
                        pattern.''',
                        action="append",
                        dest="exclude",
                        default=[])
    parser.add_argument("-X", "--xml",
                        help='''Generate XML in cppncss style instead of the
                        tabular output. Useful to generate report in Jenkins
                        server''',
                        action="store_true",
                        dest="xml",
                        default=None)
    parser.add_argument("-t", "--working_threads",
                        help='''number of working threads. The default
                        value is 1. Using a bigger
                        number can fully utilize the CPU and often faster.''',
                        type=int,
                        dest="working_threads",
                        default=1)
    parser.add_argument("-m", "--modified",
                        help="Calculate modified cyclomatic complexity number",
                        action="store_true",
                        dest="switchCasesAsOneCondition",
                        default=False)
    parser.add_argument("-E", "--extension",
                        help='''User the extensions. The available extensions
                        are: -Ecpre: it will ignore code in the #else branch.
                        -Ewordcount: count word fequencies and generate tag
                        cloud. -Eoutside: include the global code as one
                        function.
                        ''',
                        action="append",
                        dest="extensions",
                        default=[])
    parser.add_argument("-s", "--sort",
                        help='''Sort the warning with field. The field can be
                        nloc, cyclomatic_complexity, token_count,
                        parameter_count, etc. Or an customized file.''',
                        action="append",
                        dest="sorting",
                        default=[])
    parser.add_argument("-W", "--whitelist",
                        help='''The path and file name to the whitelist file.
                        It's './whitelizard.txt' by default.''',
                        type=str,
                        dest="whitelist",
                        default=DEFAULT_WHITELIST)

    parser.usage = '''lizard [options] [PATH or FILE] [PATH] ...'''
    parser.description = __doc__
    return parser


class FunctionInfo(object):  # pylint: disable=R0902

    def __init__(self, name, filename, start_line=0, ccn=1):
        self.cyclomatic_complexity = ccn
        self.nloc = 1
        self.token_count = 1  # the first token
        self.name = name
        self.long_name = name
        self.start_line = start_line
        self.end_line = start_line
        self.parameter_count = 0
        self.filename = filename
        self.indent = -1

    location = property(lambda self:
                        " %(name)s@%(start_line)s-%(end_line)s@%(filename)s"
                        % self.__dict__)

    def add_to_function_name(self, app):
        self.name += app
        self.long_name += app

    def add_to_long_name(self, app):
        self.long_name += app

    def add_parameter(self, token):
        self.add_to_long_name(" " + token)

        if self.parameter_count == 0:
            self.parameter_count = 1
        if token == ",":
            self.parameter_count += 1

    def clang_format_warning(self):
        return (
            "%(filename)s:%(start_line)s: warning: %(name)s has" +
            " %(cyclomatic_complexity)d CCN and %(parameter_count)d" +
            " params (%(nloc)d NLOC, %(token_count)d tokens)") % self.__dict__


class FileInformation(object):  # pylint: disable=R0903

    def __init__(self, filename, nloc, function_list=None):
        self.filename = filename
        self.nloc = nloc
        self.function_list = function_list or []
        self.token_count = 0

    average_NLOC = property(lambda self: self.functions_average("nloc"))
    average_token = property(
        lambda self: self.functions_average("token_count"))
    average_CCN = property(
        lambda self: self.functions_average("cyclomatic_complexity"))
    CCN = property(
        lambda self:
        sum(fun.cyclomatic_complexity for fun in self.function_list))

    def functions_average(self, att):
        summary = sum(getattr(fun, att) for fun in self.function_list)
        return summary / len(self.function_list) if self.function_list else 0


class CodeInfoContext(object):

    def __init__(self, filename):
        self.fileinfo = FileInformation(filename, 0)
        self.current_line = 0
        self.forgive = False
        self.newline = True
        self.global_pseudo_function = FunctionInfo('*global*', filename, 0)
        self.current_function = self.global_pseudo_function

    def add_nloc(self, count):
        self.current_function.nloc += count
        self.fileinfo.nloc += count

    def start_new_function(self, name):
        self.current_function = FunctionInfo(
            name,
            self.fileinfo.filename,
            self.current_line)

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
            self.fileinfo.function_list.append(self.current_function)
        self.forgive = False
        self.current_function = self.global_pseudo_function


def preprocessing(tokens, reader):
    if hasattr(reader, "preprocess"):
        return reader.preprocess(tokens)
    else:
        return (t for t in tokens if not t.isspace() or t == '\n')


def comment_counter(tokens, reader):
    get_comment = reader.get_comment_from_token
    for token in tokens:
        comment = get_comment(token)
        if comment is not None:
            for _ in comment.splitlines()[1:]:
                yield '\n'
            if comment.strip().startswith("#lizard forgive"):
                reader.context.forgive = True
        else:
            yield token


def line_counter(tokens, reader):
    reader.context.current_line = 1
    for token in tokens:
        if token != "\n":
            count = token.count('\n')
            reader.context.current_line += count
            reader.context.add_nloc(count)
            yield token
        else:
            reader.context.current_line += 1
            reader.context.newline = True


def token_counter(tokens, reader):
    for token in tokens:
        reader.context.fileinfo.token_count += 1
        if reader.context.newline:
            reader.context.add_nloc(1)
            reader.context.newline = False
        reader.context.current_function.end_line = reader.context.current_line
        reader.context.current_function.token_count += 1
        yield token


def condition_counter(tokens, reader):
    if hasattr(reader, "conditions"):
        conditions = reader.conditions
    else:
        conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                          'case'])
    for token in tokens:
        if token in conditions:
            reader.context.add_condition()
        yield token


def recount_switch_case(tokens, reader):
    for token in tokens:
        if token == 'switch':
            reader.context.add_condition()
        elif token == 'case':
            reader.context.add_condition(-1)
        yield token


class CodeReader(object):
    '''
    CodeReaders are used to parse functions structures from code of different
    language. Each language will need a subclass of CodeReader.
    '''
    def __init__(self, context):
        self.context = context
        self._state = lambda _: _

    @staticmethod
    def compile_file_extension_re(*exts):
        return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.IGNORECASE)

    @staticmethod
    def get_reader(filename):
        # pylint: disable=E1101
        for lan in list(CodeReader.__subclasses__()):
            if CodeReader.compile_file_extension_re(*lan.ext).match(filename):
                return lan

    def state(self, token):
        self._state(token)

    def eof(self):
        pass

    @staticmethod
    def generate_tokens(source_code, addition=''):
        def _generate_tokens(source_code, addition):
            # DONOT put any sub groups in the regex. Good for performance
            _until_end = r"(?:\\\n|[^\n])*"
            combined_symbals = ["||", "&&", "===", "!==", "==", "!=", "<=",
                                ">=",
                                "<<", ">>>", "++", ">>", "--", '+=', '-=',
                                '*=', '/=', '^=', '&=', '|=']
            token_pattern = re.compile(
                r"(?:\w+" +
                r"|/\*.*?\*/" +
                addition +
                r"|\"(?:\\.|[^\"\\])*\"" +
                r"|\'(?:\\.|[^\'\\])*?\'" +
                r"|//" + _until_end +
                r"|\#" +
                r"|:=|::|\*\*" +
                r"|" + r"|".join(re.escape(s) for s in combined_symbals) +
                r"|\\\n" +
                r"|\n" +
                r"|[^\S\n]+" +
                r"|.)", re.M | re.S)
            macro = ""
            for token in token_pattern.findall(source_code):
                if macro:
                    if "\\\n" in token or "\n" not in token:
                        macro += token
                    else:
                        yield macro
                        yield token
                        macro = ""
                elif token == "#":
                    macro = token
                else:
                    yield token
            if macro:
                yield macro

        return [t for t in _generate_tokens(source_code, addition)]


class CCppCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("/*") or token.startswith("//"):
            return token[2:]


try:
    # lizard.py can run as a stand alone script, without the extensions
    # The following langauages / extensions will not be supported in
    # stand alone script.
    # pylint: disable=W0611
    from lizard_ext import JavaScriptReader
    from lizard_ext import PythonReader
    from lizard_ext import xml_output
except ImportError:
    pass


# pylint: disable=R0903
class CLikeReader(CodeReader, CCppCommentsMixin):

    ''' This is the reader for C, C++ and Java. '''

    ext = ["c", "cpp", "cc", "mm", "cxx", "h", "hpp"]
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __init__(self, context):
        super(CLikeReader, self).__init__(context)
        self.bracket_stack = []
        self.br_count = 0
        self._state = self._state_global
        self._saved_tokens = []

    def preprocess(self, tokens):
        for token in tokens:
            if not token.isspace() or token == '\n':
                macro = self.macro_pattern.match(token)
                if macro:
                    if macro.group(1) in ('if', 'ifdef', 'elif'):
                        self.context.add_condition()
                    elif macro.group(1) == 'include':
                        yield "#include"
                        yield macro.group(2) or "\"\""
                    for _ in macro.group(2).splitlines()[1:]:
                        yield '\n'
                else:
                    yield token

    def _reset_to_global(self):
        self._state = self._state_global
        self.bracket_stack = []

    def _state_global(self, token):
        if token in ("class", "namespace"):
            self._state = self._state_namespace_def
        elif token == 'typedef':
            self._state = self._state_typedef
        elif token[0].isalpha() or token[0] == '_':
            self.context.start_new_function(token)
            self._state = self._state_function
            if token == 'operator':
                self._state = self._state_operator

    def _state_namespace_def(self, token):
        if token in '{;':
            self._state = self._state_global

    def _state_typedef(self, token):
        if token == ';':
            self._state = self._state_global

    def _state_function(self, token):
        if token == '(':
            self.bracket_stack.append(token)
            self._state = self._state_dec
            self.context.add_to_long_function_name(token)
        elif token == '::':
            self._state = self._state_namespace
        elif token == '<':
            self._state = self._state_template_in_name
            self.bracket_stack.append(token)
            self.context.add_to_function_name(token)
        else:
            self._state = self._state_global
            self._state_global(token)

    def _state_template_in_name(self, token):
        if token == "<":
            self.bracket_stack.append(token)
        elif token in (">", ">>"):
            for _ in token:
                if not self.bracket_stack or self.bracket_stack.pop() != "<":
                    self._reset_to_global()
        if not self.bracket_stack:
            self._state = self._state_function
        self.context.add_to_function_name(token)

    def _state_operator(self, token):
        if token != '(':
            self._state = self._state_operator_next
        self.context.add_to_function_name(' ' + token)

    def _state_operator_next(self, token):
        if token == '(':
            self._state_function(token)
        else:
            self.context.add_to_function_name(' ' + token)

    def _state_namespace(self, token):
        self._state = self._state_operator\
            if token == 'operator' else self._state_function
        self.context.add_to_function_name("::" + token)

    def _state_dec(self, token):
        if token in ('(', "<"):
            self.bracket_stack.append(token)
        elif token in (')', ">", ">>"):
            for sub in token:
                if self.bracket_stack.pop() != {')': '(', '>': '<'}[sub]:
                    self._reset_to_global()
                    return
            if not self.bracket_stack:
                self._state = self._state_dec_to_imp
        elif len(self.bracket_stack) == 1:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _state_dec_to_imp(self, token):
        if token == 'const':
            self.context.add_to_long_function_name(" " + token)
        elif token == 'throw':
            self._state = self._state_throw
        elif token == '(':
            long_name = self.context.current_function.long_name
            self.context.start_new_function(long_name)
            self._state_function(token)
        elif token == '{':
            self.br_count += 1
            self._state = self._state_imp
        elif token == ":":
            self._state = self._state_initialization_list
        elif not (token[0].isalpha() or token[0] == '_'):
            self._state = self._state_global
        else:
            self._state = self._state_old_c_params
            self._saved_tokens = [token]

    def _state_throw(self, token):
        if token == ')':
            self._state = self._state_dec_to_imp

    def _state_old_c_params(self, token):
        self._saved_tokens.append(token)
        if token == ';':
            self._saved_tokens = []
            self._state = self._state_dec_to_imp
        elif token == '{':
            if len(self._saved_tokens) == 2:
                self._saved_tokens = []
                self._state_dec_to_imp(token)
                return
            self._state = self._state_global
            for token in self._saved_tokens:
                self._state(token)
        elif token == '(':
            self._state = self._state_global
            for token in self._saved_tokens:
                self._state(token)

    def _state_initialization_list(self, token):
        if token == '{':
            self.br_count += 1
            self._state = self._state_imp

    def _state_imp(self, token):
        if token == '{':
            self.br_count += 1
        elif token == '}':
            self.br_count -= 1
            if self.br_count == 0:
                self._state = self._state_global
                self.context.end_of_function()


class JavaReader(CLikeReader, CodeReader):

    ext = ['java']

    def _state_old_c_params(self, token):
        if token == '{':
            self._state_dec_to_imp(token)

    def _state_global(self, token):
        if token == '@':
            self._state = self._state_decorator
            return
        super(JavaReader, self)._state_global(token)

    def _state_decorator(self, _):
        self._state = self._state_post_decorator

    def _state_post_decorator(self, token):
        if token == '.':
            self._state = self._state_decorator
        else:
            self._state = self._state_global
            self._state(token)


class ObjCReader(CLikeReader, CodeReader):

    ext = ['m']

    def __init__(self, context):
        super(ObjCReader, self).__init__(context)

    def _state_global(self, token):
        super(ObjCReader, self)._state_global(token)
        if token == '(':
            self.bracket_stack.append(token)
            self._state = self._state_dec
            self.context.add_to_long_function_name(token)

    def _state_dec_to_imp(self, token):
        if token in ("+", "-"):
            self._state = self._state_global
        else:
            super(ObjCReader, self)._state_dec_to_imp(token)
            if self._state != self._state_imp:
                self._state = self._state_objc_dec_begin
                self.context.start_new_function(token)

    def _state_objc_dec_begin(self, token):
        if token == ':':
            self._state = self._state_objc_dec
            self.context.add_to_function_name(token)
        elif token == '{':
            self.br_count += 1
            self._state = self._state_imp
        else:
            self._state = self._state_global

    def _state_objc_dec(self, token):
        if token == '(':
            self._state = self._state_objc_param_type
            self.context.add_to_long_function_name(token)
        elif token == ',':
            pass
        elif token == '{':
            self.br_count += 1
            self._state = self._state_imp
        else:
            self._state = self._state_objc_dec_begin
            self.context.add_to_function_name(" " + token)

    def _state_objc_param_type(self, token):
        if token == ')':
            self._state = self._state_objc_param
        self.context.add_to_long_function_name(" " + token)

    def _state_objc_param(self, _):
        self._state = self._state_objc_dec


def token_processor_for_function(tokens, reader):
    ''' token_processor_for_function parse source code into functions. This is
    different from language to language. So token_processor_for_function need
    a language specific 'reader' to actually do the job.
    '''
    for token in tokens:
        reader.state(token)
        yield token
    reader.eof()


class FileAnalyzer(object):  # pylint: disable=R0903

    def __init__(self, extensions):
        self.processors = extensions

    def __call__(self, filename):
        try:
            return self.analyze_source_code(
                filename, open(filename, 'rU').read())
        except IOError:
            sys.stderr.write("Error: Fail to read source file '%s'\n"
                             % filename)

    def analyze_source_code(self, filename, code):
        context = CodeInfoContext(filename)
        reader = (CodeReader.get_reader(filename) or CLikeReader)(context)
        tokens = reader.generate_tokens(code)
        for processor in self.processors:
            tokens = processor(tokens, reader)
        for _ in tokens:
            pass
        return context.fileinfo


def warning_filter(option, module_infos):
    for file_info in module_infos:
        if file_info:
            for fun in file_info.function_list:
                if fun.cyclomatic_complexity > option.CCN or \
                        fun.parameter_count > option.arguments:
                    yield fun


def whitelist_filter(warnings, script=None, whitelist=None):
    def _get_whitelist_item(script):
        white = {}
        pieces = script.replace('::', '##').split(':')
        if len(pieces) > 1:
            white['file_name'] = pieces[0]
            script = pieces[1]
        white['function_names'] = (
            [x.strip().replace('##', '::') for x in script.split(',')])
        return white

    def _in_list(warning):
        return any(_match_whitelist_item(white, warning)
                   for white in whitelist)

    def _match_whitelist_item(white, warning):
        return (warning.name in white['function_names'] and
                warning.filename == white.get('file_name', warning.filename))

    def get_whitelist(whitelist):
        if os.path.isfile(whitelist):
            return open(whitelist, mode='r').read()
        elif whitelist != DEFAULT_WHITELIST:
            print("WARNING: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            print("WARNING: the whitelist \""+whitelist+"\" doesn't existing.")
        return ''

    if not script:
        script = get_whitelist(whitelist)
    whitelist = [
        _get_whitelist_item(line.split('#')[0])
        for line in script.splitlines()]
    for warning in warnings:
        if not _in_list(warning):
            yield warning


class OutputScheme(object):

    def __init__(self, ext):
        self.extensions = ext
        self.items = [
            {'caption': "  NLOC  ", 'value': "nloc"},
            {'caption': "  CCN  ", 'value': "cyclomatic_complexity"},
            {'caption': " token ", 'value': "token_count"},
            {'caption': " PARAM ", 'value': "parameter_count"},
        ] + [
            {
                'caption': ext.FUNCTION_CAPTION,
                'value': ext.FUNCTION_INFO_PART
            }
            for ext in self.extensions if hasattr(ext, "FUNCTION_CAPTION")]
        self.items.append({'caption': " location  ", 'value': 'location'})

    def captions(self):
        return "".join(item['caption'] for item in self.items)

    def function_info_head(self):
        captions = self.captions()
        return "\n".join(("=" * len(captions), captions, "-" * len(captions)))

    def function_info(self, fun):
        return ''.join(
            str(getattr(fun, item['value'])).rjust(len(item['caption']))
            for item in self.items)


def print_warnings(option, scheme, warnings):
    warning_count = 0
    if isinstance(option.sorting, list) and len(option.sorting) > 0:
        warnings = list(warnings)
        warnings.sort(reverse=True,
                      key=lambda x: getattr(x, option.sorting[0]))
    if not option.warnings_only:
        print(("\n" +
               "======================================\n" +
               "!!!! Warnings (CCN > %d) !!!!") % option.CCN)
        print(scheme.function_info_head())
    for warning in warnings:
        warning_count += 1
        if option.warnings_only:
            print(warning.clang_format_warning())
        else:
            print(scheme.function_info(warning))
    return warning_count


def print_total(warning_count, saved_result, option):
    file_infos = list(file_info for file_info in saved_result if file_info)
    all_fun = list(itertools.chain(*(file_info.function_list
                                     for file_info in file_infos)))
    cnt = len(all_fun)
    if cnt == 0:
        cnt = 1
    nloc_in_functions = sum([f.nloc for f in all_fun])
    if nloc_in_functions == 0:
        nloc_in_functions = 1
    total_info = (
        sum([f.nloc for f in file_infos]),
        nloc_in_functions / cnt,
        float(sum([f.cyclomatic_complexity for f in all_fun])) / cnt,
        float(sum([f.token_count for f in all_fun])) / cnt,
        cnt,
        warning_count,
        float(warning_count) / cnt,
        float(sum([
            f.nloc for f in all_fun
            if f.cyclomatic_complexity > option.CCN
            ])) / nloc_in_functions
    )

    if not option.warnings_only:
        print("=" * 90)
        print("Total nloc  Avg.nloc  Avg CCN  Avg token  Fun Cnt  Warning" +
              " cnt   Fun Rt   nloc Rt  ")
        print("-" * 90)
        print("%10d%10d%9.2f%11.2f%9d%13d%10.2f%8.2f" % total_info)


def print_and_save_modules(all_modules, extensions, scheme):
    all_functions = []
    print(scheme.function_info_head())
    for module_info in all_modules:
        for extension in extensions:
            if hasattr(extension, 'reduce'):
                extension.reduce(module_info)
        if module_info:
            all_functions.append(module_info)
            for fun in module_info.function_list:
                print(scheme.function_info(fun))

    print("--------------------------------------------------------------")
    print("%d file analyzed." % (len(all_functions)))
    print("==============================================================")
    print("NLOC    Avg.NLOC AvgCCN Avg.ttoken  function_cnt    file")
    print("--------------------------------------------------------------")
    for module_info in all_functions:
        print((
            "{module.nloc:7d}" +
            "{module.average_NLOC:7.0f}" +
            "{module.average_CCN:7.1f}" +
            "{module.average_token:10.0f}" +
            "{function_count:10d}" +
            "     {module.filename}").format(
                module=module_info,
                function_count=len(module_info.function_list)))

    return all_functions


def print_result(code_infos, option):
    scheme = OutputScheme(option.extensions)
    if not option.warnings_only:
        code_infos = print_and_save_modules(
            code_infos, option.extensions, scheme)
    warnings = warning_filter(option, code_infos)
    warnings = whitelist_filter(warnings, whitelist=option.whitelist)
    warning_count = print_warnings(option, scheme, warnings)
    print_total(warning_count, code_infos, option)
    for extension in option.extensions:
        if hasattr(extension, 'print_result'):
            extension.print_result()
    if option.number < warning_count:
        sys.exit(1)


def print_xml(results, options):
    print(xml_output(list(results), options.verbose))


def get_map_method(working_threads):
    try:
        if working_threads == 1:
            raise ImportError
        import multiprocessing
        pool = multiprocessing.Pool(processes=working_threads)
        return pool.imap_unordered
    except ImportError:
        return map


def map_files_to_analyzer(files, file_analyzer, working_threads):
    mapmethod = get_map_method(working_threads)
    return mapmethod(file_analyzer, files)


def md5_hash_file(full_path_name):
    ''' return md5 hash of a file '''
    try:
        with open(full_path_name, mode='r') as source_file:
            if sys.version_info[0] == 3:
                code_md5 = hashlib.md5(source_file.read().encode('utf-8'))
            else:
                code_md5 = hashlib.md5(source_file.read())
        return code_md5.hexdigest()
    except IOError:
        return None


def get_all_source_files(paths, exclude_patterns):
    '''
    Function counts md5 hash for the given file
    and checks if it isn't a duplicate using set
    of hashes for previous files
    '''
    hash_set = set()

    def _validate_file(pathname):
        return (
            pathname in paths or (
                CodeReader.get_reader(pathname) and
                all(not fnmatch(pathname, p) for p in exclude_patterns) and
                _not_duplicate(pathname)))

    def _not_duplicate(full_path_name):
        fhash = md5_hash_file(full_path_name)
        if not fhash or fhash not in hash_set:
            hash_set.add(fhash)
            return True

    def all_listed_files(paths):
        for path in paths:
            if os.path.isfile(path):
                yield path
            else:
                for root, _, files in os.walk(path, topdown=False):
                    for filename in files:
                        yield os.path.join(root, filename)

    return filter(_validate_file, all_listed_files(paths))


def parse_args(argv):
    options = create_command_line_parser(argv[0]).parse_args(args=argv[1:])
    values = [
        item['value'] for item in OutputScheme([]).items]
    for sort_factor in options.sorting:
        if sort_factor not in values:
            error_message = "Wrong sorting field '%s'.\n" % sort_factor
            error_message += "Candidates are: " + ', '.join(values) + "\n"
            sys.stderr.write(error_message)
            sys.exit(2)
    return options


def get_extensions(extension_names, switch_case_as_one_condition=False):

    def expand_extensions(existing):
        for name in extension_names:
            ext = (import_module('lizard_ext.lizard' + name.lower())
                   .LizardExtension()
                   if isinstance(name, str) else name)
            existing.insert(
                len(existing) if not hasattr(ext, "ordering_index") else
                ext.ordering_index,
                ext)
        return existing

    from importlib import import_module
    extensions = [
        preprocessing,
        comment_counter,
        line_counter,
        condition_counter,
        token_counter,
        token_processor_for_function,
    ]
    if switch_case_as_one_condition:
        extensions.append(recount_switch_case)
    return expand_extensions(extensions)

analyze_file = FileAnalyzer(get_extensions([]))  # pylint: disable=C0103


def lizard_main(argv):
    options = parse_args(argv)
    options.extensions = get_extensions(options.extensions,
                                        options.switchCasesAsOneCondition)
    printer = print_xml if options.xml else print_result
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions)
    printer(result, options)

if __name__ == "__main__":
    lizard_main(sys.argv)
