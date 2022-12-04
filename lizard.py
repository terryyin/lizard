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
lizard is an extensible Cyclomatic Complexity Analyzer for many programming
languages including C/C++ (doesn't require all the header files).
For more information visit http://www.lizard.ws
"""
from __future__ import print_function, division
import codecs
import sys
import itertools
import re
import os
from fnmatch import fnmatch
import hashlib

if sys.version[0] == '2':
    from future_builtins import map, filter  # pylint: disable=W0622, F0401

try:
    from lizard_languages import languages, get_reader_for, CLikeReader
except ImportError:
    sys.stderr.write("Cannot find the lizard_languages module.")
    sys.exit(2)
try:
    from lizard_ext import version
    from lizard_ext import print_xml
    from lizard_ext import print_csv
    from lizard_ext import html_output
    from lizard_ext import auto_open, auto_read
except ImportError:
    sys.stderr.write("Cannot find the lizard_ext modules.")

DEFAULT_CCN_THRESHOLD, DEFAULT_WHITELIST, \
    DEFAULT_MAX_FUNC_LENGTH = 15, "whitelizard.txt", 1000


# pylint: disable-msg=too-many-arguments
def analyze(paths, exclude_pattern=None, threads=1, exts=None,
            lans=None):
    '''
    returns an iterator of file information that contains function
    statistics.
    '''
    exclude_pattern = exclude_pattern or []
    files = get_all_source_files(paths, exclude_pattern, lans)
    return analyze_files(files, threads, exts)


def analyze_files(files, threads=1, exts=None):
    extensions = exts or get_extensions([])
    file_analyzer = FileAnalyzer(extensions)
    result = map_files_to_analyzer(files, file_analyzer, threads)
    for extension in extensions:
        if hasattr(extension, 'cross_file_process'):
            result = extension.cross_file_process(result)
    return result


def _extension_arg(parser):
    parser.add_argument("-E", "--extension",
                        help='''User the extensions. The available extensions
                        are: -Ecpre: it will ignore code in the #else branch.
                        -Ewordcount: count word frequencies and generate tag
                        cloud. -Eoutside: include the global code as one
                        function.  -EIgnoreAssert: to ignore all code in
                        assert. -ENS: count nested control structures.''',
                        action="append",
                        dest="extensions",
                        default=[])


def arg_parser(prog=None):
    from argparse import ArgumentParser, Action, ArgumentError

    class DictAction(Action):  # pylint: disable=R0903
        def __init__(self, option_strings, dest, nargs=None, **kwargs):
            super(DictAction, self).__init__(option_strings, dest, **kwargs)

        def __call__(self, parser, namespace, value, option_string=None):
            if not re.match(r"\s*\w+\s*=\s*\d+", value):
                raise ArgumentError(self, "should be like nloc=20")
            k, val = value.split("=", 2)
            getattr(namespace, self.dest)[k.strip()] = int(val.strip())

    parser = ArgumentParser(prog=prog)
    parser.add_argument('paths', nargs='*', default=['.'],
                        help='list of the filename/paths.')
    parser.add_argument('--version', action='version', version=version)
    parser.add_argument("-l", "--languages",
                        help='''List the programming languages you want to
                        analyze. if left empty, it'll search for all languages
                        it knows. `lizard -l cpp -l java`searches for C++ and
                        Java code. The available languages are:
    ''' + ', '.join(x.language_names[0] for x in languages()),
                        action="append",
                        dest="languages",
                        default=[])
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
    parser.add_argument("-f", "--input_file",
                        help='''get a list of filenames from the given file
                        ''',
                        type=str,
                        dest="input_file")
    parser.add_argument("-o", "--output_file",
                        help='''Output file. The output format is inferred
                        from the file extension (e.g. .html), unless it is
                        explicitly specified (e.g. using --xml).
                        ''',
                        type=str,
                        dest="output_file")
    parser.add_argument("-L", "--length",
                        help='''Threshold for maximum function length
                        warning. The default value is %d.
                        Functions length bigger than it will generate warning
                        ''' % DEFAULT_MAX_FUNC_LENGTH,
                        type=int,
                        dest="length",
                        default=DEFAULT_MAX_FUNC_LENGTH)
    parser.add_argument("-a", "--arguments",
                        help="Limit for number of parameters",
                        type=int, dest="arguments", default=100)
    parser.add_argument("-w", "--warnings_only",
                        help='''Show warnings only, using clang/gcc's warning
                        format for printing warnings.
                        http://clang.llvm.org/docs/UsersManual.html#cmdoption-fdiagnostics-format
                        ''',
                        action="store_const",
                        const=print_clang_style_warning,
                        dest="printer")
    parser.add_argument("--warning-msvs",
                        help='''Show warnings only, using Visual Studio's
                        warning format for printing warnings.
                        https://msdn.microsoft.com/en-us/library/yxkt8b26.aspx
                        ''',
                        action="store_const",
                        const=print_msvs_style_warning,
                        dest="printer")
    parser.add_argument("-i", "--ignore_warnings",
                        help='''If the number of warnings is equal or less
                        than the number, the tool will exit normally;
                        otherwise, it will generate error.
                        If the number is negative,
                        the tool exits normally
                        regardless of the number of warnings.
                        Useful in makefile for legacy code.''',
                        type=int,
                        dest="number",
                        default=0)
    parser.add_argument("-x", "--exclude",
                        help='''Exclude files that match the pattern. * matches
                        everything,
                        ? matches any single character, "./folder/*" exclude
                        everything in the folder recursively. Multiple patterns
                        can be specified. Don't forget to add "" around the
                        pattern.''',
                        action="append",
                        dest="exclude",
                        default=[])
    parser.add_argument("-t", "--working_threads",
                        help='''number of working threads. The default
                        value is 1. Using a bigger
                        number can fully utilize the CPU and often faster.''',
                        type=int,
                        dest="working_threads",
                        default=1)
    parser.add_argument("-X", "--xml",
                        help='''Generate XML in cppncss style instead of the
                        tabular output. Useful to generate report in Jenkins
                        server''',
                        action="store_const",
                        const=print_xml,
                        dest="printer")
    parser.add_argument("--csv",
                        help='''Generate CSV output as a transform of the
                        default output''',
                        action="store_const",
                        const=print_csv,
                        dest="printer")
    parser.add_argument("-H", "--html",
                        help='''Output HTML report''',
                        action="store_const",
                        const=html_output,
                        dest="printer")
    parser.add_argument("-m", "--modified",
                        help='''Calculate modified cyclomatic complexity number
                        , which count a switch/case with multiple cases as
                        one CCN.''',
                        action="append_const",
                        const="modified",
                        dest="extensions",
                        default=[])
    _extension_arg(parser)
    parser.add_argument("-s", "--sort",
                        help='''Sort the warning with field. The field can be
                        nloc, cyclomatic_complexity, token_count,
                        parameter_count, etc. Or an customized field.''',
                        action="append",
                        dest="sorting",
                        default=[])
    parser.add_argument("-T", "--Threshold",
                        help='''Set the limit for a field. The field can be
                        nloc, cyclomatic_complexity, token_count,
                        parameter_count, etc. Or an customized file. Lizard
                        will report warning if a function exceed the limit''',
                        action=DictAction,
                        dest="thresholds",
                        default={})
    parser.add_argument("-W", "--whitelist",
                        help='''The path and file name to the whitelist file.
                        It's './whitelizard.txt' by default.
                        Find more information in README.''',
                        type=str,
                        dest="whitelist",
                        default=DEFAULT_WHITELIST)

    parser.usage = '''lizard [options] [PATH or FILE] [PATH] ...'''
    parser.description = __doc__
    return parser


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
                        " %(name)s@%(start_line)s-%(end_line)s@%(filename)s"
                        % self.__dict__)

    parameter_count = property(lambda self: len(self.full_parameters))

    @property
    def parameters(self):
        matches = [re.search(r'(\w+)(\s=.*)?$', f)
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


class NestingStack(object):

    def __init__(self):
        self.nesting_stack = []
        self.pending_function = None
        self.function_stack = []

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
            self.fileinfo.function_list.append(self.current_function)
        self.forgive = False
        if self.stacked_functions:
            self.current_function = self.stacked_functions.pop()
        else:
            self.current_function = self.global_pseudo_function


def preprocessing(tokens, reader):
    if hasattr(reader, "preprocess"):
        return reader.preprocess(tokens)
    return (t for t in tokens if not t.isspace() or t == '\n')


def comment_counter(tokens, reader):
    for token in tokens:
        comment = reader.get_comment_from_token(token)
        if comment is not None:
            for _ in comment.splitlines()[1:]:
                yield '\n'
            if comment.strip().startswith("#lizard forgive"):
                reader.context.forgive = True
            if "GENERATED CODE" in comment:
                return
        else:
            yield token


def line_counter(tokens, reader):
    context = reader.context
    context.current_line = 1
    newline = 1
    for token in tokens:
        if token != "\n":
            count = token.count('\n')
            context.current_line += count
            context.add_nloc(count + newline)
            newline = 0
            yield token
        else:
            context.current_line += 1
            newline = 1


def token_counter(tokens, reader):
    context = reader.context
    for token in tokens:
        context.fileinfo.token_count += 1
        context.current_function.token_count += 1
        yield token


def condition_counter(tokens, reader):
    conditions = reader.conditions
    for token in tokens:
        if token in conditions:
            reader.context.add_condition()
        yield token


class FileAnalyzer(object):  # pylint: disable=R0903

    def __init__(self, extensions):
        self.processors = extensions

    def __call__(self, filename):
        try:
            return self.analyze_source_code(
                filename, auto_read(filename))
        except UnicodeDecodeError:
            sys.stderr.write("Error: doesn't support none utf encoding '%s'\n"
                             % filename)
        except IOError:
            sys.stderr.write("Error: Fail to read source file '%s'\n"
                             % filename)
        except IndexError:
            sys.stderr.write("Error: Fail to parse file '%s'\n"
                             % filename)
            raise
        return FileInformation(filename, 0, [])

    def analyze_source_code(self, filename, code):
        context = FileInfoBuilder(filename)
        reader = (get_reader_for(filename) or CLikeReader)(context)
        tokens = reader.generate_tokens(code)
        try:
            for processor in self.processors:
                tokens = processor(tokens, reader)
            for _ in reader(tokens, reader):
                pass
        except RecursionError as e:
            sys.stderr.write("[skip] fail to process '%s' with RecursionError - %s\n" % (filename, e))
        return context.fileinfo


def map_files_to_analyzer(files, analyzer, working_threads):
    mapmethod = get_map_method(working_threads)
    return mapmethod(analyzer, files)


def warning_filter(option, module_infos):
    for file_info in module_infos:
        if file_info:
            for fun in file_info.function_list:
                if any(getattr(fun, attr) > limit for attr, limit in
                       option.thresholds.items()):
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
        if whitelist != DEFAULT_WHITELIST:
            print("WARNING: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            print("WARNING: the whitelist \""+whitelist+"\" doesn't exist.")
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
    '''
    Collect the schema of the data columns.
    Each extension can define some additional data columns to
    the FunctionInfo structure, or even add properties to
    the FileInformation structure.

    In any extension class, define a class level variable:

        FUNCTION_INFO = {
            'column_name' : {
                'caption': 'if defined, will show the column in result',
                'average_caption': 'if defined, will add average function
                                    to FileInformation and show in the
                                    end result.
            }
        }
    '''

    def __init__(self, ext):
        self.extensions = ext
        self.items = [
            {
                'caption': "  NLOC  ", 'value': "nloc",
                'avg_caption': ' Avg.NLOC '},
            {
                'caption': "  CCN  ", 'value': "cyclomatic_complexity",
                'avg_caption': ' AvgCCN '},
            {
                'caption': " token ", 'value': "token_count",
                'avg_caption': ' Avg.token '},
            {'caption': " PARAM ", 'value': "parameter_count"},
            {'caption': " length ", 'value': "length"},
            ] + [
            {
                'caption': caption,
                'value': part,
                'avg_caption': average
            }
            for caption, part, average in self._ext_member_info()]
        self.items.append({'caption': " location  ", 'value': 'location'})

    def patch_for_extensions(self):
        def _patch(name):
            setattr(FileInformation, "average_" + name,
                    property(lambda self: self.functions_average(name)))
        for item in self.items:
            if 'avg_caption' in item:
                _patch(item["value"])

    def any_silent(self):
        return any(hasattr(ex, 'silent_all_others') for ex in self.extensions)

    def value_columns(self):
        return [item['value'] for item in self.items]

    def _ext_member_info(self):
        for ext in self.extensions:
            if hasattr(ext, "FUNCTION_INFO"):
                for key in ext.FUNCTION_INFO:
                    yield (
                        ext.FUNCTION_INFO[key].get("caption", None),
                        key,
                        ext.FUNCTION_INFO[key].get("average_caption", None))

    def captions(self):
        caps = [item.get('caption') for item in self.items]
        return "".join(caption for caption in caps if caption)

    @staticmethod
    def _head(captions):
        return "\n".join(("=" * len(captions), captions, "-" * len(captions)))

    def function_info_head(self):
        return self._head(self.captions())

    def function_info(self, fun):
        return ''.join(
            str(getattr(fun, item['value'])).rjust(len(item['caption']))
            for item in self.items if item['caption'])

    def average_captions(self):
        return "".join([
            e['avg_caption'] for e in self.items
            if e.get("avg_caption", None)])

    def average_formatter(self):
        return "".join([
            "{{module.average_{ext[value]}:{size}.1f}}"
            .format(ext=e, size=len(e['avg_caption']))
            for e in self.items
            if e.get("avg_caption", None)])

    def clang_warning_format(self):
        return (
            "{f.filename}:{f.start_line}: warning: {f.name} has " +
            ", ".join([
                "{{f.{ext[value]}}} {caption}"
                .format(ext=e, caption=e['caption'].strip())
                for e in self.items[:-1]
                ]))

    def msvs_warning_format(self):
        return (
            "{f.filename}({f.start_line}): warning: {f.name} has " +
            ", ".join([
                "{{f.{ext[value]}}} {caption}"
                .format(ext=e, caption=e['caption'].strip())
                for e in self.items[:-1]
                ]))


def print_warnings(option, scheme, warnings):
    warning_count = 0
    warning_nloc = 0
    warn_str = "!!!! Warnings ({0}) !!!!".format(
        ' or '.join("{0} > {1}".format(
            k, val) for k, val in option.thresholds.items()))
    for warning in warnings:
        if warning_count == 0:
            print("\n" + "=" * len(warn_str) + "\n" + warn_str)
            print(scheme.function_info_head())
        warning_count += 1
        warning_nloc += warning.nloc
        print(scheme.function_info(warning))
    if warning_count == 0:
        print_no_warnings(option)
    return warning_count, warning_nloc


def print_no_warnings(option):
    warn_str = "No thresholds exceeded ({0})".format(
        ' or '.join("{0} > {1}".format(
            k, val) for k, val in option.thresholds.items()))
    print("\n" + "=" * len(warn_str) + "\n" + warn_str)


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


def print_total(warning_count, warning_nloc, all_result, scheme):
    print("=" * 90)
    print("Total nloc  " + scheme.average_captions() + "  Fun Cnt  Warning"
          " cnt   Fun Rt   nloc Rt")
    print("-" * 90)
    print((
        "{module.nloc:10d}" +
        scheme.average_formatter() +
        "{function_count:9d}{warning_count:13d}" +
        "{function_rate:10.2f}{nloc_rate:8.2f}").format(
                  module=all_result.as_fileinfo(),
                  function_count=all_result.function_count(),
                  warning_count=warning_count,
                  function_rate=(warning_count/all_result.function_count()),
                  nloc_rate=(warning_nloc/all_result.nloc_in_functions())))


def print_and_save_modules(all_fileinfos, scheme):
    saved_fileinfos = []
    print(scheme.function_info_head())
    for module_info in all_fileinfos:
        if module_info:
            saved_fileinfos.append(module_info)
            for fun in module_info.function_list:
                try:
                    print(scheme.function_info(fun))
                except UnicodeEncodeError:
                    print("Found ill-formatted unicode function name.")
    print("%d file analyzed." % (len(saved_fileinfos)))
    print("==============================================================")
    print("NLOC   " + scheme.average_captions() + " function_cnt    file")
    print("--------------------------------------------------------------")
    for module_info in saved_fileinfos:
        print((
            "{module.nloc:7d}" +
            scheme.average_formatter() +
            "{function_count:10d}" +
            "     {module.filename}").format(
            module=module_info,
            function_count=len(module_info.function_list)))
    return saved_fileinfos


def get_warnings(code_infos, option):
    warnings = whitelist_filter(warning_filter(option, code_infos),
                                whitelist=option.whitelist)
    if isinstance(option.sorting, list) and option.sorting:
        warnings = sorted(warnings, reverse=True, key=lambda x: getattr(
            x, option.sorting[0]))
    return warnings


def print_result(result, option, scheme, total_factory):
    result = print_and_save_modules(result, scheme)
    warnings = get_warnings(result, option)
    warning_count, warning_nloc = print_warnings(option, scheme, warnings)
    print_total(warning_count, warning_nloc, total_factory(result), scheme)
    return warning_count


def silent_printer(result, *_):
    '''
    just to exhaust the result, no output.
    '''
    for _ in result:
        pass
    return 0


def print_clang_style_warning(code_infos, option, scheme, _):
    count = 0
    for warning in get_warnings(code_infos, option):
        print(scheme.clang_warning_format().format(f=warning))
        count += 1
    return count


def print_msvs_style_warning(code_infos, option, scheme, _):
    count = 0
    for warning in get_warnings(code_infos, option):
        print(scheme.msvs_warning_format().format(f=warning))
        count += 1
    return count


def get_map_method(working_threads):
    try:
        if working_threads == 1:
            raise ImportError
        import multiprocessing
        pool = multiprocessing.Pool(processes=working_threads)
        return pool.imap_unordered
    except ImportError:
        return map


def md5_hash_file(full_path_name):
    ''' return md5 hash of a file '''
    try:
        with auto_open(full_path_name, mode='r') as source_file:
            if sys.version_info[0] == 3:
                code_md5 = hashlib.md5(source_file.read().encode('utf-8'))
            else:
                code_md5 = hashlib.md5(source_file.read())
        return code_md5.hexdigest()
    except IOError:
        return None
    except UnicodeDecodeError:
        return None
    except UnicodeEncodeError:
        return None


def get_all_source_files(paths, exclude_patterns, lans):
    '''
    Function counts md5 hash for the given file and checks if it isn't a
    duplicate using set of hashes for previous files '''
    hash_set = set()

    def _support(reader):
        return not lans or set(lans).intersection(
            reader.language_names)

    def _validate_file(pathname):
        return (
            pathname in paths or (
                get_reader_for(pathname) and
                _support(get_reader_for(pathname)) and
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
    def extend_parser(parser_to_extend):
        from argparse import ArgumentParser
        parser = ArgumentParser(add_help=False)
        _extension_arg(parser)
        opt, _ = parser.parse_known_args(args=argv[1:])
        extensions = get_extensions(opt.extensions)
        for ext in extensions:
            if hasattr(ext, "set_args"):
                ext.set_args(parser_to_extend)  # pylint: disable=E1101
        return parser_to_extend
    parser = extend_parser(arg_parser(argv[0]))
    opt = parser.parse_args(args=argv[1:])
    opt.extensions = get_extensions(opt.extensions)
    values = OutputScheme(opt.extensions).value_columns()
    no_fields = (set(opt.sorting) | set(opt.thresholds.keys())) - set(values)
    if no_fields:
        error_message = "Wrong field name '%s'.\n" % ", ".join(no_fields)
        error_message += "Candidates are: " + ', '.join(values) + "\n"
        sys.stderr.write(error_message)
        sys.exit(2)
    if "cyclomatic_complexity" not in opt.thresholds:
        opt.thresholds["cyclomatic_complexity"] = opt.CCN
    if "max_nesting_depth" not in opt.thresholds and hasattr(opt, "ND"):
        opt.thresholds["max_nesting_depth"] = opt.ND
    if "max_nested_structures" not in opt.thresholds and hasattr(opt, "NS"):
        opt.thresholds["max_nested_structures"] = opt.NS
    if "length" not in opt.thresholds:
        opt.thresholds["length"] = opt.length
    if "nloc" not in opt.thresholds:
        opt.thresholds["nloc"] = 1000000
    if "parameter_count" not in opt.thresholds:
        opt.thresholds["parameter_count"] = opt.arguments
    if opt.output_file:
        inferred_printer = infer_printer_from_file_ext(opt.output_file)
        if inferred_printer:
            if not opt.printer:
                opt.printer = inferred_printer
            else:
                msg = "Warning: overriding output file extension.\n"
                sys.stderr.write(msg)
    return opt


def infer_printer_from_file_ext(path):
    mapping = {
        '.csv': print_csv,
        '.htm': html_output,
        '.html': html_output,
        '.xml': print_xml
    }
    _, ext = os.path.splitext(path)
    printer = mapping.get(ext)
    return printer


def open_output_file(path):
    try:
        return codecs.open(path, 'w', encoding='utf8')
    except OSError:
        msg = "Error: failed to open output file '{}'\n.".format(path)
        sys.stderr.write(msg)
        sys.exit(2)


def get_extensions(extension_names):
    from importlib import import_module as im

    def expand_extensions(existing):
        for name in extension_names:
            ext = (
                    im('lizard_ext.lizard' + name.lower())
                    .LizardExtension()
                    if isinstance(name, str) else name)
            existing.insert(
                len(existing) if not hasattr(ext, "ordering_index") else
                ext.ordering_index,
                ext)
        return existing

    return expand_extensions([
            preprocessing,
            comment_counter,
            line_counter,
            token_counter,
            condition_counter,
        ])


analyze_file = FileAnalyzer(get_extensions([]))  # pylint: disable=C0103


def main(argv=None):
    """Command-line entrance to Lizard.

    Args:
        argv: Arguments vector; if None, sys.argv by default.
    """
    options = parse_args(argv or sys.argv)
    printer = options.printer or print_result
    schema = OutputScheme(options.extensions)
    if schema.any_silent():
        printer = silent_printer
    schema.patch_for_extensions()
    if options.input_file:
        options.paths = auto_read(options.input_file).splitlines()
    original_stdout = sys.stdout
    output_file = None
    if options.output_file:
        output_file = open_output_file(options.output_file)
        sys.stdout = output_file
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions,
        options.languages)
    warning_count = printer(result, options, schema, AllResult)
    print_extension_results(options.extensions)
    list(result)
    if output_file:
        sys.stdout = original_stdout
        output_file.close()
    if 0 <= options.number < warning_count:
        sys.exit(1)


def print_extension_results(extensions):
    for extension in extensions:
        if hasattr(extension, 'print_result'):
            extension.print_result()


if __name__ == "__main__":
    main()
