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
from __future__ import print_function
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
    from lizard_ext import print_xml
    from lizard_ext import html_output
except ImportError:
    pass


VERSION = "1.10.2"

DEFAULT_CCN_THRESHOLD, DEFAULT_WHITELIST, \
    DEFAULT_MAX_FUNC_LENGTH = 15, "whitelizard.txt", 1000


def analyze(paths, exclude_pattern=None, threads=1, exts=None, lans=None):
    '''
    returns an iterator of file information that contains function
    statistics.
    '''
    exclude_pattern = exclude_pattern or []
    extensions = exts or []
    files = get_all_source_files(paths, exclude_pattern, lans)
    file_analyzer = FileAnalyzer(extensions)
    return map_files_to_analyzer(files, file_analyzer, threads)


def create_command_line_parser(prog=None):
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
    parser.add_argument('--version', action='version', version=VERSION)
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
    parser.add_argument("-i", "--ignore_warnings",
                        help='''If the number of warnings is equal or less
                        than the number,
                        the tool will exit normally, otherwise it will generate
                        error. Useful in makefile for legacy code.''',
                        type=int,
                        dest="number",
                        default=0)
    parser.add_argument("-x", "--exclude",
                        help='''Exclude files that match this pattern. * matches
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
    parser.add_argument("-H", "--html",
                        help='''Output HTML report''',
                        action="store_const",
                        const=html_output,
                        dest="printer")
    parser.add_argument("-m", "--modified",
                        help="Calculate modified cyclomatic complexity number",
                        action="store_true",
                        dest="switchCasesAsOneCondition",
                        default=False)
    parser.add_argument("-E", "--extension",
                        help='''User the extensions. The available extensions
                        are: -Ecpre: it will ignore code in the #else branch.
                        -Ewordcount: count word frequencies and generate tag
                        cloud. -Eoutside: include the global code as one
                        function.  -EIgnoreAssert: to ignore all code in
                        assert''',
                        action="append",
                        dest="extensions",
                        default=[])
    parser.add_argument("-s", "--sort",
                        help='''Sort the warning with field. The field can be
                        nloc, cyclomatic_complexity, token_count,
                        p#arameter_count, etc. Or an customized field.''',
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


class FunctionInfo(object):  # pylint: disable=R0902

    def __init__(self, name, filename, start_line=0, ccn=1):
        self.cyclomatic_complexity = ccn
        self.nloc = 1
        self.token_count = 1  # the first token
        self.name = name
        self.long_name = name
        self.start_line = start_line
        self.end_line = start_line
        self.parameters = []
        self.filename = filename
        self.indent = -1
        self.length = 0

    location = property(lambda self:
                        " %(name)s@%(start_line)s-%(end_line)s@%(filename)s"
                        % self.__dict__)

    parameter_count = property(lambda self: len(self.parameters))

    def add_to_function_name(self, app):
        self.name += app
        self.long_name += app

    def add_to_long_name(self, app):
        self.long_name += app

    def add_parameter(self, token):
        self.add_to_long_name(" " + token)

        if not self.parameters or token == ",":
            self.parameters.append(token)
        else:
            self.parameters[-1] = token

    def clang_format_warning(self):
        return (
            "{f.filename}:{f.start_line}: warning: {f.name} has" +
            " {f.cyclomatic_complexity} CCN and {f.parameter_count}" +
            " params ({f.nloc} NLOC, {f.token_count} tokens)").format(f=self)


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


class FileInfoBuilder(object):

    def __init__(self, filename):
        self.fileinfo = FileInformation(filename, 0)
        self.current_line = 0
        self.forgive = False
        self.newline = True
        self.global_pseudo_function = FunctionInfo('*global*', filename, 0)
        self.current_function = self.global_pseudo_function

    def add_nloc(self, count):
        self.fileinfo.nloc += count
        self.current_function.nloc += count
        self.current_function.end_line = self.current_line
        self.current_function.length =\
            self.current_line - self.current_function.start_line
        self.newline = count > 0

    def start_new_function(self, name):
        self.current_function = FunctionInfo(
            name,
            self.fileinfo.filename,
            self.current_line)

    def add_condition(self, inc=1):
        self.current_function.cyclomatic_complexity += inc

    def reset_complexity(self):
        self.current_function.cyclomatic_complexity = 1

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
    for token in tokens:
        comment = reader.get_comment_from_token(token)
        if comment is not None:
            for _ in comment.splitlines()[1:]:
                yield '\n'
            if comment.strip().startswith("#lizard forgive"):
                reader.context.forgive = True
        else:
            yield token


def line_counter(tokens, reader):
    reader.context.current_line = 1
    newline = 1
    for token in tokens:
        if token != "\n":
            count = token.count('\n')
            reader.context.current_line += count
            reader.context.add_nloc(count + newline)
            newline = 0
            yield token
        else:
            reader.context.current_line += 1
            newline = 1


def token_counter(tokens, reader):
    for token in tokens:
        reader.context.fileinfo.token_count += 1
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
        except IndexError:
            sys.stderr.write("Error: Fail to parse file '%s'\n"
                             % filename)
            raise

    def analyze_source_code(self, filename, code):
        context = FileInfoBuilder(filename)
        reader = (get_reader_for(filename) or CLikeReader)(context)
        tokens = reader.generate_tokens(code)
        for processor in self.processors:
            tokens = processor(tokens, reader)
        for _ in reader(tokens, reader):
            pass
        return context.fileinfo


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
        elif whitelist != DEFAULT_WHITELIST:
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

    def __init__(self, ext):
        self.extensions = ext
        self.items = [
            {'caption': "  NLOC  ", 'value': "nloc"},
            {'caption': "  CCN  ", 'value': "cyclomatic_complexity"},
            {'caption': " token ", 'value': "token_count"},
            {'caption': " PARAM ", 'value': "parameter_count"},
            {'caption': " length ", 'value': "length"},
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

    warn_str = "!!!! Warnings ({0}) !!!!".format(
            ' or '.join("{0} > {1}".format(
                k, val) for k, val in option.thresholds.items()))
    print("\n" + "=" * len(warn_str) + "\n" + warn_str)
    print(scheme.function_info_head())
    for warning in warnings:
        warning_count += 1
        print(scheme.function_info(warning))
    return warning_count


def print_total(warning_count, saved_result, op):
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
            if f.cyclomatic_complexity > op.thresholds['cyclomatic_complexity']
            ])) / nloc_in_functions
    )

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


def get_warnings(code_infos, option):
    warnings = whitelist_filter(warning_filter(option, code_infos),
                                whitelist=option.whitelist)
    if isinstance(option.sorting, list) and len(option.sorting) > 0:
        warnings = sorted(warnings, reverse=True, key=lambda x: getattr(
            x, option.sorting[0]))
    return warnings


def print_result(result, option, scheme):
    result = print_and_save_modules(result, option.extensions, scheme)
    warnings = get_warnings(result, option)
    warning_count = print_warnings(option, scheme, warnings)
    print_total(warning_count, result, option)
    for extension in option.extensions:
        if hasattr(extension, 'print_result'):
            extension.print_result()
    return warning_count


def print_clang_style_warning(code_infos, option, _):
    count = 0
    for warning in get_warnings(code_infos, option):
        print(warning.clang_format_warning())
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
    opt = create_command_line_parser(argv[0]).parse_args(args=argv[1:])
    values = [item['value'] for item in OutputScheme([]).items]
    no_fields = (set(opt.sorting) | set(opt.thresholds.keys())) - set(values)
    if no_fields:
        error_message = "Wrong field name '%s'.\n" % ", ".join(no_fields)
        error_message += "Candidates are: " + ', '.join(values) + "\n"
        sys.stderr.write(error_message)
        sys.exit(2)
    if "cyclomatic_complexity" not in opt.thresholds:
        opt.thresholds["cyclomatic_complexity"] = opt.CCN
    if "length" not in opt.thresholds:
        opt.thresholds["length"] = opt.length
    if "parameter_count" not in opt.thresholds:
        opt.thresholds["parameter_count"] = opt.arguments
    return opt


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
        token_counter,
        condition_counter,
    ]
    if switch_case_as_one_condition:
        extensions.append(recount_switch_case)
    return expand_extensions(extensions)

analyze_file = FileAnalyzer(get_extensions([]))  # pylint: disable=C0103


def lizard_main(argv):
    options = parse_args(argv)
    options.extensions = get_extensions(options.extensions,
                                        options.switchCasesAsOneCondition)
    printer = options.printer or print_result
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions,
        options.languages)
    warning_count = printer(result, options, OutputScheme(options.extensions))
    if options.number < warning_count:
        sys.exit(1)

if __name__ == "__main__":
    lizard_main(sys.argv)
