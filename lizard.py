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
import re
import os

# Add script directory to sys.path for Python embeddable package / running from source
# See: https://github.com/terryyin/lizard/issues/460
# Use both __file__ and sys.argv[0]: embeddable Python on Windows may resolve them differently


def _script_dirs():
    dirs = []
    try:
        d = os.path.dirname(os.path.abspath(__file__))
        if d:
            dirs.append(d)
    except NameError:
        pass
    if sys.argv and sys.argv[0]:
        d = os.path.dirname(os.path.abspath(sys.argv[0]))
        if d and d not in dirs:
            dirs.append(d)
    return dirs


for _script_dir in _script_dirs():
    if _script_dir not in sys.path:
        sys.path.insert(0, _script_dir)


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
    from lizard_ext import print_checkstyle
except ImportError:
    sys.stderr.write("Cannot find the lizard_ext modules.")
from lizard_core.file_discovery import (  # noqa: E402
    md5_hash_file, _build_gitignore_spec, get_all_source_files)
from lizard_core.function_info import (  # noqa: E402
    Nesting, BARE_NESTING, Namespace, FunctionInfo, FileInformation,
    AllResult)
from lizard_core.file_info_builder import (  # noqa: E402
    NestingStack, FileInfoBuilder)
from lizard_core.file_analyzer import (  # noqa: E402
    preprocessing, comment_counter, line_counter, token_counter,
    condition_counter, FileAnalyzer)
from lizard_core.extensions import (  # noqa: E402
    get_extensions, print_extension_results)
from lizard_core.analysis import (  # noqa: E402
    analyze, analyze_files, map_files_to_analyzer, _map_in_pool,
    analyze_file)
from lizard_core.warning_filter import (  # noqa: E402
    DEFAULT_WHITELIST, warning_filter, whitelist_filter, get_warnings)

DEFAULT_CCN_THRESHOLD, DEFAULT_MAX_FUNC_LENGTH = 15, 1000


def _extension_arg(parser):
    parser.add_argument("-E", "--extension",
                        help='''User the extensions. The available extensions
                        are: -Ecpre: it will ignore code in the #else branch.
                        -Ewordcount: count word frequencies and generate tag
                        cloud. -Eoutside: include the global code as one
                        function.  -EIgnoreAssert: to ignore all code in
                        assert. -ENS: count nested control structures.
                        -Ehalstead: compute Halstead complexity metrics.
                        -Ecognitive: compute Cognitive Complexity
                        (SonarSource).''',
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
    parser.add_argument("--no-gitignore",
                        help='''Do not use .gitignore files to exclude files.''',
                        action="store_false",
                        dest="use_gitignore",
                        default=True)
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
    parser.add_argument("--checkstyle",
                        help='''Generate Checkstyle XML output for integration with Jenkins and other tools.''',
                        action="store_const",
                        const=print_checkstyle,
                        dest="printer")
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
        return ("{f.filename}:{f.start_line}: warning: {f.name} has {f.nloc} NLOC, "
                "{f.cyclomatic_complexity} CCN, {f.token_count} token, {f.parameter_count} PARAM, "
                "{f.length} length, {f.max_nesting_depth} ND")

    def msvs_warning_format(self):
        return (
            "{f.filename}({f.start_line}): warning: {f.name} ({f.long_name}) has " +
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
    if "cognitive_complexity" not in opt.thresholds and hasattr(opt, "CogC"):
        opt.thresholds["cognitive_complexity"] = opt.CogC
    if "length" not in opt.thresholds:
        opt.thresholds["length"] = opt.length
    if "nloc" not in opt.thresholds:
        opt.thresholds["nloc"] = 1000000
    if "parameter_count" not in opt.thresholds:
        opt.thresholds["parameter_count"] = opt.arguments
    if opt.output_file:
        inferred_printer = infer_printer_from_file_ext(opt.output_file)
        if inferred_printer:
            # Always use print_checkstyle for .checkstyle.xml
            if opt.output_file.lower().endswith('.checkstyle.xml'):
                opt.printer = inferred_printer
            elif not opt.printer:
                opt.printer = inferred_printer
            elif opt.printer != inferred_printer:
                msg = "Warning: overriding output file extension.\n"
                sys.stderr.write(msg)
    return opt


def infer_printer_from_file_ext(path):
    lower_path = path.lower()
    if lower_path.endswith(".checkstyle.xml"):
        return print_checkstyle
    if lower_path.endswith(".html"):
        return html_output
    if lower_path.endswith(".xml"):
        return print_xml
    if lower_path.endswith(".csv"):
        return print_csv
    return None


def open_output_file(path):
    try:
        return codecs.open(path, 'w', encoding='utf8')
    except OSError:
        msg = "Error: failed to open output file '{}'\n.".format(path)
        sys.stderr.write(msg)
        sys.exit(2)


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
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions,
        options.languages,
        options.use_gitignore)
    warning_count = None
    if options.output_file:
        output_file = open_output_file(options.output_file)
        sys.stdout = output_file
        # Special handling for checkstyle output
        if getattr(printer, "__name__", "") == "print_checkstyle":
            warning_count = printer(result, options, schema, AllResult, file=output_file)
        else:
            warning_count = printer(result, options, schema, AllResult)
    else:
        # Special handling for checkstyle output
        if getattr(printer, "__name__", "") == "print_checkstyle":
            warning_count = printer(result, options, schema, AllResult, file=original_stdout)
        else:
            warning_count = printer(result, options, schema, AllResult)
    print_extension_results(options.extensions)
    list(result)
    if output_file:
        sys.stdout = original_stdout
        output_file.close()
    if 0 <= options.number < warning_count:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
