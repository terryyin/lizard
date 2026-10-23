"""
Command-line options: the option table, extension-contributed options, and
resolving the parsed options into thresholds and the output printer.
"""
import re
import sys
from lizard_languages import languages
from lizard_ext import version, print_xml, print_csv, html_output, \
    print_checkstyle
from lizard_core.extensions import get_extensions
from lizard_core.warning_filter import DEFAULT_WHITELIST
from lizard_core.output_scheme import OutputScheme
from lizard_core.text_output import print_clang_style_warning, \
    print_msvs_style_warning

DEFAULT_CCN_THRESHOLD, DEFAULT_MAX_FUNC_LENGTH = 15, 1000
# The --help description: the lizard module's docstring, copied because
# lizard_core never imports lizard; test_options keeps the two equal.
DESCRIPTION = """
lizard is an extensible Cyclomatic Complexity Analyzer for many programming
languages including C/C++ (doesn't require all the header files).
For more information visit http://www.lizard.ws
"""


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
    parser.description = DESCRIPTION
    return parser


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
