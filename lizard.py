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
    from lizard_ext import print_checkstyle
except ImportError:
    sys.stderr.write("Cannot find the lizard_ext modules.")

DEFAULT_CCN_THRESHOLD, DEFAULT_WHITELIST, \
    DEFAULT_MAX_FUNC_LENGTH, DEFAULT_COGC_THRESHOLD = 15, "whitelizard.txt", 1000, 15


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
    parser.add_argument("-G", "--CogC",
                        help='''Threshold for Cognitive Complexity warning.
                        The default value is %d.
                        Functions with CogC bigger than it will generate warning
                        ''' % DEFAULT_COGC_THRESHOLD,
                        type=int,
                        dest="CogC",
                        default=DEFAULT_COGC_THRESHOLD)
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
        self.cognitive_complexity = 0
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
        self.initial_nesting_level = 0  # Track bracket-based nesting level at function start (for CogC)
        self.initial_cogc_nesting_level = 0  # Track control-flow nesting at function start (for preprocessor directives)
        self.max_nesting_depth = 0  # Initialize max_nesting_depth to 0

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

    parameter_count = property(lambda self: len(self.parameters))

    @property
    def parameters(self):
        # Exclude empty tokens as parameters. These can occur in languages
        # allowing a trailing comma on the last parameter in an function
        # argument list.
        # Regex matches the parameter name, then optionally:
        # - a default value given after an '=' sign
        # - a type annotation given after a ':'
        matches = [re.search(r'(\w+)(\s=.*)?(\s:.*)?$', f)
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
    average_cognitive_complexity = property(
        lambda self: self.functions_average("cognitive_complexity"))
    CCN = property(
        lambda self:
        sum(fun.cyclomatic_complexity for fun in self.function_list))
    CogC = property(
        lambda self:
        sum(fun.cognitive_complexity for fun in self.function_list))
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
        self.stacked_pending_functions = []  # Save pending_function for nested functions (PLSQL)
        self._nesting_stack = NestingStack()
        # Cognitive complexity tracking
        self.cogc_nesting_level = 0
        self.cogc_last_operator = None  # Track last binary logical operator
        self.cogc_nesting_stack = []  # Track which nesting levels are from structural control flow
        self.cogc_excluded_nesting = 0  # Track nesting levels that don't count (try blocks)
        self.pending_lambda_nesting = False  # Track if next brace should increase nesting for lambda
        self.lambda_depth = 0  # Track how many lambdas deep we are (for JavaScript nested lambdas)
        self.has_top_level_increment = False  # Track if current function has top-level structural increments (JavaScript)
        self.pending_structural_nesting = False  # Track if next bare nesting is for structural keyword (brace-less languages)

    def __getattr__(self, attr):
        # delegating to _nesting_stack
        return getattr(self._nesting_stack, attr)

    def decorate_nesting_stack(self, decorate_class):
        self._nesting_stack = decorate_class(self._nesting_stack)
        return self._nesting_stack

    def add_bare_nesting(self):
        '''Add a bare nesting level (for Python indentation, function bodies, etc.)'''
        self._nesting_stack.add_bare_nesting()
        # Check if this is structural nesting (for brace-less languages like Lua/Ruby/Python)
        # If pending_structural_nesting is True, this block increases CogC nesting
        if self.pending_structural_nesting:
            self.increase_cogc_nesting()
            self.pending_structural_nesting = False
        else:
            # Mark that this nesting level does NOT increase cognitive complexity
            # (Only structural control flow like if/for/while increases CogC nesting)
            self.cogc_nesting_stack.append(False)

    def pop_nesting(self):
        nest = self._nesting_stack.pop_nesting()
        # Check if this nesting level corresponds to cognitive complexity nesting
        if self.cogc_nesting_stack and self.cogc_nesting_stack[-1]:
            self.decrease_cogc_nesting()
        elif self.cogc_nesting_stack:
            self.cogc_nesting_stack.pop()  # Pop the False marker
        if isinstance(nest, FunctionInfo):
            endline = self.current_function.end_line
            # Check if there are stacked functions (nested procedures/functions)
            # If so, end_of_function() will restore the parent from stacked_functions
            has_stacked_parent = bool(self.stacked_functions)
            self.end_of_function()
            # Only restore from nesting_stack if there was no stacked parent
            # (stacked_functions is used by PLSQL nested procedures, nesting_stack by most other languages)
            # Also restore from nesting_stack if stacked parent was *global* (e.g., Python nested functions in classes)
            if not has_stacked_parent or self.current_function.name == '*global*':
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
        # Store the initial nesting level for cognitive complexity calculation
        # This accounts for classes/namespaces that the function is nested in
        self.current_function.initial_nesting_level = self.current_nesting_level
        # Store the initial cogc nesting separately (e.g., from preprocessor directives)
        # This will be added to cogc-based nesting calculations
        self.current_function.initial_cogc_nesting_level = self.cogc_nesting_level

    def confirm_new_function(self):
        self.start_new_function_nesting(self.current_function)
        self.current_function.cyclomatic_complexity = 1
        # If the function is inside a preprocessor block (cogc_nesting_level > 0),
        # inherit the CogC from global preprocessor directives
        if self.current_function.initial_cogc_nesting_level > 0:
            self.current_function.cognitive_complexity = self.global_pseudo_function.cognitive_complexity

    def restart_new_function(self, name):
        self.try_new_function(name)
        self.confirm_new_function()

    def push_new_function(self, name):
        self.stacked_functions.append(self.current_function)
        # Save pending_function for nested functions (used by PLSQL nested procedures)
        self.stacked_pending_functions.append(getattr(self._nesting_stack, 'pending_function', None))
        self.restart_new_function(name)
        # Reset cognitive complexity nesting for the new function
        self.reset_cogc_nesting()

    def add_condition(self, inc=1):
        self.current_function.cyclomatic_complexity += inc

    def add_cognitive_complexity(self, inc=1):
        '''Add to cognitive complexity with current nesting level'''
        # Calculate nesting using both tracking methods:
        # 1. current_nesting_level: Bracket-based nesting from NestingStack (used by C/C++/Java)
        # 2. cogc_nesting_level: Control-flow based nesting (used for all languages, especially JavaScript)
        #
        # For languages with NestingStack (C/C++/Java):
        #   - Use current_nesting_level minus initial offset
        # For languages without NestingStack (JavaScript):
        #   - Use cogc_nesting_level plus any initial cogc nesting (e.g., from preprocessor directives)
        #
        # We use the maximum of both to support both approaches
        nesting_from_stack = max(0, self.current_nesting_level - self.current_function.initial_nesting_level - 1)
        nesting_from_cogc = self.cogc_nesting_level + self.current_function.initial_cogc_nesting_level
        nesting = max(nesting_from_stack, nesting_from_cogc) - self.cogc_excluded_nesting
        nesting = max(0, nesting)
        self.current_function.cognitive_complexity += inc + nesting

    def increase_cogc_nesting(self):
        '''Increase cognitive complexity nesting level for structural control flow'''
        self.cogc_nesting_level += 1
        # Always add a True marker to track this cognitive nesting level
        self.cogc_nesting_stack.append(True)

    def decrease_cogc_nesting(self):
        '''Decrease cognitive complexity nesting level'''
        if self.cogc_nesting_level > 0:
            self.cogc_nesting_level -= 1
        if self.cogc_nesting_stack:
            self.cogc_nesting_stack.pop()

    def reset_cogc_nesting(self):
        '''Reset cognitive complexity nesting level (e.g., at function boundary)'''
        self.cogc_nesting_level = 0
        self.cogc_last_operator = None
        self.cogc_nesting_stack = []

    def enter_lambda(self):
        '''Mark that a lambda/anonymous function is being entered.
        This increases nesting for code inside the lambda, but does NOT create a new FunctionInfo.
        The complexity of the lambda body is added to the current (parent) function.
        '''
        self.pending_lambda_nesting = True
        self.lambda_depth += 1

    def exit_lambda(self):
        '''Mark that a lambda/anonymous function is being exited.'''
        if self.lambda_depth > 0:
            self.lambda_depth -= 1

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
            # Restore pending_function for nested functions (used by PLSQL nested procedures)
            if self.stacked_pending_functions:
                self._nesting_stack.pending_function = self.stacked_pending_functions.pop()
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
            if comment.strip().startswith("#lizard forgive global"):
                reader.context.forgive_global = True
            elif comment.strip().startswith("#lizard forgive"):
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


def cognitive_complexity_counter(tokens, reader):
    '''
    Calculate Cognitive Complexity according to the specification.
    Three basic rules:
    1. Ignore shorthand structures (methods, null-coalescing)
    2. Increment for breaks in linear flow
    3. Increment for nesting
    '''
    context = reader.context
    prev_token = None
    in_switch = False
    switch_nesting = 0
    after_else = False  # Track if we just saw 'else'
    in_do_while = False  # Track if we're in a do-while construct
    after_break_continue = False  # Track if we just saw break/continue
    saw_question_mark = False  # Track if we saw '?' (to check for ?. operator)
    in_exception_block = False  # Track if we're in PL/SQL EXCEPTION block
    after_loop_keyword = False  # Track if we just saw 'for' or 'while' (for Lua/Ruby 'do')

    # Erlang-specific tracking for clause-based constructs
    is_erlang = hasattr(reader, 'language_names') and 'erlang' in reader.language_names
    erlang_if_case_depth = 0  # Track nesting depth of if/case blocks
    after_semicolon_in_if_case = False  # Track if we saw ';' inside if/case
    after_dash_in_erlang = False  # Track if we saw '-' (for '->' arrow detection)

    # Stack to track which braces increase nesting
    # Each entry is (brace_depth, increases_nesting)
    nesting_stack = []
    brace_depth = 0
    pending_nesting_increase = False
    pending_try_block = False  # Track if we just saw 'try'
    try_block_stack = []  # Track which braces are try blocks (excluded from nesting)

    # Structural keywords that increase both complexity and nesting
    # Includes C preprocessor directives like #if, #ifdef (spec line 143)
    # repeat is for Lua's repeat...until loop
    structural_keywords = {'if', 'for', 'while', 'foreach', 'repeat', '#if', '#ifdef', '#elif'}
    # 'case' is special: it's a structural keyword in Erlang and ST, but not in C/C++/Java (where it's a switch label)
    if hasattr(reader, 'language_names') and ('erlang' in reader.language_names or 'st' in reader.language_names):
        structural_keywords.add('case')
    # catch/except and try (except is Python, catch is C++/Java/C#/JavaScript)
    catch_keywords = {'catch', 'except'}
    # Keywords that create blocks but don't increase cognitive nesting (spec line 234, 537)
    try_keywords = {'try', 'synchronized'}
    # Labeled jumps
    jump_keywords = {'goto'}
    # PL/SQL exception block keyword
    exception_keywords = {'exception'}

    for token in tokens:
        # Check if previous token was '?' and current is not '.'
        # If so, the '?' was a ternary operator
        if saw_question_mark:
            if token != '.':
                # The '?' was a ternary operator, count it
                context.add_cognitive_complexity()
            # Reset the flag regardless
            saw_question_mark = False

        # Track opening braces
        if token == '{':
            brace_depth += 1
            # Check if this is a try block (excluded from cognitive nesting)
            if pending_try_block:
                context.cogc_excluded_nesting += 1
                try_block_stack.append(True)
                pending_try_block = False
            else:
                try_block_stack.append(False)

            # Check if this brace should increase nesting
            # Both structural keywords (if, for, etc.) and lambdas increase nesting
            if pending_nesting_increase or context.pending_lambda_nesting:
                context.increase_cogc_nesting()
                nesting_stack.append(True)  # This brace increases nesting
                pending_nesting_increase = False
                context.pending_lambda_nesting = False
            else:
                nesting_stack.append(False)  # This brace doesn't increase nesting
            after_else = False  # Reset after_else when we see a brace
            after_loop_keyword = False  # Reset for/while tracking (C++ uses {, not do)
            context.pending_structural_nesting = False  # Reset to avoid double-nesting in clike.py
            # Reset binary logical operator tracking (new statement/block)
            context.cogc_last_operator = None

        # Track closing braces (including PL/SQL's }nosync for END IF/LOOP/CASE)
        elif token == '}' or token == '}nosync':
            # Check if this closes a try block
            if try_block_stack:
                is_try_block = try_block_stack.pop()
                if is_try_block:
                    context.cogc_excluded_nesting -= 1

            if nesting_stack:
                increased_nesting = nesting_stack.pop()
                if increased_nesting:
                    context.decrease_cogc_nesting()
            brace_depth -= 1
            # Check if we exited a switch
            if in_switch and brace_depth == switch_nesting:
                in_switch = False
            after_else = False  # Reset after_else

        # Track closing 'end' keyword (for Lua/Ruby/Erlang)
        # Note: Fortran also has 'end' tokens, but Fortran calls pop_nesting() which handles
        # CogC stack automatically. We only handle structural nesting (True) here.
        elif token == 'end':
            # Erlang: track exiting if/case block
            if is_erlang and erlang_if_case_depth > 0:
                erlang_if_case_depth -= 1
            # Only pop if we added structural nesting for a control structure
            # Non-structural nesting (False) is handled by language state machine's pop_nesting()
            if context.cogc_nesting_stack and context.cogc_nesting_stack[-1]:
                # This is a structural nesting level (if/while/for/case), pop it
                context.decrease_cogc_nesting()
            after_else = False  # Reset after_else
            # Reset binary operator tracking (new statement/block)
            context.cogc_last_operator = None

        # C/C++ preprocessor directives: #if, #ifdef, #elif (spec line 143)
        # These are counted as structural increments like regular 'if'
        # Unlike regular 'if', preprocessor directives don't have braces, so we increase nesting immediately
        elif token.startswith('#if') or token.startswith('#elif'):
            context.add_cognitive_complexity()
            # Preprocessor directives don't use braces, so increase nesting immediately
            context.increase_cogc_nesting()

        # Structural increments: if, for, while, foreach
        elif token in structural_keywords:
            # Don't count 'if' after 'else' (else-if is one increment)
            if not (token == 'if' and after_else):
                # Don't count 'while' if it's the end of a do-while
                if not (token == 'while' and in_do_while):
                    context.add_cognitive_complexity()
                    pending_nesting_increase = True
                    # For brace-less languages (Python), increase nesting immediately
                    # The next add_bare_nesting() will mark it properly
                    context.pending_structural_nesting = True
            else:
                # This is the 'if' part of 'else if', still needs to set nesting
                pending_nesting_increase = True
                context.pending_structural_nesting = True
            after_else = False
            if token == 'while' and in_do_while:
                in_do_while = False
            # Track if we saw 'for' or 'while' (for Lua/Ruby where 'do' follows)
            if token in ('for', 'while'):
                after_loop_keyword = True
            # Erlang: track if we entered an if or case block
            if is_erlang and token in ('if', 'case'):
                erlang_if_case_depth += 1

        # do-while loop (C/C++/Java) or do keyword after for/while (Lua/Ruby)
        elif token == 'do':
            # In Lua/Ruby, 'do' follows 'for' or 'while' and shouldn't be counted separately
            # In C/C++/Java, 'do' starts a do-while loop and should be counted
            if not after_loop_keyword:
                # C-style do-while
                context.add_cognitive_complexity()
                pending_nesting_increase = True
                in_do_while = True
            else:
                # Lua/Ruby-style: do follows for/while, add nesting for the block
                if context.pending_structural_nesting:
                    context.add_bare_nesting()
            after_loop_keyword = False  # Reset the flag
            # Reset binary operator tracking (new block starting)
            context.cogc_last_operator = None

        # else and else-if
        # These are HYBRID increments (spec line 144-147, Appendix B2/B3):
        # - They ADD to complexity (+1)
        # - They DO increase nesting level for things inside them
        # - But they DON'T get nesting penalty themselves (mental cost already paid)
        # NOTE: 'else' inside a switch/match statement does NOT add complexity
        # (the switch itself already counted as +1)
        elif token == 'else' and not in_switch:
            # Fundamental increment: +1 without nesting multiplier
            context.current_function.cognitive_complexity += 1
            pending_nesting_increase = True
            after_else = True

        # elif/elseif/elsif/else if (some languages use elsif like PL/SQL, Ruby; Fortran uses 'else if')
        elif token in ('elif', 'elseif', 'elsif', 'else if'):
            # Fundamental increment: +1 without nesting multiplier
            context.current_function.cognitive_complexity += 1
            pending_nesting_increase = True

        # catch clause (or PL/SQL EXCEPTION WHEN)
        elif token in catch_keywords or (in_exception_block and token == 'when'):
            context.add_cognitive_complexity()
            pending_nesting_increase = True

        # PL/SQL EXCEPTION keyword - following WHEN clauses act like catch
        elif token in exception_keywords:
            in_exception_block = True

        # try doesn't add complexity, and try blocks don't increase nesting (spec line 234)
        elif token in try_keywords:
            pending_try_block = True

        # Switch/match statement - single increment for entire switch
        # (switch in C/Java/JavaScript, match in Rust/Scala)
        elif token in ('switch', 'match'):
            context.add_cognitive_complexity()
            in_switch = True
            switch_nesting = brace_depth
            pending_nesting_increase = True

        # Ternary operator (? :) - treat like if statement (spec line 143, 470, 481, 490)
        # Note: We only increment on '?', not on ':', to avoid double-counting
        # The '?' increments and increases nesting, the ':' doesn't add anything
        elif token == '?':
            # Check if this is the ternary operator or optional chaining (?.)
            # Optional chaining would have '.' as next token, but we don't have lookahead here
            # For now, assume all '?' are ternary (optional chaining is rare and doesn't affect complexity much)
            context.add_cognitive_complexity()
            pending_nesting_increase = True

        # Binary logical operators: sequences (fundamental increment - no nesting multiplier)
        # Note: 'and'/'or' are Python, '&&'/'||' are C/C++/Java/JavaScript, '.and.'/'.or.' are Fortran
        # 'andalso'/'orelse' are Erlang
        elif token.lower() in ('&&', '||', 'and', 'or', '.and.', '.or.', 'andalso', 'orelse'):
            # Normalize operator for comparison (and/&&/.and./andalso are equivalent, or/||/.or./orelse are equivalent)
            normalized = 'and' if token.lower() in ('&&', 'and', '.and.', 'andalso') else 'or'
            # Only increment on first operator or when switching operator types
            if context.cogc_last_operator != normalized:
                # This is a fundamental increment: +1 without nesting multiplier (spec line 209-216)
                context.current_function.cognitive_complexity += 1
                context.cogc_last_operator = normalized

        # Ternary operator: condition ? true_value : false_value
        # The '?' acts like an 'if' statement (spec line 470, 481, 490)
        # But ignore null-coalescing operators: ?. and ?? (spec line 119-135)
        elif token == '?':
            # Check if this is null-coalescing (?. or ??) or ternary
            # If previous token is also '?', this is ?? (null-coalescing, handled as single token in C#)
            # Note: We need to wait to see if next token is '.' to determine if this is ?.
            # For now, mark that we saw a '?' and check on the next iteration.
            # We use a flag to defer the decision.
            saw_question_mark = True

        # Labeled jumps: goto LABEL, break LABEL, continue LABEL
        # goto always counts, break/continue only count if followed by a label
        elif token in jump_keywords:
            # goto always adds +1 (fundamental increment without nesting multiplier)
            context.current_function.cognitive_complexity += 1
        elif token in ('break', 'continue'):
            # Mark that we saw break/continue, will check next token to see if it's a label
            after_break_continue = True
        # Check if current token is a label after break/continue
        elif after_break_continue:
            # If the token after break/continue is not a semicolon or brace, it's likely a label
            if token not in (';', '}', '\n'):
                # This is a labeled jump: add +1 (fundamental increment)
                context.current_function.cognitive_complexity += 1
            after_break_continue = False

        # Handle 'then' keyword (starts body of if/while in Lua/Ruby)
        # DISABLED: Fortran also handles 'then' and this causes double nesting
        # elif token == 'then':
        #     # If this follows a structural keyword (if/while), add nesting
        #     if context.pending_structural_nesting:
        #         context.add_bare_nesting()
        #     context.cogc_last_operator = None

        # Erlang: handle clause separators in if/case statements
        # Each clause after the first (separated by ';') counts like an elsif
        elif is_erlang and token == ';' and erlang_if_case_depth > 0:
            # Mark that we saw a semicolon inside an if/case block
            after_semicolon_in_if_case = True
            context.cogc_last_operator = None

        # Erlang: track '-' which might be part of '->' arrow
        elif is_erlang and token == '-':
            after_dash_in_erlang = True

        # Erlang: '>' after '-' forms '->' (arrow operator)
        # When this appears after ';' in if/case, it indicates a new clause (like elsif)
        elif is_erlang and token == '>' and after_dash_in_erlang:
            after_dash_in_erlang = False
            if after_semicolon_in_if_case:
                # This is a new clause in an if/case statement
                # NOTE: Due to Erlang parser limitations, this increment may go to the wrong function
                # when ';' appears in if/case blocks (parser treats ';' as function terminator)
                context.add_cognitive_complexity()
                after_semicolon_in_if_case = False
            context.cogc_last_operator = None

        # Reset operator tracking on statement boundaries
        elif token in (';', '\n', '{'):
            context.cogc_last_operator = None

        prev_token = token
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
            sys.stderr.write(
                "[skip] fail to process '%s' with RecursionError - %s\n" %
                (filename, e))
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
            return auto_read(whitelist)
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
                'caption': " CogC  ", 'value': "cognitive_complexity",
                'avg_caption': ' Avg.CogC '},
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
                "{f.cyclomatic_complexity} CCN, {f.cognitive_complexity} CogC, {f.token_count} token, {f.parameter_count} PARAM, "
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
    duplicate using set of hashes for previous files.

    If a .gitignore file is found in any of the given paths, it will be used
    to filter out files that match the gitignore patterns.
    '''
    hash_set = set()
    gitignore_spec = None
    base_path = None

    def _load_gitignore():
        nonlocal gitignore_spec, base_path
        try:
            import pathspec
            for path in paths:
                gitignore_path = os.path.join(path, '.gitignore')
                if os.path.exists(gitignore_path):
                    gitignore_file = auto_read(gitignore_path)
                    # Read lines and strip whitespace and empty lines
                    patterns = [line.strip() for line in gitignore_file.splitlines()]
                    patterns = [p for p in patterns if p and not p.startswith('#')]
                    gitignore_spec = pathspec.PathSpec.from_lines('gitwildmatch', patterns)
                    base_path = path
                    break
        except ImportError:
            pass

    def _support(reader):
        return not lans or set(lans).intersection(
            reader.language_names)

    def _validate_file(pathname):
        if gitignore_spec is not None and base_path is not None:
            rel_path = os.path.relpath(pathname, base_path)
            # Normalize path separators for consistent matching
            rel_path = rel_path.replace(os.sep, '/')
            if gitignore_spec.match_file(rel_path):
                return False
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

    _load_gitignore()
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
    if "cognitive_complexity" not in opt.thresholds:
        opt.thresholds["cognitive_complexity"] = opt.CogC
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
            cognitive_complexity_counter,
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
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions,
        options.languages)
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
        sys.exit(1)


def print_extension_results(extensions):
    for extension in extensions:
        if hasattr(extension, 'print_result'):
            extension.print_result()


if __name__ == "__main__":
    main()
