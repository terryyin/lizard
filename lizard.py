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
#  author: terry.yinzhe@gmail.com
#
"""
lizard is a simple code complexity analyzer without caring about the C/C++
header files or Java imports. It can deal with

* Java
* C/C++
* Objective C.

It counts

* the nloc (net lines of code, excluding comments and blanks),
* CCN (cyclomatic complexity number) or Modified CCN,
* token count of functions.
* parameter count of functions.

You can set limitation for CCN (-C), the number of parameters (-a). Functions
that exceed these limitations will generate warnings. The exit code of lizard
will be none-Zero if there are warnings.

This tool actually calculates how complex the code 'looks' rather than how
complex the code real 'is'. People will need this tool because it's often very
hard to get all the included folders and files right when they are complicated.
But we don't really need that kind of accuracy when come to cyclomatic
complexity.

It requires python2.6 or above (early versions are not verified).
"""

VERSION = "1.7.8"


import itertools, traceback
import re


DEFAULT_CCN_THRESHOLD = 15


def analyze(paths, options):
    ''' This is the most important function of lizard.
        It analyzes the given paths with the options.
        Can be used directly by other Python application.
    '''

    languages = LanguageChooser(
            includeHashIfConditions = not options.no_preprocessor_count,
            switchCasesAsOneCondition = options.switchCasesAsOneCondition,)

    files = FilesFilter(options.exclude, options.duplicates).getFileNames(paths)
    fileAnalyzer = FileAnalyzer(languages, options.extensions)
    return mapFilesToAnalyzer(files, fileAnalyzer, options.working_threads)

def createCommandLineParser():
    from optparse import OptionParser
    parser = OptionParser(version=VERSION)
    parser.add_option("-v", "--verbose",
            help="Output in verbose mode (long function name)",
            action="store_true",
            dest="verbose",
            default=False)
    parser.add_option("-C", "--CCN",
            help =  "Threshold for cyclomatic complexity number warning. "+
                    "The default value is %d. Functions with CCN bigger than this number will generate warning" % DEFAULT_CCN_THRESHOLD,
            action="store",
            type="int",
            dest="CCN",
            default=DEFAULT_CCN_THRESHOLD)
    parser.add_option("-a", "--arguments",
            help="Limit for number of parameters",
            action="store",
            type="int",
            dest="arguments",
            default=100)
    parser.add_option("-w", "--warnings_only",
            help="Show warnings only, using clang/gcc's warning format for printing warnings. http://clang.llvm.org/docs/UsersManual.html#cmdoption-fdiagnostics-format",
            action="store_true",
            dest="warnings_only",
            default=False)
    parser.add_option("-i", "--ignore_warnings",
            help="If the number of warnings is equal or less than the number, the tool will exit normally, otherwize it will generate error. Useful in makefile when improving legacy code.",
            action="store",
            type="int",
            dest="number",
            default=0)
    parser.add_option("-x", "--exclude",
            help="Exclude files that match this pattern. * matches everything, ? matches any single characoter, \"./folder/*\" exclude everything in the folder, recursively. Multiple patterns can be specified. Don't forget to add \"\" around the pattern.",
            action="append",
            dest="exclude",
            default=[])
    parser.add_option("-X", "--xml",
            help="Generate XML in cppncss style instead of the normal tabular output. Useful to generate report in Jenkins server",
            action="store_true",
            dest="xml",
            default=None)
    parser.add_option("-P", "--no_preprocessor_count",
            help="By default, a #if will also increase the complexity. Adding this option to ignore them",
            action="store_true",
            dest="no_preprocessor_count",
            default=False)
    parser.add_option("-t", "--working_threads",
            help="number of working threads. The default value is 1.",
            action="store",
            type="int",
            dest="working_threads",
            default=1)
    parser.add_option("-d", "--find_duplicates",
            help="find and skip analysis for identical files. Will be made default in the next release",
            action="store_true",
            dest="duplicates",
            default=False)
    parser.add_option("-e", "--display_fn_end_line",
            help="display function end line number in addition to start line number. Will be made default in the next release",
            action="store_true",
            dest="display_fn_end_line",
            default=False)
    parser.add_option("-m", "--modified",
            help="Calculate modified cyclomatic complexity number",
            action="store_true",
            dest="switchCasesAsOneCondition",
            default=False)
    parser.add_option("-E", "--extension",
            help="under construction...", #"Use extension. Can be WordCount.",
            action="append",
            dest="extensions",
            default=[])

    parser.usage = "lizard [options] [PATH or FILE] [PATH] ... "
    parser.description = __doc__
    return parser


class FunctionInfo(object):
    '''
    Statistic information of a function.
    '''

    def __init__(self, name, start_line = 0, ccn = 1):
        self.cyclomatic_complexity = ccn
        self.nloc = 0
        self.token_count = 1  # the first token
        self.name = name
        self.long_name = name
        self.start_line = start_line
        self.end_line = None
        self.parameter_count = 0

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


class FileInformation(object):
    ''' 
    Statistic information of a source file.
    Including all the functions and the file summary.
    '''

    def __init__(self, filename, nloc, function_list):
        self.filename = filename
        self.nloc = nloc
        self.function_list = function_list
        self.token_count = 0

    average_NLOC = property(lambda self:self._functions_average("nloc"))
    average_token = property(lambda self:self._functions_average("token_count"))
    average_CCN = property(lambda self:self._functions_average("cyclomatic_complexity"))
    CCN = property(lambda self:sum(fun.cyclomatic_complexity for fun in self.function_list))

    def _functions_average(self, att):
        return sum(getattr(fun, att) for fun in self.function_list) \
                / len(self.function_list) if self.function_list else 0


class CodeInfoContext(object):
    """
        provides the builder to build FileInformation
        The building blocks are:
        START_NEW_FUNCTION
            ADD_TO_FUNCTION_NAME
            ADD_TO_LONG_FUNCTION_NAME
                PARAMETER
                    CONDITION
                    TOKEN
        END_OF_FUNCTION
    """

    def __init__(self, filename):
        self.fileinfo = FileInformation(filename, 0, [])
        self.current_line = 0
        self.START_NEW_FUNCTION('')
        self.forgive = False

    def _new_line(self):
        self.fileinfo.nloc += 1
        self.newline = True

    def _count_token(self):
        self.fileinfo.token_count+=1
        if self.newline:
            self.current_function.nloc += 1
            self.newline = False
        self.current_function.token_count += 1

    def START_NEW_FUNCTION(self, name):
        self.newline = True
        self.current_function = FunctionInfo(name, self.current_line)

    def CONDITION(self):
        self.current_function.cyclomatic_complexity += 1

    def ADD_TO_LONG_FUNCTION_NAME(self, app):
        self.current_function.add_to_long_name(app)

    def ADD_TO_FUNCTION_NAME(self, app):
        self.current_function.add_to_function_name(app)

    def PARAMETER(self, token):
        self.current_function.add_parameter(token)

    def END_OF_FUNCTION(self):
        self.current_function.end_line = self.current_line
        if not self.forgive:
            self.fileinfo.function_list.append(self.current_function)
        self.forgive = False
        self.START_NEW_FUNCTION('')


class LineCounter():

    def extend_tokens(self, tokens, context):
        self.context = context
        self.context.current_line = 1 
        for token in tokens:
            self.context.current_line += 1 if token == '\n' else (len(token.splitlines()) - 1)
            if token == "\n":
                self.context._new_line()
            elif token.startswith("/*") or token.startswith("//"):
                if len(token.splitlines()) > 1:
                    self.context._new_line()
                if token[2:].strip().startswith("#lizard forgive"):
                    self.context.forgive = True
            else:
                self.context._count_token()
                yield token


class LanguageReaderBase(object):

    def process_tokens(self, tokens, context):
        self._state = self._GLOBAL
        self.context = context
        for token in tokens:
            self._state(token)

    def extend_tokens(self, tokens, context):
        return tokens


class CLikeReader(LanguageReaderBase):
    '''
    This is the reader for C, C++ and Java.
    '''

    def __init__(self, includeHashIfConditions = True, switchCasesAsOneCondition = False, **kwargs):
        '''includeHashIfConditions: Should #if and #elseif have effect on the CCN
           switchCasesAsOneCondition: Use modified CCN (i.e. for one switch case increases CCN by 1 instead of
                        by the amount of case blocks)
        '''
        super(CLikeReader, self).__init__(**kwargs)
        self.conditions = set(
            ['if', 'for', 'while', '&&', '||', '?', 'catch'])

        if includeHashIfConditions:
            self.conditions.add('#if')
            self.conditions.add('#ifdef')
            self.conditions.add('#elif')

        if switchCasesAsOneCondition:
            self.conditions.add('switch')
        else:
            self.conditions.add('case')

        self.bracket_level = 0
        self.br_count = 0
        self.last_preprocessor = None

    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def extend_tokens(self, tokens, context):
        # handle c preprocessors
        for token in tokens:
            m = self.macro_pattern.match(token)
            if m:
                yield "#" + m.group(1)
                param = m.group(2).strip()
                if param:
                    yield param
            else:
                yield token

    def _is_condition(self, token):
        return token in self.conditions

    def _GLOBAL(self, token):
        if token == '(':
            self.bracket_level += 1
            self._state = self._DEC
            self.context.ADD_TO_LONG_FUNCTION_NAME(token)
        elif token == '::':
            self._state = self._NAMESPACE
        else:
            self.context.START_NEW_FUNCTION(token)
            if token == 'operator':
                self._state = self._OPERATOR

    def _OPERATOR(self, token):
        if token != '(':
            self._state = self._GLOBAL
        self.context.ADD_TO_FUNCTION_NAME(' ' + token)

    def _NAMESPACE(self, token):
        self._state = self._OPERATOR if token == 'operator'  else self._GLOBAL
        self.context.ADD_TO_FUNCTION_NAME("::" + token)

    def _DEC(self, token):
        if token in ('(', "<"):
            self.bracket_level += 1
        elif token in (')', ">"):
            self.bracket_level -= 1
            if (self.bracket_level == 0):
                self._state = self._DEC_TO_IMP
        elif self.bracket_level == 1:
            self.context.PARAMETER(token)
            return
        self.context.ADD_TO_LONG_FUNCTION_NAME(" " + token)

    def _DEC_TO_IMP(self, token):
        if token == 'const':
            self.context.ADD_TO_LONG_FUNCTION_NAME(" " + token)
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        elif token == ":":
            self._state = self._CONSTRUCTOR_INITIALIZATION_LIST
        else:
            self._state = self._GLOBAL

    def _CONSTRUCTOR_INITIALIZATION_LIST(self, token):
        if token == '{':
            self.br_count += 1
            self._state = self._IMP

    def _IMP(self, token):
        if token in ("#else", "#endif"):
            self.last_preprocessor = token
        # will ignore the braces in a #else branch            
        if self.last_preprocessor != '#else':
            if token == '{':
                self.br_count += 1
            elif token == '}':
                self.br_count -= 1
                if self.br_count == 0:
                    self._state = self._GLOBAL
                    self.context.END_OF_FUNCTION()
                    return
        if self._is_condition(token):
            self.context.CONDITION()


class ObjCReader(CLikeReader):
    def __init__(self, **kwargs):
        super(ObjCReader, self).__init__(**kwargs)

    def _DEC_TO_IMP(self, token):
        if token in ("+", "-"):
            self._state = self._GLOBAL
        else:
            super(ObjCReader, self)._DEC_TO_IMP(token)
            if self._state == self._GLOBAL:
                self._state = self._OBJC_DEC_BEGIN
                self.context.START_NEW_FUNCTION(token)

    def _OBJC_DEC_BEGIN(self, token):
        if token == ':':
            self._state = self._OBJC_DEC
            self.context.ADD_TO_FUNCTION_NAME(token)
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._GLOBAL

    def _OBJC_DEC(self, token):
        if token == '(':
            self._state = self._OBJC_PARAM_TYPE
            self.context.ADD_TO_LONG_FUNCTION_NAME(token)
        elif token == ',':
            pass
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._OBJC_DEC_BEGIN
            self.context.ADD_TO_FUNCTION_NAME(" " + token)

    def _OBJC_PARAM_TYPE(self, token):
        if token == ')':
            self._state = self._OBJC_PARAM
        self.context.ADD_TO_LONG_FUNCTION_NAME(" " + token)

    def _OBJC_PARAM(self, token):
        self._state = self._OBJC_DEC


def compile_file_extension_re(*exts):
    return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.IGNORECASE)

class LanguageChooser(object):

    def __init__(self, **readerArgs):
        object.__init__(self)
        self.readerArgs = readerArgs

    lizard_language_infos = {
                     'c/c++': {
                          'name_pattern': compile_file_extension_re("c", "cpp", "cc", "mm", "cxx", "h", "hpp"),
                          'reader':CLikeReader},
                     'Java': {
                          'name_pattern': compile_file_extension_re("java"),
                          'reader':CLikeReader},
                      'objC' : {
                          'name_pattern': compile_file_extension_re("m"),
                          'reader':ObjCReader}
                    }

    def get_language_by_filename(self, filename):
        for lan in self.lizard_language_infos:
            info = self.lizard_language_infos[lan]
            if info['name_pattern'].match(filename):
                return lan

    def get_reader_by_file_name_otherwise_default(self, filename):
        lan = self.get_language_by_filename(filename)
        return self.lizard_language_infos[lan or "c/c++"]['reader'](**self.readerArgs)

default_language_chooser = LanguageChooser(
    includeHashIfConditions = True,
    switchCasesAsOneCondition = False,
)

class FileAnalyzer:

    def __init__(self, languageChooser = default_language_chooser, extensions = []):
        self.extensions = extensions
        self.languageChooser = languageChooser

    def __call__(self, filename):
        with open(filename) as f:
            return self.analyze_source_code(filename, f.read())

    def analyze_source_code(self, filename, code):
        reader = self.languageChooser.get_reader_by_file_name_otherwise_default(filename)
        return self.analyze_source_code_with_parser(filename, code, reader)

    def analyze_source_code_with_parser(self, filename, code, parser):
        tokens = generate_tokens(code)
        context = CodeInfoContext(filename)
        for extension in [parser, LineCounter()] + self.extensions:
            tokens = extension.extend_tokens(tokens, context)
        parser.process_tokens(tokens, context)
        return context.fileinfo

analyze_file = FileAnalyzer()


def warning_filter(option, fileStatistics):
    for file_info in fileStatistics:
        if file_info:
            for fun in file_info.function_list:
                if fun.cyclomatic_complexity > option.CCN or \
                        fun.parameter_count > option.arguments:
                    yield fun, file_info.filename


class Whitelist(object):

    def __init__(self, text):
        self.whitelist = [
            self._get_whitelist_item(line.split('#')[0])
            for line in text.splitlines()]

    def filter(self, warnings):
        for warning in warnings:
            if not self._in_list(warning):
                yield warning

    def _get_whitelist_item(self, text):
        white = {}
        pieces = text.replace('::', '##').split(':')
        if len(pieces) > 1:
            white['file_name'] = pieces[0]
            text = pieces[1]
        white['function_names'] = [x.strip().replace('##','::') for x in text.split(',')]
        return white

    def _in_list(self, warning):
        return any(self._match_whitelist_item(white, warning) for white in self.whitelist)

    def _match_whitelist_item(self, white, warning):
        if warning[0].name in white['function_names']:
            if 'file_name' in white:
                return warning[1] == white['file_name']
            return True
        return False

class FreeFormattingTokenizer(object):
    '''
    Use this tokenizer to tokenize C/C++, Java, ObjC code, which the
    format is not part of the syntax. So indentation & new lines
    doesn't matter.
    '''

    # DONOT put any sub groups in the regex. Good for performance
    _until_end = r"(?:\\\n|[^\n])*"
    token_pattern = re.compile(r"(\w+"+
                           r"|/\*.*?\*/"+
                           r"|\"(?:\\.|[^\"])*\""+
                           r"|\'(?:\\.|[^\'])*\'"+
                           r"|//"+ _until_end +
                           r"|#" + _until_end +
                           r"|:=|::|>=|\*=|\*\*|\*|>"+
                           r"|&=|&&|&"+
                           r"|[!%^&\*\-=+\|\\<>/\]\+]+"+
                           r"|\n"+
                           r"|\s+"+
                           r"|.)", re.M | re.S)

    def __call__(self, source_code):
        return [t for t in self.token_pattern.findall(source_code) if not t.isspace() or t == '\n']

generate_tokens = FreeFormattingTokenizer()


import sys

def print_function_info_header():
    print("==============================================================")
    print("  nloc    CCN  token  param    function@line@file")
    print("--------------------------------------------------------------")

def print_function_info(fun, filename, option):
    line = '{}-{}'.format(fun.start_line, fun.end_line) if option.display_fn_end_line else fun.start_line
    output_params = {
        'nloc': fun.nloc,
        'CCN': fun.cyclomatic_complexity,
        'token': fun.token_count,
        'param': fun.parameter_count,
        'name': fun.name,
        'line': line,
        'file': filename
    }
    output_format = "%(nloc)6d %(CCN)6d %(token)6d %(param)6d    %(name)s@%(line)s@%(file)s"
    if option.verbose:
        output_params['name'] = fun.long_name
    if option.warnings_only:
        output_format = "%(file)s:%(line)s: warning: %(name)s has %(CCN)d CCN and %(param)d params (%(nloc)d NLOC, %(token)d tokens)"
    print(output_format % output_params)

def print_warnings(option, warnings):
    warning_count = 0
    if not option.warnings_only:
        print(("\n" +
               "======================================\n" +
              "!!!! Warnings (CCN > %d) !!!!") % option.CCN)
        print_function_info_header()
    for warning in warnings:
        warning_count += 1
        print_function_info(warning[0], warning[1], option)

    if warning_count == 0:
        print("No warning found. Excellent!")

    return warning_count

def print_total(warning_count, saved_result, option):
    file_infos = list(file_info for file_info in saved_result if file_info)
    all_fun = list(itertools.chain(*(file_info.function_list for file_info in file_infos)))
    cnt = len(all_fun)
    if (cnt == 0):
        cnt = 1
    files_NLOC = sum([f.nloc for f in file_infos])
    functions_NLOC = sum([f.nloc for f in all_fun])
    if (functions_NLOC == 0):
        functions_NLOC = 1
    total_info = (
                  files_NLOC,
                  functions_NLOC / cnt,
                  float(sum([f.cyclomatic_complexity for f in all_fun])) / cnt,
                  float(sum([f.token_count for f in all_fun])) / cnt,
                  cnt,
                  warning_count,
                  float(warning_count) / cnt,
                  float(sum([f.nloc for f in all_fun if f.cyclomatic_complexity > option.CCN])) / functions_NLOC
                  )

    if not option.warnings_only:
        print("=================================================================================")
        print("Total nloc  Avg.nloc  Avg CCN  Avg token  Fun Cnt  Warning cnt   Fun Rt   nloc Rt  ")
        print("--------------------------------------------------------------------------------")
        print("%10d%10d%9.2f%11.2f%9d%13d%10.2f%8.2f" % total_info)

def print_and_save_detail_information(allStatistics, option):
    all_functions = []
    if (option.warnings_only):
        all_functions = allStatistics
    else:
        print_function_info_header()
        for fileStatistics in allStatistics:
            for extension in option.extensions:
                extension.reduce(fileStatistics)
            if fileStatistics:
                all_functions.append(fileStatistics)
                for fun in fileStatistics.function_list:
                    print_function_info(fun, fileStatistics.filename, option)

        print("--------------------------------------------------------------")
        print("%d file analyzed." % (len(all_functions)))
        print("==============================================================")
        print("NLOC    Avg.NLOC AvgCCN Avg.ttoken  function_cnt    file")
        print("--------------------------------------------------------------")
        for fileStatistics in all_functions:
            print("%7d%7d%7d%10d%10d     %s" % (
                            fileStatistics.nloc, 
                            fileStatistics.average_NLOC, 
                            fileStatistics.average_CCN, 
                            fileStatistics.average_token, 
                            len(fileStatistics.function_list), 
                            fileStatistics.filename))

    return all_functions

def print_result(r, option):
    all_functions = print_and_save_detail_information(r, option)
    warnings = warning_filter(option, all_functions)
    warnings = Whitelist(option.whitelist).filter(warnings)
    warning_count = print_warnings(option, warnings)
    print_total(warning_count, all_functions, option)
    for extension in option.extensions:
        extension.print_result()
    if option.number < warning_count:
        sys.exit(1)

class XMLFormatter(object):

    def xml_output(self, result, options):
        ''' Thanks for Holy Wen from Nokia Siemens Networks to let me use his code
            to put the result into xml file that is compatible with cppncss.
            Jenkens has plugin for cppncss format result to display the diagram.
        '''
        import xml.dom.minidom

        impl = xml.dom.minidom.getDOMImplementation()
        doc = impl.createDocument(None, "cppncss", None)
        root = doc.documentElement

        pi = doc.createProcessingInstruction('xml-stylesheet','type="text/xsl" href="https://raw.github.com/terryyin/lizard/master/lizard.xsl"')
        doc.insertBefore(pi, root)

        measure = doc.createElement("measure")
        measure.setAttribute("type", "Function")
        measure.appendChild(self._createLabels(doc, ["Nr.", "NCSS", "CCN"]))

        Nr = 0
        total_func_ncss = 0
        total_func_ccn = 0

        for source_file in result:
            file_name = source_file.filename
            for func in source_file.function_list:
                Nr += 1
                total_func_ncss += func.nloc
                total_func_ccn += func.cyclomatic_complexity
                measure.appendChild(self._createFunctionItem(doc, Nr, file_name, func, options.verbose))

            if Nr != 0:
                measure.appendChild(self._createLabeledValueItem(doc, 'average', "NCSS", str(total_func_ncss / Nr)))
                measure.appendChild(self._createLabeledValueItem(doc, 'average', "CCN", str(total_func_ccn / Nr)))

        root.appendChild(measure)

        measure = doc.createElement("measure")
        measure.setAttribute("type", "File")
        measure.appendChild(self._createLabels(doc, ["Nr.", "NCSS", "CCN", "Functions"]))

        file_NR = 0
        file_total_ncss = 0
        file_total_ccn = 0
        file_total_funcs = 0

        for source_file in result:
            file_NR += 1
            file_total_ncss += source_file.nloc
            file_total_ccn += source_file.CCN
            file_total_funcs += len(source_file.function_list)
            measure.appendChild(self._createFileNode(doc, source_file, file_NR))

        if file_NR != 0:
            fileSummary = [("NCSS", file_total_ncss / file_NR),
                           ("CCN", file_total_ccn / file_NR),
                           ("Functions", file_total_funcs / file_NR)]
            for k, v in fileSummary:
                measure.appendChild(self._createLabeledValueItem(doc, 'average', k, v))

        summary = [("NCSS", file_total_ncss),
                       ("CCN", file_total_ccn ),
                       ("Functions", file_total_funcs)]
        for k, v in summary:
            measure.appendChild(self._createLabeledValueItem(doc, 'sum', k, v))

        root.appendChild(measure)

        xmlString = doc.toprettyxml()
        return xmlString

    def _createLabel(self, doc, name):
        label = doc.createElement("label")
        text1 = doc.createTextNode(name)
        label.appendChild(text1)
        return label

    def _createLabels(self, doc, labelNames):
        labels = doc.createElement("labels")
        for label in labelNames:
            labels.appendChild(self._createLabel(doc, label))

        return labels

    def _createFunctionItem(self, doc, Nr, file_name, func, verbose):
        item = doc.createElement("item")
        if verbose:
            item.setAttribute("name", "%s at %s:%s" % (func.long_name, file_name, func.start_line))
        else:
            item.setAttribute("name", "%s(...) at %s:%s" % (func.name, file_name, func.start_line))
        value1 = doc.createElement("value")
        text1 = doc.createTextNode(str(Nr))
        value1.appendChild(text1)
        item.appendChild(value1)
        value2 = doc.createElement("value")
        text2 = doc.createTextNode(str(func.nloc))
        value2.appendChild(text2)
        item.appendChild(value2)
        value3 = doc.createElement("value")
        text3 = doc.createTextNode(str(func.cyclomatic_complexity))
        value3.appendChild(text3)
        item.appendChild(value3)
        return item


    def _createLabeledValueItem(self, doc, name, label, value):
        average_ncss = doc.createElement(name)
        average_ncss.setAttribute("lable", label)
        average_ncss.setAttribute("value", str(value))
        return average_ncss


    def _createFileNode(self, doc, source_file, file_NR):
        item = doc.createElement("item")
        item.setAttribute("name", source_file.filename)
        value1 = doc.createElement("value")
        text1 = doc.createTextNode(str(file_NR))
        value1.appendChild(text1)
        item.appendChild(value1)
        value2 = doc.createElement("value")
        text2 = doc.createTextNode(str(source_file.nloc))
        value2.appendChild(text2)
        item.appendChild(value2)
        value3 = doc.createElement("value")
        text3 = doc.createTextNode(str(source_file.CCN))
        value3.appendChild(text3)
        item.appendChild(value3)
        value4 = doc.createElement("value")
        text4 = doc.createTextNode(str(len(source_file.function_list)))
        value4.appendChild(text4)
        item.appendChild(value4)
        return item

def print_xml(r, options):
        print (XMLFormatter().xml_output(list(r), options))


def mapFilesToAnalyzer(files, fileAnalyzer, working_threads):
    try:
        # python 2.6 cannot work properly with multiple threading
        if sys.version_info[0:2] == (2, 6):
            raise
        import multiprocessing
        it = multiprocessing.Pool(processes=working_threads)
        mapFun = it.imap_unordered
    except:
        try:
            mapFun = itertools.imap
        except:
            mapFun = map
    r = mapFun(fileAnalyzer, files)
    return r

import os
import fnmatch
import hashlib

class FilesFilter(object):
    
    def __init__(self, exclude_patterns, check_duplicates):
        self.exclude_patterns = exclude_patterns
        self.check_duplicates = check_duplicates
        self.hash_set = set()

    def getFileNames(self, paths):
        for SRC_DIR in paths:
            if os.path.isfile(SRC_DIR) :
                yield SRC_DIR
            else:
                for root, _, files in os.walk(SRC_DIR, topdown=False):
                    for filename in files:
                        full_path_name = os.path.join(root, filename)
                        if self._checkFile(full_path_name):
                            yield full_path_name

    def _checkFile(self, full_path_name):
        if self._notExluded(full_path_name):
            if self.check_duplicates:
                return self._notDuplicate(full_path_name)
            else:
                return True

    def _md5HashFile(self, full_path_name):
        ''' return md5 hash of a file '''
        with open(full_path_name, mode='r') as source_file:
            if sys.version_info[0] == 3:
                code_md5 = hashlib.md5(source_file.read().encode('utf-8'))
            else:
                code_md5 = hashlib.md5(source_file.read())
        return code_md5.hexdigest()

    def _notExluded(self, str_to_match):
        return LanguageChooser().get_language_by_filename(str_to_match) and \
            all(not fnmatch.fnmatch(str_to_match, p) for p in self.exclude_patterns)

    def _notDuplicate(self, full_path_name):
        ''' Function counts md5 hash for the given file 
            and checks if it isn't a duplicate using set 
            of hashes for previous files '''
        fhash = self._md5HashFile(full_path_name)
        if fhash and fhash not in self.hash_set:
            self.hash_set.add(fhash)
            return True

def parse_args(argv):

    def get_extensions(extension_names):
        return [__import__('lizard' + name).LizardExtension() for name in extension_names]

    def get_whitelist():
        whitelist_filename = "whitelizard.txt"
        if os.path.isfile(whitelist_filename):
            return open(whitelist_filename, mode='r').read()
        return ''

    options, args = createCommandLineParser().parse_args(args=argv)
    options.whitelist = get_whitelist()
    options.extensions = get_extensions(options.extensions)
    paths = ["."] if len(args) == 1 else args[1:]
    return paths, options

def lizard_main(argv =  sys.argv):
    paths, options = parse_args(argv)
    printer = print_xml if options.xml else print_result
    r = analyze(paths, options)
    printer(r, options)

if __name__ == "__main__":
    lizard_main()
