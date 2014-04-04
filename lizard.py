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
#
"""
lizard is a simple code complexity analyzer without caring about the C/C++
header files or Java imports.
Please find the README.md for more information.
"""
VERSION = "1.8.0"

import itertools
import re, sys


DEFAULT_CCN_THRESHOLD = 15


def analyze(paths, options):
    ''' returns an iterator of file infomation.
    '''
    files = FilesFilter(options.exclude, options.duplicates).getFileNames(paths)
    fileAnalyzer = FileAnalyzer(options.extensions)
    return mapFilesToAnalyzer(files, fileAnalyzer, options.working_threads)

def createCommandLineParser(prog=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(prog=prog)
    parser.add_argument('paths', nargs='*', default=['.'], help='list of the filename/paths.')
    parser.add_argument('--version', action='version', version=VERSION)
    parser.add_argument("-V", "--verbose",
            help="Output in verbose mode (long function name)",
            action="store_true",
            dest="verbose",
            default=False)
    parser.add_argument("-C", "--CCN",
            help =  "Threshold for cyclomatic complexity number warning. "+
                    "The default value is %d. Functions with CCN bigger than this number will generate warning" % DEFAULT_CCN_THRESHOLD,
            action="store",
            type=int,
            dest="CCN",
            default=DEFAULT_CCN_THRESHOLD)
    parser.add_argument("-a", "--arguments",
            help="Limit for number of parameters",
            action="store",
            type=int,
            dest="arguments",
            default=100)
    parser.add_argument("-w", "--warnings_only",
            help="Show warnings only, using clang/gcc's warning format for printing warnings. http://clang.llvm.org/docs/UsersManual.html#cmdoption-fdiagnostics-format",
            action="store_true",
            dest="warnings_only",
            default=False)
    parser.add_argument("-i", "--ignore_warnings",
            help="If the number of warnings is equal or less than the number, the tool will exit normally, otherwize it will generate error. Useful in makefile when improving legacy code.",
            action="store",
            type=int,
            dest="number",
            default=0)
    parser.add_argument("-x", "--exclude",
            help="Exclude files that match this pattern. * matches everything, ? matches any single characoter, \"./folder/*\" exclude everything in the folder, recursively. Multiple patterns can be specified. Don't forget to add \"\" around the pattern.",
            action="append",
            dest="exclude",
            default=[])
    parser.add_argument("-X", "--xml",
            help="Generate XML in cppncss style instead of the normal tabular output. Useful to generate report in Jenkins server",
            action="store_true",
            dest="xml",
            default=None)
    parser.add_argument("-P", "--no_preprocessor_count",
            help="By default, a #if will also increase the complexity. Adding this option to ignore them",
            action="store_true",
            dest="no_preprocessor_count",
            default=False)
    parser.add_argument("-t", "--working_threads",
            help="number of working threads. The default value is 1.",
            action="store",
            type=int,
            dest="working_threads",
            default=1)
    parser.add_argument("-d", "--find_duplicates",
            help="find and skip analysis for identical files. Will be made default in the next release",
            action="store_true",
            dest="duplicates",
            default=False)
    parser.add_argument("-e", "--display_fn_end_line",
            help="display function end line number in addition to start line number. Will be made default in the next release",
            action="store_true",
            dest="display_fn_end_line",
            default=False)
    parser.add_argument("-m", "--modified",
            help="Calculate modified cyclomatic complexity number",
            action="store_true",
            dest="switchCasesAsOneCondition",
            default=False)
    parser.add_argument("-E", "--extension",
            help="under construction...", #"Use extension. Can be WordCount.",
            action="append",
            dest="extensions",
            default=[])
    parser.add_argument("-s", "--sort",
            help="Sort the warning with field. The field can be nloc, cyclomatic_complexity, token_count, parameter_count, etc. Or an customized file.",
            action="append",
            dest="sorting",
            default=[])

    parser.usage = "lizard [options] [PATH or FILE] [PATH] ... "
    parser.description = __doc__
    return parser


class FunctionInfo(object):

    def __init__(self, name, start_line = 0, ccn = 1):
        self.cyclomatic_complexity = ccn
        self.nloc = 0
        self.token_count = 1  # the first token
        self.name = name
        self.long_name = name
        self.start_line = start_line
        self.end_line = start_line
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
        self.reader = (CodeReader.get_reader(filename) or CLikeReader)()

    def START_NEW_FUNCTION(self, name):
        self.newline = True
        self.current_function = FunctionInfo(name, self.current_line)

    def CONDITION(self, inc = 1):
        self.current_function.cyclomatic_complexity += inc

    def ADD_TO_LONG_FUNCTION_NAME(self, app):
        self.current_function.add_to_long_name(app)

    def ADD_TO_FUNCTION_NAME(self, app):
        self.current_function.add_to_function_name(app)

    def PARAMETER(self, token):
        self.current_function.add_parameter(token)

    def END_OF_FUNCTION(self):
        if not self.forgive:
            self.fileinfo.function_list.append(self.current_function)
        self.forgive = False
        self.START_NEW_FUNCTION('')


class Preprocessor(object):

    def extend_tokens(self, tokens, context):
        if hasattr(context.reader, "preprocess"):
            return context.reader.preprocess(tokens, context)
        else:
            return (t for t in tokens if not t.isspace() or t == '\n')


class CommentCounter(object):

    def extend_tokens(self, tokens, context):
        for token in tokens:
            if token.startswith("/*") or token.startswith("//"):
                if len(token.splitlines()) > 1:
                    yield '\n'
                if token[2:].strip().startswith("#lizard forgive"):
                    context.forgive = True
            else:
                yield token


class LineCounter(object):
    ''' And extension of lizard, to count the line and nloc.
    '''

    FUNCTION_CAPTION = "  NLOC  "
    FUNCTION_INFO_PART = "nloc"


    def extend_tokens(self, tokens, context):
        context.current_line = 1 
        for token in tokens:
            context.current_line += 1 if token == '\n' else (len(token.splitlines()) - 1)
            if token == "\n":
                self._new_line(context)
            else:
                yield token

    def _new_line(self, context):
        context.fileinfo.nloc += 1
        context.newline = True


class TokenCounter(object):
    ''' An extension that counts the token.
        But because of historical and implementation reason, the token
        counting is already done in LineCounter. So this class only do
        the output.
    '''

    FUNCTION_CAPTION = " token "
    FUNCTION_INFO_PART = "token_count"

    def extend_tokens(self, tokens, context):
        for token in tokens:
            self._count_token(context)
            yield token

    def _count_token(self, context):
        context.fileinfo.token_count+=1
        if context.newline:
            context.current_function.nloc += 1
            context.newline = False
            context.current_function.end_line = context.current_line
        context.current_function.token_count += 1


class ParameterCounter(object):
    ''' An extension that counts the parameters.
        But because of historical and implementation reason, the parameter
        counting is already done in FunctionCounter. So this class only do
        the output.
    '''

    FUNCTION_CAPTION = " PARAM "
    FUNCTION_INFO_PART = "parameter_count"

    def extend_tokens(self, tokens, context):
        return tokens


class ConditionCounter(object):

    FUNCTION_CAPTION = "  CCN  "
    FUNCTION_INFO_PART = "cyclomatic_complexity"
    conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch', 'case'])

    def is_condition(self, token):
        return token in self.conditions

    def extend_tokens(self, tokens, context):
        for token in tokens:
            if self.is_condition(token):
                context.CONDITION()
            yield token


class SwitchCasesAsOneConditionCounter(object):

    def extend_tokens(self, tokens, context):
        for token in tokens:
            if token == 'switch':
                context.CONDITION()
            elif token == 'case':
                context.CONDITION(-1)
            yield token


class PreprocessorConditionCounter(object):

    def extend_tokens(self, tokens, context):
        for token in tokens:
            if token in ('#if', '#ifdef', '#elif'):
                context.CONDITION()
            yield token


#----------------------------------------------
# CodeReaders are used to parse functions structures from code of different
# language. Each language will need a subclass of CodeReader.

class CodeReader(object):

    @staticmethod
    def compile_file_extension_re(*exts):
        return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.IGNORECASE)

    @staticmethod
    def get_reader(filename):
        for lan in list(CodeReader.__subclasses__()):
            if CodeReader.compile_file_extension_re(*lan.ext).match(filename):
                return lan

    def eof(self): pass


try: # lizard.py can run as a stand alone python script, without the extensions
    from lizard_ext import JavaScriptReader
    from lizard_ext import PythonReader
except:
    pass

class CLikeReader(CodeReader):
    '''
    This is the reader for C, C++ and Java.
    '''

    ext = ["c", "cpp", "cc", "mm", "cxx", "h", "hpp"]
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __init__(self):
        self.bracket_level = 0
        self.br_count = 0
        self.last_preprocessor = None
        self._state = self._GLOBAL

    def preprocess(self, tokens, context):
        for token in tokens:
            if not token.isspace() or token == '\n':
                m = self.macro_pattern.match(token)
                if m:
                    yield "#" + m.group(1)
                    param = m.group(2).strip()
                    if param:
                        yield param
                else:
                    yield token

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

class JavaReader(CLikeReader, CodeReader):

    ext = ['java']

class ObjCReader(CLikeReader, CodeReader):

    ext = ['m']

    def __init__(self):
        super(ObjCReader, self).__init__()

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


class FunctionParser(object):
    ''' FunctionParser parse source code into functions. This is different from language
        to language. So FunctionParser need a language specific 'reader' to actually do
        the job.
        TODO: FunctionParser might yield functions in the future.
    '''
    def extend_tokens(self, tokens, context):
        function_reader = context.reader
        function_reader.context = context
        for token in tokens:
            function_reader._state(token)
            yield token
        function_reader.eof()


class FunctionOutputPlaceholder(object):
    ''' FunctionParser parse source code into functions. This is different from language
        to language. So FunctionParser need a language specific 'reader' to actually do
        the job.
        TODO: FunctionParser might yield functions in the future.
    '''

    FUNCTION_CAPTION = " function@line@filename          "

    def extend_tokens(self, tokens, context):
        for token in tokens:
            pass


def generate_tokens(source_code):
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
                           r"|[^\S\n]+"+
                           r"|.)", re.M | re.S)
    return token_pattern.findall(source_code)


class FileAnalyzer(object):

    def __init__(self, extensions = []):
        self.processors = extensions

    def __call__(self, filename):
        try:
            return self.analyze_source_code(filename, open(filename).read())
        except IOError:
            sys.stderr.write("Error: Fail to read source file '%s'"%filename)

    def analyze_source_code(self, filename, code):
        tokens = generate_tokens(code)
        context = CodeInfoContext(filename)
        for processor in self.processors:
            tokens = processor.extend_tokens(tokens, context)
        return context.fileinfo


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


def print_function_info_header(extensions):
    captions = "".join(ext.FUNCTION_CAPTION for ext in extensions if hasattr(ext, "FUNCTION_CAPTION"))
    print("=" * len(captions))
    print(captions)
    print("-" * len(captions))

def print_function_info(fun, filename, extensions, option):
    ll = ''
    for ext in extensions:
        if hasattr(ext, "FUNCTION_INFO_PART"):
            ll += str(getattr(fun, ext.FUNCTION_INFO_PART)).rjust(len(ext.FUNCTION_CAPTION))
    line = '{}-{}'.format(fun.start_line, fun.end_line) if option.display_fn_end_line else fun.start_line
    output_params = {
        'nloc': fun.nloc,
        'CCN': fun.cyclomatic_complexity,
        'token': fun.token_count,
        'param': fun.parameter_count,
        'name': fun.long_name if option.verbose else fun.name,
        'line': line,
        'file': filename
    }

    output_format = ll + " %(name)s@%(line)s@%(file)s"
    if option.warnings_only:
        output_format = "%(file)s:%(line)s: warning: %(name)s has %(CCN)d CCN and %(param)d params (%(nloc)d NLOC, %(token)d tokens)"
    print(output_format % output_params)

def print_warnings(option, warnings):
    warning_count = 0
    if type(option.sorting) is type([]) and len(option.sorting) > 0:
        warnings = list(warnings)
        warnings.sort(reverse = True, key=lambda x: getattr(x[0], option.sorting[0]))
    if not option.warnings_only:
        print(("\n" +
               "======================================\n" +
              "!!!! Warnings (CCN > %d) !!!!") % option.CCN)
        print_function_info_header(option.extensions)
    for warning in warnings:
        warning_count += 1
        print_function_info(warning[0], warning[1], option.extensions,  option)

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
        print_function_info_header(option.extensions)
        for fileStatistics in allStatistics:
            for extension in option.extensions:
                if hasattr(extension, 'reduce'):
                    extension.reduce(fileStatistics)
            if fileStatistics:
                all_functions.append(fileStatistics)
                for fun in fileStatistics.function_list:
                    print_function_info(fun, fileStatistics.filename, option.extensions, option)

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
        if hasattr(extension, 'print_result'):
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
        return CodeReader.get_reader(str_to_match) and \
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

    def get_whitelist():
        whitelist_filename = "whitelizard.txt"
        if os.path.isfile(whitelist_filename):
            return open(whitelist_filename, mode='r').read()
        return ''

    options = createCommandLineParser(argv[0]).parse_args(args=argv[1:])
    options.whitelist = get_whitelist()
    options.extensions = get_extensions(options.extensions, not options.no_preprocessor_count, options.switchCasesAsOneCondition)
    function_parts = [getattr(ext, 'FUNCTION_INFO_PART') for ext in options.extensions
            if hasattr(ext, 'FUNCTION_INFO_PART')]
    for sort_factor in options.sorting:
        if sort_factor not in function_parts:
            error_message =  "Wrong sorting field '%s'.\n" % sort_factor
            error_message +=  "Candidates are: " + str(function_parts) + "\n"
            sys.stderr.write(error_message)
            sys.exit(2)
    return options

def get_extensions(extension_names, countPreprocessor = True, switchCaseAsOneCondition = False):
    from importlib import import_module
    extensions = [
        Preprocessor(),
        CommentCounter(),
        LineCounter(),
        ConditionCounter(),
        TokenCounter(),
        FunctionParser(),
        ParameterCounter()
        ]
    if countPreprocessor:
        extensions.append(PreprocessorConditionCounter())
    if switchCaseAsOneCondition:
        extensions.append(SwitchCasesAsOneConditionCounter())

    return extensions +\
        [import_module('lizard_ext.lizard' + name).LizardExtension() if type(name) == type("") else name  for name in extension_names] +\
        [FunctionOutputPlaceholder()]

analyze_file = FileAnalyzer(get_extensions([]))

def lizard_main(argv =  sys.argv):
    options = parse_args(argv)
    printer = print_xml if options.xml else print_result
    r = analyze(options.paths, options)
    printer(r, options)

if __name__ == "__main__":
    lizard_main()
