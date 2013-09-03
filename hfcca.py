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
hfcca is a simple code complexity analyzer without caring about the C/C++ 
header files or Java imports. It can deal with

* Java
* /C/C++
* Objective C.

It counts 

* the nloc (lines of code without comments), 
* CCN (cyclomatic complexity number),
* token count of functions.
* parameter count of functions.

You can set limitation for CCN (-C), the number of parameters (-a). Functions
that exceed these limitations will generate warnings. The exit code of hfcca
will be none-Zero if there are warnings. 

This tool actually calculates how complex the code 'looks' rather than how
complex the code real 'is'. People will need this tool because it's often very
hard to get all the included folders and files right when they are complicated.
But we don't really need that kind of accuracy when come to cyclomatic
complexity.

It requires python2.6 or above (early versions are not verified).
"""

VERSION = "1.7.1"

import itertools

DEFAULT_CCN_THRESHOLD = 15

class FunctionInfo(object):
    ''' Statistic information of a function.
    '''
    def __init__(self, name, start_line):
        self.cyclomatic_complexity = 1
        self.nloc = 0
        self.token_count = 0
        self.name = name
        self.function_name_with_param = name
        self.start_line = start_line
        self.parameter_count = 0

    def __eq__(self, other):
        return other == self.name

    def add_to_function_name(self, app):
        self.name += app
        self.function_name_with_param += app

    def long_name(self):
        return self.function_name_with_param

    def add_to_long_name(self, app):
        self.function_name_with_param += app

    def add_condition(self):
        self.cyclomatic_complexity += 1

    def add_token(self):
        self.token_count += 1

    def add_non_comment_line(self):
        self.nloc += 1

    def add_parameter(self, token):
        if self.parameter_count == 0:
            self.parameter_count = 1
        if token == ",":
            self.parameter_count += 1

class FileInformation(list):
    ''' Statistic information of a source file.
        Including all the functions and the file summary.
    '''
    def __init__(self, filename):
        list.__init__(self)
        self.filename = filename

    def summarize(self, NLOC):
        self.nloc = NLOC
        self.average_NLOC = 0
        self.average_CCN = 0
        self.average_token = 0

        nloc = 0
        ccn = 0
        token = 0
        for fun in self:
            nloc += fun.nloc
            ccn += fun.cyclomatic_complexity
            token += fun.token_count
        fc = len(self)
        if fc > 0:
            self.average_NLOC = nloc / fc
            self.average_CCN = ccn / fc
            self.average_token = token / fc

        self.CCN = ccn
        self.token = token

class UniversalAnalyzer(list):
    """
        UniversalCode is the code that is unrelated to any programming
        languages. The code could be:
        START_NEW_FUNCTION
            ADD_TO_FUNCTION_NAME
            ADD_TO_LONG_FUNCTION_NAME
                PARAMETER
            CONDITION
            TOKEN
        END_OF_FUNCTION

        A TokenTranslator will generate UniversalCode.
    """
    def __init__(self):
        self.current_function = None
        self.newline = True
        self.nloc = 0

    def analyze(self, parsed_code, filename):
        fileInfo = FileInformation(filename)
        for fun in self._functions(parsed_code):
            fileInfo.append(fun)
        fileInfo.summarize(self.nloc)
        return fileInfo

    def START_NEW_FUNCTION(self, name_and_line):
        self.current_function = FunctionInfo(*name_and_line)

    def CONDITION(self, token):
        self.TOKEN(token)
        self.current_function.add_condition()

    def TOKEN(self, text):
        if self.newline:
            self.current_function.add_non_comment_line()
            self.newline = False
        self.current_function.add_token()

    def NEW_LINE(self, token):
        self.nloc += 1
        self.newline = True

    def ADD_TO_LONG_FUNCTION_NAME(self, app):
        self.current_function.add_to_long_name(app)

    def ADD_TO_FUNCTION_NAME(self, app):
        self.current_function.add_to_function_name(app)

    def PARAMETER(self, token):
        self.current_function.add_parameter(token)
        self.ADD_TO_LONG_FUNCTION_NAME(" " + token)

    END_OF_FUNCTION = 1

    def _functions(self, parsed_code):
        for code, text in parsed_code:
            if code is UniversalAnalyzer.END_OF_FUNCTION:
                yield self.current_function
            else:
                code(self, text)

class TokenTranslatorBase:

    def __init__(self):
        self._state = self._GLOBAL
        self._current_line = 0

    def get_current_line(self):
        return self._current_line

    def getFunctions(self, tokens):
        for token, self._current_line in tokens:
            fun = self._read_token(token)
            if fun: yield fun

    def _read_token(self, token):
        if token.isspace():
            return UniversalAnalyzer.NEW_LINE, None
        else:
            return self._state(token)
    def remove_hash_if(self):
        self.conditions.remove("#if")


class CTokenTranslator(TokenTranslatorBase):

    def __init__(self):
        TokenTranslatorBase.__init__(self)
        self.conditions = set(
            ['if', 'for', 'while', '&&', '||', 'case', '?', '#if', 'catch'])
        self.bracket_level = 0
        self.br_count = 0
        self.last_preprocessor = None

    def _is_condition(self, token):
        return token in self.conditions

    def _GLOBAL(self, token):
        if token == '(':
            self.bracket_level += 1
            self._state = self._DEC
            return UniversalAnalyzer.ADD_TO_LONG_FUNCTION_NAME, token
        elif token == '::':
            self._state = self._NAMESPACE
        else:
            if token == 'operator':
                self._state = self._OPERATOR
            return UniversalAnalyzer.START_NEW_FUNCTION,\
                    (token, self._current_line)


    def _OPERATOR(self, token):
        if token != '(':
            self._state = self._GLOBAL
        return UniversalAnalyzer.ADD_TO_FUNCTION_NAME, ' ' + token

    def _NAMESPACE(self, token):
        self._state = self._OPERATOR if token == 'operator'  else self._GLOBAL
        return UniversalAnalyzer.ADD_TO_FUNCTION_NAME, "::" + token

    def _DEC(self, token):
        if token in ('(', "<"):
            self.bracket_level += 1
        elif token in (')', ">"):
            self.bracket_level -= 1
            if (self.bracket_level == 0):
                self._state = self._DEC_TO_IMP
        elif self.bracket_level == 1:
            return UniversalAnalyzer.PARAMETER, token
        return UniversalAnalyzer.ADD_TO_LONG_FUNCTION_NAME, " " + token

    def _DEC_TO_IMP(self, token):
        if token == 'const':
            return UniversalAnalyzer.ADD_TO_LONG_FUNCTION_NAME, " " + token
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._GLOBAL

    def _IMP(self, token):
        if token in ("#else", "#if", "#endif"):
            self.last_preprocessor = token
        # will ignore the braces in a #else branch            
        if self.last_preprocessor != '#else':
            if token == '{':
                self.br_count += 1
            elif token == '}':
                self.br_count -= 1
                if self.br_count == 0:
                    self._state = self._GLOBAL
                    return UniversalAnalyzer.END_OF_FUNCTION, ""
        if self._is_condition(token):
            return UniversalAnalyzer.CONDITION, token
        return UniversalAnalyzer.TOKEN, token

class ObjCTokenTranslator(CTokenTranslator):
    def __init__(self):
        CTokenTranslator.__init__(self)

    def _DEC_TO_IMP(self, token):
        if token in ("+", "-"):
            self._state = self._GLOBAL
        else:
            CTokenTranslator._DEC_TO_IMP(self, token)
            if self._state == self._GLOBAL:
                self._state = self._OBJC_DEC_BEGIN
                return UniversalAnalyzer.START_NEW_FUNCTION,\
                        (token, self._current_line)
    def _OBJC_DEC_BEGIN(self, token):
        if token == ':':
            self._state = self._OBJC_DEC
            return UniversalAnalyzer.ADD_TO_FUNCTION_NAME, token
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._GLOBAL
    def _OBJC_DEC(self, token):
        if token == '(':
            self._state = self._OBJC_PARAM_TYPE
            return UniversalAnalyzer.ADD_TO_LONG_FUNCTION_NAME, token
        elif token == ',':
            pass
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._OBJC_DEC_BEGIN
            return UniversalAnalyzer.ADD_TO_FUNCTION_NAME, " " + token

    def _OBJC_PARAM_TYPE(self, token):
        if token == ')':
            self._state = self._OBJC_PARAM
        return UniversalAnalyzer.ADD_TO_LONG_FUNCTION_NAME, " " + token
    def _OBJC_PARAM(self, token):
        self._state = self._OBJC_DEC

import re

c_pattern = re.compile(r".*\.(c|C|cpp|CPP|CC|cc|mm)$")
java_pattern = re.compile(r".*\.(java)$")
objc_pattern = re.compile(r".*\.(m)$")

hfcca_language_infos = {
                 'c/c++': {
                  'name_pattern': c_pattern,
                  'creator':CTokenTranslator},
                 'Java': {
                  'name_pattern': java_pattern,
                  'creator':CTokenTranslator},
                  'objC' : {
                  'name_pattern': objc_pattern,
                  'creator':ObjCTokenTranslator}
            }

def get_parser_by_file_name(filename):
    for lan in hfcca_language_infos:
        info = hfcca_language_infos[lan]
        if info['name_pattern'].match(filename):
            return info['creator']

def get_parser_by_file_name_otherwise_default(filename):
    for lan in hfcca_language_infos:
        info = hfcca_language_infos[lan]
        if info['name_pattern'].match(filename):
            return info['creator']()
    return hfcca_language_infos['c/c++']['creator']()

class FileAnalyzer:
    ''' A FileAnalyzer works as a function. It takes filename as parameter.
        Returns a list of function infos in this file.
    '''

    open = open

    def __init__(self, noCountPre=False):
        self.noCountPre = noCountPre

    def __call__(self, filename):
        return self.analyze(filename)


    def analyze_source_code_with_parser1(self, filename, code, parser):
        tokens = generate_tokens(code)
        result = self.analyze_source_code_with_parser(filename, parser, tokens)
        return result

    def analyze(self, filename):
        code = self._readFileContent(filename)
        parser = get_parser_by_file_name_otherwise_default(filename)
        if self.noCountPre:
            parser.remove_hash_if()
        result = self.analyze_source_code_with_parser1(filename, code, parser)
        return result


    def analyze_source_code_with_parser(self, filename, parser, tokens):
        result = UniversalAnalyzer().analyze(
                            parser.getFunctions(tokens), filename)
        return result

    def _readFileContent(self, filename):
        f = self.open(filename)
        code = f.read()
        f.close()
        return code

token_pattern = re.compile(r"(\w+|/\*|//|:=|::|>=|\*=|\*\*|\*|>"+
                           r"|&=|&&|&"+
                           r"|#\s*define|#\s*if|#\s*else|#\s*endif|#\s*\w+"+
                           r"|[!%^&\*\-=+\|\\<>/\]\+]+|.)", re.M | re.S)

def generate_tokens(source_code):
    for t, l in generate_tokens_from_code(source_code):
        if not any(t.startswith(x) for x in ('#define', '/*', '//')) :
            yield t, l

def generate_tokens_from_code(source_code):
    in_middle_of_empty_lines = False
    for (token, line) in tokens_from_code_with_multiple_newlines(source_code):
        if token != '\n' or not in_middle_of_empty_lines:
            yield token, line
        in_middle_of_empty_lines = (token == '\n')

def tokens_from_code_with_multiple_newlines(source_code):
    index = 0
    line = 1
    while 1:
        m = token_pattern.match(source_code, index)
        if not m:
            break
        token = m.group(0)
        if token == '\n': line += 1

        if token.startswith("#"):
            token = "#" + token[1:].strip()

        if token == "#define":
            while(1):
                bindex = index + 1
                index = source_code.find('\n', bindex)
                if index == -1:
                    break
                if not source_code[bindex:index].rstrip().endswith('\\'):
                    break
            if index == -1:
                break
            token = source_code[m.start(0):index]
        elif token == '/*':
            index = source_code.find("*/", index + 2)
            if index == -1:
                break
            index += 2
            token = source_code[m.start(0):index]
        elif token in( '//', '#if','#endif'):
            index = source_code.find('\n', index)
            if index == -1:
                break
        elif token == '"' or token == '\'':
            while(1):
                index += 1
                index = source_code.find(token, index)
                if index == -1:
                    break
                if source_code[index - 1] == '\\':
                    if source_code[index - 2] != '\\':
                        continue
                break
            if index == -1:
                break
            token = source_code[m.start(0):index + 1]
            index = index + 1
        else:
            index = m.end(0)
        line += (len(token.splitlines()) - 1)
        if not token.isspace() or token == '\n':
            yield token, line

import sys

def print_function_info_header():
    print("==============================================================")
    print("  nloc    CCN  token  param    function@line@file")
    print("--------------------------------------------------------------")

def print_function_info(fun, filename, option):
    output_params = {
        'nloc': fun.nloc,
        'CCN': fun.cyclomatic_complexity,
        'token': fun.token_count,
        'param': fun.parameter_count,
        'name': fun.name,
        'line': fun.start_line,
        'file': filename
    }
    output_format = "%(nloc)6d %(CCN)6d %(token)6d %(param)6d    %(name)s@%(line)s@%(file)s"
    if option.verbose:
        output_params['name'] = fun.long_name()
    if option.warnings_only:
        output_format = "%(file)s:%(line)s: warning: %(name)s has %(CCN)d CCN and %(param)d params (%(nloc)d NLOC, %(token)d tokens)"
    print(output_format % output_params)

def print_warnings(option, saved_result):
    warning_count = 0
    if not option.warnings_only:
        print(("\n" +
              "======================================\n" +
              "!!!! Warnings (CCN > %d) !!!!\n" +
              "======================================") % option.CCN)
    for f in saved_result:
        for fun in f:
            if fun.cyclomatic_complexity > option.CCN or \
                    fun.parameter_count > option.arguments:
                warning_count += 1
                print_function_info(fun, f.filename, option)

    if warning_count == 0:
        print("No warning found. Excellent!")

    return warning_count

def print_total(warning_count, saved_result, option):
    all_fun = list(itertools.chain(*saved_result))
    cnt = len(all_fun)
    if (cnt == 0):
        cnt = 1
    tNLOC = sum([f.nloc for f in all_fun])
    if (tNLOC == 0):
        tNLOC = 1
    total_info = (
                  tNLOC, tNLOC / cnt,
                  float(sum([f.cyclomatic_complexity for f in all_fun])) / cnt,
                  float(sum([f.token_count for f in all_fun])) / cnt,
                  cnt,
                  warning_count,
                  float(warning_count) / cnt,
                  float(sum([f.nloc for f in all_fun if f.cyclomatic_complexity > option.CCN])) / tNLOC
                  )

    if not option.warnings_only:
        print("=================================================================================")
        print("Total nloc  Avg.nloc  Avg CCN  Avg token  Fun Cnt  Warning cnt   Fun Rt   nloc Rt  ")
        print("--------------------------------------------------------------------------------")
        print("%10d%10d%9.2f%11.2f%9d%13d%10.2f%8.2f" % total_info)

def print_and_save_detail_information(allStatistics, option):
    saved_result = []
    if (option.warnings_only):
        saved_result = allStatistics
    else:
        print_function_info_header()
        for fileStatistics in allStatistics:
            saved_result.append(fileStatistics)
            for fun in fileStatistics:
                print_function_info(fun, fileStatistics.filename, option)

        print("--------------------------------------------------------------")
        print("%d file analyzed." % (len(saved_result)))
        print("==============================================================")
        print("NLOC    Avg.NLOC AvgCCN Avg.ttoken  function_cnt    file")
        print("--------------------------------------------------------------")
        for fileStatistics in saved_result:
            print("%7d%7d%7d%10d%10d     %s" % (fileStatistics.nloc, fileStatistics.average_NLOC, fileStatistics.average_CCN, fileStatistics.average_token, len(fileStatistics), fileStatistics.filename))

    return saved_result

def print_result(r, option):
    saved_result = print_and_save_detail_information(r, option)
    warning_count = print_warnings(option, saved_result)
    print_total(warning_count, saved_result, option)
    if option.number > warning_count:
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
    
        measure = doc.createElement("measure")
        measure.setAttribute("type", "Function")
        measure.appendChild(self._createLabels(doc, ["Nr.", "NCSS", "CCN"]))
    
        Nr = 0
        total_func_ncss = 0
        total_func_ccn = 0
    
        for source_file in result:
            file_name = source_file.filename
            for func in source_file:
                Nr += 1
                total_func_ncss += func.nloc
                total_func_ccn += func.cyclomatic_complexity
                measure.appendChild(self._createFunctionItem(doc, Nr, file_name, func))
    
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
            file_total_funcs += len(source_file)
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

    def _createFunctionItem(self, doc, Nr, file_name, func):
        item = doc.createElement("item")
        item.setAttribute("name", "%s(...) at %s:0" % (func.name, file_name))
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
        text4 = doc.createTextNode(str(len(source_file)))
        value4.appendChild(text4)
        item.appendChild(value4)
        return item

def createHfccaCommandLineParser():
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

    parser.usage = "hfcca [options] [PATH or FILE] [PATH] ... "
    parser.description = __doc__
    return parser

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

def _notExluded(str_to_match, patterns):
    return get_parser_by_file_name(str_to_match) and \
        all(not fnmatch.fnmatch(str_to_match, p) for p in patterns)

def getSourceFiles(SRC_DIRs, exclude_patterns):
    for SRC_DIR in SRC_DIRs:
        if os.path.isfile(SRC_DIR) and get_parser_by_file_name(SRC_DIR):
            yield SRC_DIR
        else:
            for root, _, files in os.walk(SRC_DIR, topdown=False):
                for filename in files:
                    full_path_name = os.path.join(root, filename)
                    if _notExluded(full_path_name, exclude_patterns):
                        yield full_path_name

def analyze(paths, options):
    ''' This is the most important function of hfcca.
        It analyze the given paths with the options.
        Can be used directly by other Python application.
    '''
    files = getSourceFiles(paths, options.exclude)
    fileAnalyzer = FileAnalyzer(options.no_preprocessor_count)
    r = mapFilesToAnalyzer(files, fileAnalyzer, options.working_threads)
    return r

def hfcca_main(argv):
    options, args = createHfccaCommandLineParser().parse_args(args=argv)
    paths = ["."] if len(args) == 1 else args[1:]
    r = analyze(paths, options)
    if options.xml:
        print (XMLFormatter().xml_output(list(r), options))
    else:
        print_result(r, options)

def main():
    hfcca_main(sys.argv)

if __name__ == "__main__":
    main()
