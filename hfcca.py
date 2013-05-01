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
source_analyzer (verion 1.5.2) is a simple code complexity source_file_counter without caring about the C/C++ header files.
It can deal with C/C++/Objective C & TNSDL code. It count the NLOC (lines of code without comments), CCN 
(cyclomatic complexity number) and token count of _functions.
It requires python2.6 or above (early versions are not verified).
To install it on your computer(so that you don't need to do python myPath/hfcca/hfcca.py all the time):
python hfcca.py install
Or,
sudo python hfcca.py install
Then you can just type hfcca.py from any where in your command line.
"""
import itertools

DEFAULT_CCN_THRESHOLD = 15

class FunctionInfo:
    def __init__(self, name, start_line):
        self.cyclomatic_complexity = 1
        self.NLOC = 1
        self.token_count = 0
        self.name = name
        self.function_name_with_param = name
        self.return_type = ""
        self.start_line = start_line
        self.parameter_count = 0
    def __eq__(self, other): return other == self.name
    def add_to_function_name(self, app):
        self.name += app
        self.function_name_with_param += app
    def long_name(self):
        return self.return_type + self.function_name_with_param
    def add_to_long_name(self, app):
        self.function_name_with_param += app
    def add_condition(self): self.cyclomatic_complexity += 1
    def add_token(self): self.token_count += 1
    def add_non_comment_line(self): self.NLOC += 1
    def add_parameter(self, token):
        if self.parameter_count == 0:
            self.parameter_count = 1
        if token == ",":
            self.parameter_count += 1

class DefaultPreprocessor:
    def process(self, tokens):
        for token in tokens:
            yield token

class CPreprocessor:
    def __init__(self):
        self.stack = []
    def process(self, tokens):
        for token, line in tokens:
            if not self.in_else(token):
                yield token, line
    def in_else(self, token):
        if token == '#if':
            self.stack.append(token)
        elif token == '#else':
            self.stack.append(token)
        elif token == '#endif':
            while len(self.stack) and self.stack.pop() != "#if":
                pass
        return token.startswith("#") or '#else' in self.stack

class UniversalCodeCounter(list):
    """
        UniversalCode is the code that is unrelated to any programming languages. The code could be:
        START_NEW_FUNCTION
            ADD_TO_FUNCTION_NAME
            ADD_TO_LONG_FUNCTION_NAME
                PARAMETER
            CONDITION
            TOKEN
        END_OF_FUNCTION
        
        A TokenTranslator will generate UniversalCode.
    """
    def __init__(self, parsed_code, filename):
        self.NLOC = 0
        self.current_function = None
        self.filename = filename
        self.functionInfos = []
        for fun in self._functions(parsed_code.process()):
            self.append(fun)
        self.LOC = parsed_code.get_current_line()
        self._summarize()
    def countCode(self):
        return self
    def START_NEW_FUNCTION(self, name_and_line):
        self.current_function = FunctionInfo(*name_and_line)
    def CONDITION(self, token): 
        self.TOKEN(token)
        self.current_function.add_condition()
    def TOKEN(self, text):
        self.current_function.add_token()
    def NEW_LINE(self, token): 
        self.NLOC += 1
        if self.current_function is not None:
            self.current_function.add_non_comment_line()
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
            if code == UniversalCodeCounter.END_OF_FUNCTION:
                yield self.current_function
            else:
                code(self, text)
    def _summarize(self):
        self.average_NLOC = 0
        self.average_CCN = 0
        self.average_token = 0
        
        nloc = 0
        ccn = 0
        token = 0
        for fun in self:
            nloc += fun.NLOC
            ccn += fun.cyclomatic_complexity
            token += fun.token_count
        fc = len(self)
        if fc > 0:
            self.average_NLOC = nloc / fc
            self.average_CCN = ccn / fc
            self.average_token = token / fc
    
        self.NLOC = nloc
        self.CCN = ccn
        self.token = token

class TokenTranslatorBase:
    def __init__(self, token):
        self._tokens = token
        self._state = self._GLOBAL
        self._current_line = 0
    def get_current_line(self):
        return self._current_line
    def process(self):
        for token, self._current_line in self._tokens:
            fun = self._read_token(token)
            if fun: yield fun
    def _read_token(self, token):
        if token.isspace():
            return UniversalCodeCounter.NEW_LINE, None
        else:
            return self._state(token)
    
class CTokenTranslator(TokenTranslatorBase):
    conditions = set(['if', 'for', 'while', '&&', '||', 'case', '?', '#if', 'catch'])
    def __init__(self, token):
        TokenTranslatorBase.__init__(self, token)
        self.pa_count = 0
        self.br_count = 0
    def _is_condition(self, token):
        return token in self.conditions
    def _GLOBAL(self, token):
        if token == '(':
            self.pa_count += 1
            self._state = self._DEC
            return UniversalCodeCounter.ADD_TO_LONG_FUNCTION_NAME, token
        elif token == '::':
            self._state = self._NAMESPACE
        else:
            return UniversalCodeCounter.START_NEW_FUNCTION, (token, self._current_line)
    def _NAMESPACE(self, token):
            self._state = self._GLOBAL
            return UniversalCodeCounter.ADD_TO_FUNCTION_NAME, "::" + token
    def _DEC(self, token):
        if token == '(' or token == "<":
            self.pa_count += 1
        elif token == ')' or token == ">" or token == "*>":
            self.pa_count -= 1
            if (self.pa_count == 0):
                self._state = self._DEC_TO_IMP
        elif self.pa_count == 1:
            return UniversalCodeCounter.PARAMETER, token
        return UniversalCodeCounter.ADD_TO_LONG_FUNCTION_NAME, " " + token
    def _DEC_TO_IMP(self, token):
        if token == 'const':
            return UniversalCodeCounter.ADD_TO_LONG_FUNCTION_NAME, " " + token
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._GLOBAL
    def _IMP(self, token):
        if token == '{':
            self.br_count += 1
        elif token == '}':
            self.br_count -= 1
            if self.br_count == 0:
                self._state = self._GLOBAL
                return UniversalCodeCounter.END_OF_FUNCTION, ""      
        else:
            if self._is_condition(token):
                return UniversalCodeCounter.CONDITION, token
            if token not in '();':
                return UniversalCodeCounter.TOKEN, token
        
class ObjCTokenTranslator(CTokenTranslator):
    def __init__(self, token):
        CTokenTranslator.__init__(self, token)
    def _DEC_TO_IMP(self, token):
        if token in ("+", "-"):
            self._state = self._GLOBAL
        else:
            CTokenTranslator._DEC_TO_IMP(self, token)
            if self._state == self._GLOBAL:
                self._state = self._OBJC_DEC_BEGIN
                return UniversalCodeCounter.START_NEW_FUNCTION, (token, self._current_line)
    def _OBJC_DEC_BEGIN(self, token):
        if token == ':':
            self._state = self._OBJC_DEC
            return UniversalCodeCounter.ADD_TO_FUNCTION_NAME, token
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._GLOBAL
    def _OBJC_DEC(self, token):
        if token == '(':
            self._state = self._OBJC_PARAM_TYPE
            return UniversalCodeCounter.ADD_TO_LONG_FUNCTION_NAME, token
        elif token == ',':
            pass
        elif token == '{':
            self.br_count += 1
            self._state = self._IMP
        else:
            self._state = self._OBJC_DEC_BEGIN
            return UniversalCodeCounter.ADD_TO_FUNCTION_NAME, " " + token
        
    def _OBJC_PARAM_TYPE(self, token):
        if token == ')':
            self._state = self._OBJC_PARAM
        return UniversalCodeCounter.ADD_TO_LONG_FUNCTION_NAME, " " + token
    def _OBJC_PARAM(self, token):
        self._state = self._OBJC_DEC

class SDLTokenTranslator(TokenTranslatorBase):
    def __init__(self, token):
        TokenTranslatorBase.__init__(self, token)
        self.last_token = ""
        self.prefix = ""
        self.statename = ""
        self.start_of_statement = True
        self.saved_process = ""
    def _GLOBAL(self, token):
            if token == 'PROCEDURE':
                self._state = self._DEC
            elif token == 'PROCESS':
                self._state = self._PROCESS
            elif token == 'STATE': 
                self._state = self._STATE
            elif token == 'START': 
                self.prefix = self.saved_process
                self._state = self._IMP
                return UniversalCodeCounter.START_NEW_FUNCTION, (self.prefix, self._current_line)
    def _DEC(self, token):
            self.prefix = "PROCEDURE " + token
            self._state = self._IMP
            return UniversalCodeCounter.START_NEW_FUNCTION, (self.prefix, self._current_line)
    def _PROCESS(self, token):
        self.prefix = "PROCESS " + token
        self.saved_process = self.prefix
        self._state = self._IMP
        return UniversalCodeCounter.START_NEW_FUNCTION, (self.prefix, self._current_line)
    def _STATE(self, token):
        self.statename = token
        self._state = self._BETWEEN_STATE_AND_INPUT
    def _BETWEEN_STATE_AND_INPUT(self, token):
        if token == 'INPUT':
            self._state = self._INPUT
    def _INPUT(self, token):
        if token != 'INTERNAL':
            self._state = self._IMP
            return UniversalCodeCounter.START_NEW_FUNCTION, (self.prefix + " STATE " + self.statename + " INPUT " + token, self._current_line)
    def _IMP(self, token):
        if token == 'PROCEDURE':
            self._state = self._DEC
            return False
        if token == 'ENDPROCEDURE' or token == 'ENDPROCESS' or token == 'ENDSTATE':
            self._state = self._GLOBAL
            return UniversalCodeCounter.END_OF_FUNCTION, ""
        if self.start_of_statement:     
            if token == 'STATE': 
                self._state = self._STATE
                return UniversalCodeCounter.END_OF_FUNCTION, ""     
            elif token == 'INPUT': 
                self._state = self._INPUT
                return UniversalCodeCounter.END_OF_FUNCTION, ""     
        condition = self._is_condition(token, self.last_token)

        self.last_token = token
        if not token.startswith("#"):
            self.start_of_statement = (token == ';')
        if condition:
            return UniversalCodeCounter.CONDITION, token
        return UniversalCodeCounter.TOKEN, token
        
    conditions = set(['WHILE', 'AND', 'OR', '#if'])
    def _is_condition(self, token, last_token):
        if token == ':' and last_token == ')':
            return UniversalCodeCounter.END_OF_FUNCTION, ""
        return token in self.conditions

import re

c_pattern = re.compile(r".*\.(c|C|cpp|CPP|CC|cc|mm)$")
sdl_pattern = re.compile(r".*\.(sdl|SDL)$")
objc_pattern = re.compile(r".*\.(m)$")

hfcca_language_infos = {
                 'c/c++': {
                  'name_pattern': c_pattern,
                  'creator':CTokenTranslator},
                    
                 'sdl' : {
                  'name_pattern': sdl_pattern,
                  'creator':SDLTokenTranslator},
                  
                  'objC' : {
                  'name_pattern': objc_pattern,
                  'creator':ObjCTokenTranslator}
            }

def get_parser_by_file_name(filename):
        for lan in hfcca_language_infos:
            info = hfcca_language_infos[lan]
            if info['name_pattern'].match(filename):
                return info['creator']
class FileAnalyzer:
    def __init__(self, use_preprocessor=False):
        self.use_preprocessor = use_preprocessor
        self.open = open
    def __call__(self, filename):
        return self.analyze(filename)
    def analyze(self, filename):
        f = self.open(filename)
        code = f.read()
        f.close()
        parser = get_parser_by_file_name(filename)
        if parser:
            preprocessor = DefaultPreprocessor
            if self.use_preprocessor:
                preprocessor = CPreprocessor
            return self.analyze_source_code_with_parser(code, preprocessor, filename, parser)
       
    def analyze_source_code_with_parser(self, code, preprocessor, filename, parser):
        return UniversalCodeCounter(parser(preprocessor().process(generate_tokens(code))), filename).countCode()

token_pattern = re.compile(r"(\w+|/\*|//|:=|::|>=|\*=|>|#\s*define|#\s*if|#\s*else|#\s*endif|#\s*\w+|[!%^&\*\-=+\|\\<>/\]\+]+|.)", re.M | re.S)

def generate_tokens(source_code):
    for t, l in generate_tokens_from_code(source_code):
        if not t.startswith('#define') and not t.startswith('/*') and not t.startswith('//') :
            yield t, l
def generate_tokens_from_code(source_code):
    in_middle_of_empty_lines = False
    for (token, line) in generate_tokens_from_code_with_multiple_newlines(source_code):
        if token != '\n' or not in_middle_of_empty_lines:
            yield token, line
        in_middle_of_empty_lines = (token == '\n')
            
def generate_tokens_from_code_with_multiple_newlines(source_code):
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
        elif token == '//' or token == '#if' or token == '#endif':
            index = source_code.find('\n', index)  
            if index == -1:
                break
        elif token == '"' or token == '\'':
            while(1):
                index += 1
                index = source_code.find(token, index)  
                if index == -1:
                    break
                if source_code[index - 1] == '\\' and source_code[index - 2] != '\\':
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
         

import os

def match_patterns(str_to_match, patterns):
    for p in patterns:
        if p.search(str_to_match):
            return True
    return False
    
def iterate_files(SRC_DIRs, exclude_patterns):
    for SRC_DIR in SRC_DIRs:
        if os.path.isfile(SRC_DIR):
            yield SRC_DIR
        else:
            for root, dirs, files in os.walk(SRC_DIR, topdown=False):
                for filename in files:
                    if get_parser_by_file_name(filename):
                        if match_patterns(filename, exclude_patterns):
                            continue
                        full_path_name = os.path.join(root, filename)
                        if match_patterns(full_path_name, exclude_patterns):
                            continue
                        yield full_path_name

import sys

def print_function_info(fun, filename, option):
    name = fun.name
    if option.verbose:
        name = fun.long_name()
    print("%6d%6d%6d%6d %s@%s@%s" % (fun.NLOC, fun.cyclomatic_complexity, fun.token_count, fun.parameter_count, name, fun.start_line, filename))

def print_warnings(option, saved_result):
    warning_count = 0
    print() 
    print("!!!! Warnings (CCN > %d) !!!!" % option.CCN)
    print("==============================================================")
    print("NLOC    CCN   token   param            function@file")
    print("--------------------------------------------------------------")
    cnt = 0
    for f in saved_result:
        for fun in f:
            cnt += 1
            if fun.cyclomatic_complexity > option.CCN or fun.parameter_count > option.arguments:
                warning_count += 1
                print_function_info(fun, f.filename, option)
    
    return warning_count

def print_total(warning_count, saved_result, option):
    all_fun = list(itertools.chain(*saved_result))
    cnt = len(all_fun)
    if (cnt == 0):
        cnt = 1
    tNLOC = sum([f.NLOC for f in all_fun])
    if (tNLOC == 0):
        tNLOC = 1
    total_info = (
                  tNLOC, tNLOC / cnt,
                  float(sum([f.cyclomatic_complexity for f in all_fun])) / cnt,
                  float(sum([f.token_count for f in all_fun])) / cnt,
                  cnt,
                  warning_count,
                  float(warning_count) / cnt,
                  float(sum([f.NLOC for f in all_fun if f.cyclomatic_complexity > option.CCN])) / tNLOC
                  )


    print("=================================================================================")
    print("Total NLOC  Avg.NLOC  Avg CCN  Avg token  Fun Cnt  Warning cnt   Fun Rt   NLOC Rt  ")
    print("--------------------------------------------------------------------------------")
    print("%10d%10d%9.2f%11.2f%9d%13d%10.2f%8.2f" % total_info)

def print_and_save_detail_information(r, option):
    saved_result = []
    if (option.warnings_only):
        saved_result = r
    else:
        print("==============================================================")
        print("NLOC    CCN   token   param           function@line@file")
        print("--------------------------------------------------------------")
        for f in r:
            saved_result.append(f)
            for fun in f:
                print_function_info(fun, f.filename, option)
        
        print("--------------------------------------------------------------")
        print("%d file analyzed." % (len(saved_result)))
        print("==============================================================")
        print("LOC    Avg.NLOC AvgCCN Avg.ttoken  function_cnt    file")
        print("--------------------------------------------------------------")
        for f in saved_result:
            print("%7d%7d%7d%10d%10d     %s" % (f.LOC, f.average_NLOC, f.average_CCN, f.average_token, len(f), f.filename))
    
    return saved_result

def print_result(r, option):
    saved_result = print_and_save_detail_information(r, option)
    warning_count = print_warnings(option, saved_result)
    print_total(warning_count, saved_result, option)
    if option.number > warning_count:
        sys.exit(1)

def xml_output(result, options):
    import xml.dom.minidom

    impl = xml.dom.minidom.getDOMImplementation()
    doc = impl.createDocument(None, "cppncss", None)
    root = doc.documentElement

    measure = doc.createElement("measure")
    measure.setAttribute("type", "Function")
    labels = doc.createElement("labels")
    label1 = doc.createElement("label")
    text1 = doc.createTextNode("Nr.")
    label1.appendChild(text1)
    label2 = doc.createElement("label")
    text2 = doc.createTextNode("NCSS")
    label2.appendChild(text2)
    label3 = doc.createElement("label")
    text3 = doc.createTextNode("CCN")
    label3.appendChild(text3)
    labels.appendChild(label1)
    labels.appendChild(label2)
    labels.appendChild(label3)
    measure.appendChild(labels)
    
    Nr = 0
    total_func_ncss = 0
    total_func_ccn = 0
    
    for source_file in result:
        file_name = source_file.filename
        for func in source_file:
            Nr += 1
            name = func.name
            if options.verbose:
                name = func.long_name()
            item = doc.createElement("item")
            item.setAttribute("name", "%s(...) at %s:%s" % (name, file_name, func.start_line))
            value1 = doc.createElement("value")
            text1 = doc.createTextNode(str(Nr))
            value1.appendChild(text1)
            item.appendChild(value1)
            value2 = doc.createElement("value")
            text2 = doc.createTextNode(str(func.NLOC))
            value2.appendChild(text2)
            item.appendChild(value2)
            value3 = doc.createElement("value")
            text3 = doc.createTextNode(str(func.cyclomatic_complexity))
            value3.appendChild(text3)
            item.appendChild(value3)
            measure.appendChild(item)
            total_func_ncss += func.NLOC
            total_func_ccn += func.cyclomatic_complexity
        
        if Nr != 0:
            average_ncss = doc.createElement("average")
            average_ncss.setAttribute("lable", "NCSS")
            average_ncss.setAttribute("value", str(total_func_ncss / Nr))
            measure.appendChild(average_ncss)
            
            average_ccn = doc.createElement("average")
            average_ccn.setAttribute("lable", "CCN")
            average_ccn.setAttribute("value", str(total_func_ccn / Nr))
            measure.appendChild(average_ccn)
    
    root.appendChild(measure)

    measure = doc.createElement("measure")
    measure.setAttribute("type", "File")
    labels = doc.createElement("labels")
    label1 = doc.createElement("label")
    text1 = doc.createTextNode("Nr.")
    label1.appendChild(text1)
    label2 = doc.createElement("label")
    text2 = doc.createTextNode("NCSS")
    label2.appendChild(text2)
    label3 = doc.createElement("label")
    text3 = doc.createTextNode("CCN")
    label3.appendChild(text3)
    label4 = doc.createElement("label")
    text4 = doc.createTextNode("Functions")
    label4.appendChild(text4)
    labels.appendChild(label1)
    labels.appendChild(label2)
    labels.appendChild(label3)
    labels.appendChild(label4)
    measure.appendChild(labels)
    
    file_NR = 0
    file_total_ncss = 0
    file_total_ccn = 0
    file_total_funcs = 0
    
    for source_file in result:
        file_NR += 1
        item = doc.createElement("item")
        item.setAttribute("name", source_file.filename)
        value1 = doc.createElement("value")
        text1 = doc.createTextNode(str(file_NR))
        value1.appendChild(text1)
        item.appendChild(value1)
        value2 = doc.createElement("value")
        text2 = doc.createTextNode(str(source_file.NLOC))
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
        measure.appendChild(item)
        file_total_ncss += source_file.NLOC
        file_total_ccn += source_file.CCN
        file_total_funcs += len(source_file)
    
    if file_NR != 0:
            average_ncss = doc.createElement("average")
            average_ncss.setAttribute("lable", "NCSS")
            average_ncss.setAttribute("value", str(file_total_ncss / file_NR))
            measure.appendChild(average_ncss)
            
            average_ccn = doc.createElement("average")
            average_ccn.setAttribute("lable", "CCN")
            average_ccn.setAttribute("value", str(file_total_ccn / file_NR))
            measure.appendChild(average_ccn)
            
            average_funcs = doc.createElement("average")
            average_funcs.setAttribute("lable", "Functions")
            average_funcs.setAttribute("value", str(file_total_funcs / file_NR))
            measure.appendChild(average_funcs)
            
    sum_ncss = doc.createElement("sum")
    sum_ncss.setAttribute("lable", "NCSS")
    sum_ncss.setAttribute("value", str(file_total_ncss))
    measure.appendChild(sum_ncss)
    sum_ccn = doc.createElement("sum")
    sum_ccn.setAttribute("lable", "CCN")
    sum_ccn.setAttribute("value", str(file_total_ccn))
    measure.appendChild(sum_ccn)
    sum_funcs = doc.createElement("sum")
    sum_funcs.setAttribute("lable", "Functions")
    sum_funcs.setAttribute("value", str(file_total_funcs))
    measure.appendChild(sum_funcs)
            
    root.appendChild(measure)
    
    xmlString = doc.toprettyxml()
    return xmlString

def remove_sharp_from_class(parser_class):
    parser_class.conditions.remove("#if")

def createHfccaCommandLineParser():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-v", "--verbose",
            help="Output in verbose mode (long function name)",
            action="store_true",
            dest="verbose",
            default=False)
    parser.add_option("-C", "--CCN",
            help="Threshold for cyclomatic complexity number warning. _functions with CCN bigger than this number will be shown in warning",
            action="store",
            type="int",
            dest="CCN",
            default=DEFAULT_CCN_THRESHOLD)
    parser.add_option("-w", "--warnings_only",
            help="Show warnings only",
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
            help="Exclude data files that match this regular expression. Multiple regular expressions can be specified.",
            action="append",
            dest="exclude",
            default=[])
    parser.add_option("-X", "--xml",
            help="Generate XML in cppncss style instead of the normal tabular output. Useful to generate report in Hudson server",
            action="store_true",
            dest="xml",
            default=None)
    parser.add_option("-p", "--preprocess",
            help="Use preprocessor, always ignore the #else branch. By default, source_analyzer just ignore any preprocessor statement.",
            action="store_true",
            dest="use_preprocessor",
            default=False)
    parser.add_option("-a", "--arguments",
            help="Limit for number of parameters",
            action="store",
            type="int",
            dest="arguments",
            default=100)
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
    
    parser.usage = "source_analyzer.py [options] [PATH or FILE] [PATH] ... "
    parser.description = __doc__
    
    return parser

from distutils.core import setup
def install():
    setup(name='hfcca',
          version='1.5',
          py_modules=['hfcca'],
          author='Terry Yin',
          author_email='terry.yinze@gmail.com',
          url='https://github.com/terryyin/hfcca',
          scripts=['hfcca.py']
          )

def mapFilesToAnalyzer(files, fileAnalyzer, working_threads):
    try:
        import multiprocessing
        it = multiprocessing.Pool(processes=working_threads)
        mapFun = it.imap_unordered
    except:
        mapFun = itertools.imap
    r = mapFun(fileAnalyzer, files)
    return r

def main(argv):
    parser = createHfccaCommandLineParser()
    (options, args) = parser.parse_args(args=sys.argv)
    
    if len(args) == 1:
        paths = ["."]
    else:
        if args[1] == "install":
            install()
            return
        paths = args[1:]
    
    if options.no_preprocessor_count:
        remove_sharp_from_class(CTokenTranslator)
        remove_sharp_from_class(SDLTokenTranslator)
    
    exclude_patterns = [re.compile(p) for p in options.exclude]
    files = iterate_files(paths, exclude_patterns)
    fileAnalyzer = FileAnalyzer(options.use_preprocessor)
    r = mapFilesToAnalyzer(files, fileAnalyzer, options.working_threads)
    if options.xml:
        print(xml_output([f for f in r], options))
    else:
        print_result(r, options)
if __name__ == "__main__":
    main(sys.argv[1:])

#
# Unit Test
#
import unittest
class Test_generate_tonken(unittest.TestCase):
    def test_empty_string(self):
        result = [t for t in generate_tokens_from_code("")]
        self.assertEqual(0, len(result))
    def test_with_one_return(self):
        result = [t for t in generate_tokens_from_code("\n")]
        self.assertEqual(1, len(result))
    def test_with_two_returns(self):
        result = [t for t in generate_tokens_from_code("\n\n")]
        self.assertEqual(1, len(result))

class Test_c_cpp_hfcca(unittest.TestCase):
    def create_c_hfcca(self, source_code, preprocessor=DefaultPreprocessor):
        return FileAnalyzer().analyze_source_code_with_parser(source_code, preprocessor, "", CTokenTranslator)
    def test_empty(self):
        result = self.create_c_hfcca("")
        self.assertEqual(0, len(result))
    def test_no_function(self):
        result = self.create_c_hfcca("#include <stdio.h>\n")
        self.assertEqual(0, len(result))
    def test_one_function(self):
        result = self.create_c_hfcca("int fun(){}")
        self.assertEqual(1, len(result))
        self.assertTrue("fun" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
    def test_two_function(self):
        result = self.create_c_hfcca("int fun(){}\nint fun1(){}")
        self.assertEqual(2, result.LOC)
        self.assertEqual(2, len(result))
        self.assertTrue("fun" in result)
        self.assertTrue("fun1" in result)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(2, result[1].start_line)
    def test_function_with_content(self):
        result = self.create_c_hfcca("int fun(xx oo){int a; a= call(p1,p2);}")
        self.assertEqual(1, len(result))
        self.assertTrue("fun" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual("fun( xx oo )", result[0].long_name())
    def test_one_function_with_content(self):
        result = self.create_c_hfcca("int fun(){if(a){xx;}}")
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].NLOC)
        self.assertEqual(3, result[0].token_count)
    def test_nloc(self):
        result = self.create_c_hfcca("int fun(){\n\n\n}")
        self.assertEqual(2, result[0].NLOC)
    def test_nloc2(self):
        result = self.create_c_hfcca("int fun(){aa();\n\n\n\nbb();\n\n\n}")
        self.assertEqual(3, result[0].NLOC)
    def test_one_function_with_question_mark(self):
        result = self.create_c_hfcca("int fun(){return (a)?b:c;}")
        self.assertEqual(2, result[0].cyclomatic_complexity)
    def test_one_function_with_forever_loop(self):
        result = self.create_c_hfcca("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
    def test_one_function_with_and(self):
        result = self.create_c_hfcca("int fun(){if(a&&b){xx;}}")
        self.assertEqual(3, result[0].cyclomatic_complexity)
    def test_double_slash_within_string(self):
        result = self.create_c_hfcca("""int fun(){char *a="\\\\";}""")
        self.assertEqual(1, result[0].cyclomatic_complexity)
    def test_example1(self):
        result = self.create_c_hfcca(example1)
        self.assertEqual(3, result[0].cyclomatic_complexity)
    def test_example_macro(self):
        result = self.create_c_hfcca(example_macro)
        self.assertEqual(0, len(result))
    def test_function_with_no_param(self):
        result = self.create_c_hfcca("int fun(){}")
        self.assertEqual(0, result[0].parameter_count)
    def test_function_with_1_param(self):
        result = self.create_c_hfcca("int fun(aa * bb){}")
        self.assertEqual(1, result[0].parameter_count)
    def test_function_with_param(self):
        result = self.create_c_hfcca("int fun(aa * bb, cc dd){}")
        self.assertEqual(2, result[0].parameter_count)
    def test_function_with_strang_param(self):
        result = self.create_c_hfcca("int fun(aa<mm, nn> bb){}")
        self.assertEqual(1, result[0].parameter_count)
    def test_one_function1(self):
        result = self.create_c_hfcca("int abc::fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( )", result[0].long_name())
        self.assertEqual(1, result[0].cyclomatic_complexity)
    def test_one_function_with_const(self):
        result = self.create_c_hfcca("int abc::fun()const{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( ) const", result[0].long_name())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_in_class(self):
        result = self.create_c_hfcca("class c {~c(){}}; int d(){}")
        self.assertEqual(2, len(result))
        self.assertTrue("c" in result)
        self.assertTrue("d" in result)
        
    def test_1(self):
        result = self.create_c_hfcca("abc::def(a<b>& c){}")
        self.assertEqual(1, len(result))
    def test_2(self):
        result = self.create_c_hfcca("abc::def(a<b*> c){}")
        self.assertEqual(1, len(result))
        
        

class Test_sdl_hfcca(unittest.TestCase):
    def create_sdl_hfcca(self, source_code):
        return UniversalCodeCounter(SDLTokenTranslator(generate_tokens(source_code)) , "").countCode()
    def test_empty(self):
        result = self.create_sdl_hfcca("")
        self.assertEqual(0, len(result))
    def test_process(self):
        result = self.create_sdl_hfcca("PROCESS pofcap\n ENDPROCESS;")
        self.assertEqual(1, len(result))
        self.assertTrue('PROCESS pofcap' in result)
    def test_one_function(self):
        result = self.create_sdl_hfcca("PROCEDURE xxx\n ENDPROCEDURE;");
        self.assertEqual(1, len(result))
        self.assertTrue("PROCEDURE xxx" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(0, result[0].token_count)
    def test_one_function_with_condition(self):
        result = self.create_sdl_hfcca(example_sdl_procedure);
        self.assertEqual(1, len(result))
        self.assertTrue("PROCEDURE send_swo_msgs__r" in result)
        self.assertEqual(7, result[0].cyclomatic_complexity)
        self.assertEqual(173, result[0].token_count)
    def test_one_function_with_array(self):
        result = self.create_sdl_hfcca("""
        PROCEDURE send_swo_msgs__r;
        START;
            TASK array(0):= 1;
        ENDPROCEDURE;
        """);
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)
    def test_process_with_content(self):
        result = self.create_sdl_hfcca(example_sdl_process);
        self.assertEqual(5, len(result))
        self.assertTrue("PROCEDURE send_swo_msgs__r" in result)
        self.assertTrue("PROCESS pofsrt" in result)
        self.assertTrue("PROCESS pofsrt STATE start_state INPUT supervision_msg_s" in result)
        self.assertTrue("PROCESS pofsrt STATE start_state1 INPUT supervision_msg_s2" in result)
        self.assertEqual(2, result[1].cyclomatic_complexity)

class Test_objc_hfcca(unittest.TestCase):
    def create_objc_hfcca(self, source_code):
        return UniversalCodeCounter(ObjCTokenTranslator(generate_tokens(source_code)) , "").countCode()
    def test_empty(self):
        result = self.create_objc_hfcca("")
        self.assertEqual(0, len(result))
    def test_no_function(self):
        result = self.create_objc_hfcca("#import <unistd.h>\n")
        self.assertEqual(0, len(result))
    def test_one_c_function(self):
        result = self.create_objc_hfcca("int fun(int a, int b) const{}")
        self.assertTrue("fun" in result)
    def test_one_objc_function(self):
        result = self.create_objc_hfcca("-(void) foo {}")
        self.assertTrue("foo" in result)
    def test_one_objc_function_with_param(self):
        result = self.create_objc_hfcca("-(void) replaceScene: (CCScene*) scene {}")
        self.assertEqual("replaceScene:", result[0].name)
        self.assertEqual("replaceScene:( CCScene * )", result[0].long_name())
    def test_one_objc_functio_nwith_two_param(self):
        result = self.create_objc_hfcca("- (BOOL)scanJSONObject:(id *)outObject error:(NSError **)outError {}")
        self.assertTrue("scanJSONObject: error:" in result)
        self.assertEqual("scanJSONObject:( id * ) error:( NSError ** )", result[0].long_name())
    def test_one_objc_function_with_three_param(self):
        result = self.create_objc_hfcca("- (id)initWithRequest:(NSURLRequest *)request delegate:(id <NSURLConnectionDelegate>)delegate startImmediately:(BOOL)startImmediately{}")
        self.assertEqual("initWithRequest: delegate: startImmediately:", result[0].name)
        self.assertEqual("initWithRequest:( NSURLRequest * ) delegate:( id < NSURLConnectionDelegate > ) startImmediately:( BOOL )", result[0].long_name())
    def test_implementation(self):
        code = """
            @implementation classname(xx)
            + (return_type)classMethod
            {
                if (failure){

                     //wefailed

                 }
            }
            - (return_type)instanceMethod
            {
                // implementation
            }
            @end
            """
        result = self.create_objc_hfcca(code)
        self.assertEqual(2, len(result))
        self.assertEqual("classMethod", result[0].name)
        


class Test_parser_token(unittest.TestCase):
    def get_tokens(self, source_code):
        return [x for x, l in generate_tokens(source_code)]
    def get_tokens_and_line(self, source_code):
        return [x for x in generate_tokens(source_code)]
    def test_empty(self):
        tokens = self.get_tokens("")
        self.assertEqual(0, len(tokens))
    def test_one_digit(self):
        tokens = self.get_tokens("1")
        self.assertEqual(['1'], tokens)
    def test_operators(self):
        tokens = self.get_tokens("-;")
        self.assertEqual(['-', ';'], tokens)
    def test_operators1(self):
        tokens = self.get_tokens("-=")
        self.assertEqual(['-='], tokens)
    def test_operators2(self):
        tokens = self.get_tokens(">=")
        self.assertEqual(['>='], tokens)

    def test_more(self):
        tokens = self.get_tokens("int a{}")
        self.assertEqual(['int', "a", "{", "}"], tokens)
    def test_or(self):
        tokens = self.get_tokens("||")
        self.assertEqual(['||'], tokens)
    def test_comment(self):
        tokens = self.get_tokens("/***\n**/")
        self.assertEqual([], tokens)
        tokens = self.get_tokens("//aaa\n")
        self.assertEqual(['\n'], tokens)
    def test_commentedComment(self):
        tokens = self.get_tokens(" /*/*/")
        self.assertEqual([], tokens)
    def test_string(self):
        tokens = self.get_tokens(r'""')
        self.assertEqual(['""'], tokens)
        tokens = self.get_tokens(r'"x\"xx")')
        self.assertEqual(['"x\\"xx"', ')'], tokens)
    def test_define(self):
        tokens = self.get_tokens('''#define xx()\
                                      abc
                                    int''')
        self.assertEqual(['\n', 'int'], tokens)
       
    def test_line_number(self):
        tokens = self.get_tokens_and_line(r'abc')
        self.assertEqual(('abc', 1), tokens[0])
    def test_line_number2(self):
        tokens = self.get_tokens_and_line('abc\ndef')
        self.assertTrue(('def', 2) in tokens)
    def test_with_mutiple_line_string(self):
        tokens = self.get_tokens_and_line('"sss\nsss" t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_cpp_comments(self):
        tokens = self.get_tokens_and_line('//abc\n t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_line_continuer_comments(self):
        tokens = self.get_tokens_and_line('#define a \\\nb\n t')
        self.assertTrue(('t', 3) in tokens)
    def test_define2(self):
        tokens = self.get_tokens_and_line(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
''')
        self.assertTrue(('t', 3) in tokens)
    def test_with_c_comments(self):
        tokens = self.get_tokens_and_line('/*abc\n*/ t')
        self.assertTrue(('t', 2) in tokens)

class MockFileAnalyzer(FileAnalyzer):
    def __init__(self):
        self.mockRecord = []
    def analyze(self, filename):
        return filename
class Test_analyze_files(unittest.TestCase):
    def test_NoFiles(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 1)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_NoFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 2)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_OneFile(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 1)
        self.assertEqual(["filename"], [x for x in r])
    def test_OneFileMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 2)
        self.assertEqual(["filename"], [x for x in r])
    def test_MoreFiles(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 1)
        self.assertEqual(["f1", "f2"], [x for x in r])
    def test_MoreFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 2)
        self.assertEqual(["f1", "f2"], [x for x in r])

class MockFile:
    def __init__(self):
        self.mockRecord = []
    def read(self):
        return "int foo(){}"
    def close(self):
        pass
def mockOpen(name):
    return MockFile()
class Test_FileAnalyzer(unittest.TestCase):
    def setUp(self):
        self.analyzer = FileAnalyzer()
        self.analyzer.open = mockOpen
    def create_c_hfcca(self, source_code, preprocessor=DefaultPreprocessor):
        return FileAnalyzer().analyze_source_code_with_parser(source_code, preprocessor, "", CTokenTranslator)
    def test_analyze_c_file(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        self.assertEqual(1, len([x for x in r]))
    def test_analyze_c_file_with_multiple_thread(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 2)
        self.assertEqual(1, len([x for x in r]))
class Test_FunctionInfo(unittest.TestCase):
    def test_FunctionInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(FunctionInfo("a", 1))

example1 = r'''
int startup(u_short *port)
{
 if (*port == 0)  /* if dynamically allocating a port */
 {
  socklen_t namelen = sizeof(name);
#ifdef abc
  *port = ntohs(name.sin_port);
#endif
 }
 return(httpd);
}
'''

example_macro = r'''
#define MTP_CHECK                                                             \
   if (mea_data->which_data != MTP_WHICH_DATA_T_NONE_C) {                     \
   phys_address_t np;                                                         \
   }
'''

example_sdl_procedure = '''
/**************************************************************************/
PROCEDURE send_swo_msgs__r;
/*
 * Send the given switchover message to POFFIC in all computers in the target list.
 **************************************************************************/
FPAR
    IN/OUT  targets  targets__t,
    IN      msg_num  message_number_t;

DCL
    i     dword := 0,
    c_i   dword,
    msg_group message_group_t,
    activity_signal byte := msg_attr_t_normal_priority_c,
    ppid  pid;

START;
    DECISION routing_state__pv;
    ( routing_state_t_active_c ):
       TASK activity_signal := msg_attr_t_is_active__c;
    ENDDECISION;

    TASK  ppid := SELF;
    WHILE i < targets.item_count;
       TASK  set_pid_computer_r( ppid, targets.target(i).addr );
       TASK  c_i := 0,
             msg_group := direct_delivery_gi;

       WHILE c_i < 2;
          DECISION targets.target(i).chan(c_i);
          ( T ):
             DECISION msg_num;
             ( NUMBER_FROM( pof_deny_messages_s )):
                OUTPUT pof_deny_messages_s TO ppid,
                       SET GROUP = msg_group;
             ( NUMBER_FROM( pof_allow_messages_s )):
                OUTPUT pof_allow_messages_s TO ppid,
                       SET GROUP = msg_group, PRIORITY = activity_signal;
             ENDDECISION;
          ENDDECISION;
          TASK c_i := c_i + 1,
               msg_group := rt_direct_delivery_gi;
       ENDWHILE;

       TASK i := i + 1;
    ENDWHILE;
ENDPROCEDURE send_swo_msgs__r;
'''

example_sdl_process = r'''
PROCESS pofsrt
  COMMENT '@(#)SID: POFSRTGX.SDL 2.1-0 06/07/11';
/*
 */

DCL
  addr_range addr_range_t;

PROCEDURE send_swo_msgs__r;
START;
    TASK  ppid := SELF;
ENDPROCEDURE send_swo_msgs__r;
START;
  /* announce to DMXRTE */
  DECISION post_office_announcement_r( post_district_index_t_atm_c,
           addr_range,
           pof_advisable_msg_len__c - sizeof(buffer_bottom_t),
           pof_ack_waiting_time__c  );
    ( success_ec ):
      TASK /* nop */;
    ELSE:
      TASK /* hanskat tiskiin */;
  ENDDECISION;
  NEXTSTATE start_state
    COMMENT 'Nuthin fancy here';

/******************************************************/
STATE start_state
  COMMENT 'Wait for the first supervision message';

  INPUT supervision_msg_s(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
ENDSTATE start_state
STATE start_state1
  COMMENT 'Wait for the first supervision message';

#if (tr)
  INPUT supervision_msg_s1(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
#endif
  INPUT INTERNAL supervision_msg_s2(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
ENDSTATE start_state
  COMMENT 'Hand prefix started';
ENDPROCESS pofsrt;
'''

