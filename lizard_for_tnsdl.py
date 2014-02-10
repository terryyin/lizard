'''
lizard support for TNSDL. 
TNSDL is an in-house programming language of Nokia/Nokia Solutions and Networks.
'''
import re
from lizard import LanguageReaderBase

class SDLReader(LanguageReaderBase):
    def __init__(self):
        super(SDLReader, self).__init__()
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
                self.sourceCodeInfoBuilder.START_NEW_FUNCTION(self.prefix)
            
    def _DEC(self, token):
            self.prefix = "PROCEDURE " + token
            self._state = self._IMP
            self.sourceCodeInfoBuilder.START_NEW_FUNCTION(self.prefix)

    def _PROCESS(self, token):
        self.prefix = "PROCESS " + token
        self.saved_process = self.prefix
        self._state = self._IMP
        self.sourceCodeInfoBuilder.START_NEW_FUNCTION(self.prefix)

    def _STATE(self, token):
        self.statename = token
        self._state = self._BETWEEN_STATE_AND_INPUT
        
    def _BETWEEN_STATE_AND_INPUT(self, token):
        if token == 'INPUT':
            self._state = self._INPUT

    def _INPUT(self, token):
        if token != 'INTERNAL':
            self._state = self._IMP
            self.sourceCodeInfoBuilder.START_NEW_FUNCTION(self.prefix + " STATE " + self.statename + " INPUT " + token)

    def _IMP(self, token):
        if token == 'PROCEDURE':
            self._state = self._DEC
            return
        if token == 'ENDPROCEDURE' or token == 'ENDPROCESS' or token == 'ENDSTATE':
            self._state = self._GLOBAL
            self.sourceCodeInfoBuilder.END_OF_FUNCTION()
            return
        if self.start_of_statement:     
            if token == 'STATE': 
                self._state = self._STATE
                self.sourceCodeInfoBuilder.END_OF_FUNCTION()
                return 
            elif token == 'INPUT': 
                self._state = self._INPUT
                self.sourceCodeInfoBuilder.END_OF_FUNCTION()
                return
        condition = self._is_condition(token, self.last_token)

        self.last_token = token
        if not token.startswith("#"):
            self.start_of_statement = (token == ';')
        if condition:
            return self.sourceCodeInfoBuilder.CONDITION()
        else:
            return self.sourceCodeInfoBuilder.TOKEN()
        
    conditions = set(['WHILE', 'AND', 'OR', '#if'])
    def _is_condition(self, token, last_token):
        if token == ':' and last_token == ')':
            return True
        return token in self.conditions

sdl_pattern = re.compile(r".*\.(sdl|SDL)$")

extra_lizard_language_infos = {
                 'sdl' : {
                  'name_pattern': sdl_pattern,
                  'creator':SDLReader}
            }
