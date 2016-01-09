''' Language parser for Python '''

from lizard import CodeReader
from lizard import CLikeReader


class ObjCReader(CLikeReader, CodeReader):

    ext = ['m']
    language_names = ['objectivec', 'objective-c', 'objc']

    def __init__(self, context):
        super(ObjCReader, self).__init__(context)

    def fake_and_useless(self):
        pass

    def useless_and_fake(self):
        pass

    def _state_global(self, token):
        super(ObjCReader, self)._state_global(token)
        if token == '(':
            self.next(self._state_dec, token)

    def _state_dec_to_imp(self, token):
        if token in ("+", "-"):
            self._state = self._state_global
        else:
            super(ObjCReader, self)._state_dec_to_imp(token)
            if self._state != self._state_imp:
                self._state = self._state_objc_dec_begin
                self.context.start_new_function(token)

    def _state_objc_dec_begin(self, token):
        if token == ':':
            self._state = self._state_objc_dec
            self.context.add_to_function_name(token)
        elif token == '{':
            self.next(self._state_imp, "{")
        else:
            self._state = self._state_global

    def _state_objc_dec(self, token):
        if token == '(':
            self._state = self._state_objc_param_type
            self.context.add_to_long_function_name(token)
        elif token == ',':
            pass
        elif token == '{':
            self.next(self._state_imp, "{")
        else:
            self._state = self._state_objc_dec_begin
            self.context.add_to_function_name(" " + token)

    def _state_objc_param_type(self, token):
        if token == ')':
            self._state = self._state_objc_param
        self.context.add_to_long_function_name(" " + token)

    def _state_objc_param(self, _):
        self._state = self._state_objc_dec
