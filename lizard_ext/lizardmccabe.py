'''
This is an extension of lizard. It rectifies the cyclomatic complexity
counted by lizard according to McCabe's definition:
    http://www.mccabe.com/pdf/mccabe-nist235r.pdf

In McCabe's version, the fall-through cases in switch/case statement
doesn't count as 1 complexity.
'''
from lizard_languages.code_reader import CodeStateMachine


class LizardExtension(CodeStateMachine):  # pylint: disable=R0903

    def _state_global(self, token):
        if token == "case":
            self._state = self._in_case

    def _in_case(self, token):
        if token == ":":
            self._state = self._after_a_case

    def _after_a_case(self, token):
        if token == "case":
            self.context.add_condition(-1)
            self.next(self._in_case)
        else:
            self.next(self._state_global)

    def __init__(self):
        super(LizardExtension, self).__init__(None)
