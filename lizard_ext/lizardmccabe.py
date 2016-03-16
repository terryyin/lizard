'''
This is an extension of lizard. It rectifies the cyclomatic complexity
counted by lizard according to McCabe's definition:
    http://www.mccabe.com/pdf/mccabe-nist235r.pdf

It also rectifies the nesting depth counted by lizard according
to codebetter's definition available on their website:
http://codebetter.com/patricksmacchia/2008/03/07/a-simple-trick-to-code-better-and-to-increase-testability/
In McCabe's version and nesting depth metric, the fall-through cases in
switch/case statement doesn't count as 1 complexity and nested depth
due to its readability.
'''
from .extension_base import ExtensionBase


class LizardExtension(ExtensionBase):  # pylint: disable=R0903

    def _state_global(self, token):
        if token == "case":
            self._state = self._in_case

    def _in_case(self, token):
        if token == ":":
            self._state = self._after_a_case

    def _after_a_case(self, token):
        if token == "case":
            self.context.add_condition(-1)
            if hasattr(self.context, "add_nd_condition"):
                self.context.add_nd_condition(-1)
            self.next(self._in_case)
        else:
            self.next(self._state_global)

    def __init__(self):
        super(LizardExtension, self).__init__(None)
