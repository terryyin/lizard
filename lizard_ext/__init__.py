''' extensions of lizard '''

from __future__ import print_function
from .htmloutput import html_output
from .xmloutput import xml_output
from .auto_open import auto_open


def print_xml(results, options, _):
    print(xml_output(list(results), options.verbose))
    return 0
