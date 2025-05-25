""" extensions of lizard """

from __future__ import print_function
from .version import version
from .htmloutput import html_output
from .csvoutput import csv_output
from .xmloutput import xml_output
from .auto_open import auto_open, auto_read
from .checkstyleoutput import checkstyle_output


def print_xml(results, options, _, total_factory):
    print(xml_output(total_factory(list(results)), options.verbose))
    return 0


def print_csv(results, options, _, total_factory):
    csv_output(total_factory(list(results)), options)
    return 0


def print_checkstyle(results, options, _, total_factory, file=None):
    import sys
    print("DEBUG: print_checkstyle called", file=sys.stderr)
    output = checkstyle_output(total_factory(list(results)), options.verbose)
    if file is None:
        file = sys.stdout
    file.write(output)
    if not output.endswith("\n"):
        file.write("\n")
    file.flush()
    return 0
