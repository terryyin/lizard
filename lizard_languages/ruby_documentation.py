"""Recognize Ruby documentation and data sections at column zero."""

import re


DOCUMENTATION_BODY = (
    r"(?:=begin(?=\s|\Z).*?(?:^=end(?=\s|\Z)[^\n]*|\Z)"
    r"|__END__(?:\r?\n|\Z).*\Z)"
)
DOCUMENTATION = re.compile(r"^" + DOCUMENTATION_BODY, re.M | re.S)


def may_have_documentation(source):
    return '=begin' in source or '__END__' in source


def pattern_for_fragment(source, at_file_start):
    # Interpolation may restart in the middle of a line. Only a marker at
    # the start of that fragment needs the stricter newline lookbehind.
    if not at_file_start and source.startswith(('=begin', '__END__')):
        return r"(?<=\n)" + DOCUMENTATION_BODY
    return DOCUMENTATION.pattern


def match_opaque_span(source, start, percent_literal):
    # Recognize percent literals first so a column-zero marker in their
    # contents cannot start a documentation section in the regex scan.
    pattern = percent_literal if source[start] == '%' else DOCUMENTATION
    return pattern.match(source, start)
