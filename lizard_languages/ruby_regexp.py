"""Ruby slash literals that use the shared forward scan."""

import re

from .js_style_regex_context import REGEX_LITERAL


_EMPTY_SUFFIX = r"/[imxounes]*(?![\w/])"
_MULTILINE_SUFFIX = (
    r"(?![/*])(?=[^/\r\n]*\r?\n)"
    r"(?:\\[\s\S]|[^/\\[#]|#(?!\{)|\[(?:\\[\s\S]|[^\\\]#]|#(?!\{))*\])+"
    r"/[imounes]*x[imxounes]*(?!\w)"
)
REGEX_SUFFIX = r"(?:" + _EMPTY_SUFFIX + "|" + _MULTILINE_SUFFIX + ")"
LITERAL = re.compile(REGEX_LITERAL + r"|/" + REGEX_SUFFIX)
COMPLETE_INTERPOLATION = re.compile(r"#\{[^{}\/]*\}")
