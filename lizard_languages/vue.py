'''
Language parser for Vue.js files
'''

import re
from itertools import repeat

from .js_style_regex_context import REGEX_TOKEN
from .javascript_templates import LINE_COMMENT, _template_end
from .typescript import TypeScriptReader
from .tsx import TSXReader

# The markup around the script blocks: a comment, an interpolation (up to
# the first '}}', as in Vue) or a tag. A quoted attribute value, or a
# Svelte one in braces, can hold a '>'; a '<' ends a tag.
_MARKUP = re.compile(
    r'<!--.*?(?:-->|\Z)'
    r'|\{\{.*?\}\}'
    r'|<(/?)([A-Za-z][^\s/<>]*)(?:=\s*(?:"[^"]*"|\'[^\']*\'|\{[^}]*\})|[^<>])*>?',
    re.S)
_ATTRIBUTE = re.compile(
    r'''([^\s"'<>/=]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]*)))?''')
_SCRIPT_END = re.compile(r'</script(?=[\s/>]|\Z)')
# In a script, a closing tag in a string, a comment or a template is text.
_SCRIPT_TEXT_OR_END = re.compile(
    r'/\*.*?\*/|' + LINE_COMMENT
    + r'''|"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|''' + REGEX_TOKEN
    + r'|`|' + _SCRIPT_END.pattern, re.S)
# Elements of a template that hold only text, as in an HTML parser.
_TEXT_ELEMENTS = frozenset(('script', 'style', 'textarea', 'title'))
# The blocks of reused source objects (warmed analyzer batches): the same
# code strings keep their memoized regex marks.
_BLOCK_CACHE = {}


def _attribute(tag, name):
    """The value of an attribute of a start tag, or None without it."""
    for match in _ATTRIBUTE.finditer(tag, 1):
        if match.group(1).lower() == name:
            return next((v for v in match.group(2, 3, 4) if v is not None), '')
    return None


class _ScriptTag(str):
    """The opening tag of a script block; it gives the lang of the code."""

    @property
    def lang(self):
        return (_attribute(self, 'lang') or 'js').lower()


def _script_blocks(source):
    """(tag start, tag, code, code end) of each top-level script block."""
    cached = _BLOCK_CACHE.get(id(source))
    if cached is not None and cached[0] is source:
        return cached[1]
    blocks = [
        (tag_start, _ScriptTag(source[tag_start:code_start]),
         source[code_start:code_end], code_end)
        for tag_start, code_start, code_end in _find_script_blocks(source)]
    _BLOCK_CACHE[id(source)] = (source, blocks)
    return blocks


def _find_script_blocks(source):
    """(tag start, code start, code end) of each top-level script block.

    As in the Vue compiler, a top-level HTML <template> holds nested markup,
    and the other top-level blocks hold text up to their closing tag."""
    pos = 0
    while True:
        match = _MARKUP.search(source, pos)
        if match is None:
            return
        pos = match.end()
        name = match.group(2)
        if name is None or match.group(1) or match.group().endswith('/>'):
            continue
        if name == 'template' and _attribute(
                match.group(), 'lang') in (None, 'html'):
            # Markup read wrongly can leave the template open; the scan then
            # goes on after its tag rather than losing the rest of the file.
            pos = _markup_end(source, pos) or pos
        elif name == 'script':
            end = _script_end(source, pos)
            yield match.start(), pos, end
            pos = end
        else:
            end = source.find('</' + name, pos)
            if end >= 0:
                pos = end


def _script_end(source, pos):
    """Where the code ends: at the first closing tag, as in Vue. When another
    closing tag follows before the next script, the first one was text in
    the code, and the first closing tag outside strings, comments and
    templates ends it."""
    end = _SCRIPT_END.search(source, pos)
    if end is None:
        return len(source)
    stray = _SCRIPT_END.search(source, end.end())
    if stray is None or source.find('<script', end.end(), stray.start()) >= 0:
        return end.start()
    while True:
        match = _SCRIPT_TEXT_OR_END.search(source, pos)
        if match is None:
            return end.start()
        token = match.group()
        if token.startswith('</'):
            return match.start()
        if token == '`':
            pos, expressions = _template_end(source, match.start())
            if expressions is None:
                return end.start()
        else:
            pos = match.end()


def _markup_end(source, pos):
    """The end of a <template>, or None when it does not close. Text
    elements and v-pre elements hold only text."""
    depth = 1
    while True:
        match = _MARKUP.search(source, pos)
        if match is None:
            return None
        pos = match.end()
        name, tag = match.group(2), match.group()
        if name is None or tag.endswith('/>'):
            continue
        if name == 'template':
            depth += -1 if match.group(1) else 1
            if depth == 0:
                return pos
        elif not match.group(1) and (
                name.lower() in _TEXT_ELEMENTS
                or ('v-pre' in tag and _attribute(tag, 'v-pre') is not None)):
            end = source.find('</' + name, pos)
            if end >= 0:
                pos = end


class VueReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue', 'vuejs']
    # A lang="tsx" block keeps whole templates, as in a .tsx file.
    _scan_atomic_templates = True
    script_tokens = staticmethod(TypeScriptReader.generate_tokens)

    def __init__(self, context):
        super(VueReader, self).__init__(context)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        """The code of the script blocks, tokenized by their lang. The
        markup keeps only its newlines, so the lines are those of the file."""
        pos = 0
        for tag_start, tag, code, code_end in _script_blocks(source_code):
            yield from repeat('\n', source_code.count('\n', pos, tag_start))
            yield tag
            tokenizer = (TSXReader.generate_tokens if tag.lang in ('tsx', 'jsx')
                         else TypeScriptReader.generate_tokens)
            yield from tokenizer(code, addition, token_class)
            pos = code_end
        yield from repeat('\n', source_code.count('\n', pos))

    def code_tokens(self, tokens):
        """Code tokens; a script tag sets the lang of the code after it."""
        for token in tokens:
            if token.__class__ is _ScriptTag:
                self.context.class_field_annotations = token.lang in (
                    'ts', 'tsx', 'typescript')
                yield from repeat('\n', token.count('\n'))
            else:
                yield token
