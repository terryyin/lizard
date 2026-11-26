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
_MARKUP_COMMENT = r'<!--.*?(?:-->|\Z)'
_MARKUP_INTERPOLATION = r'\{\{.*?\}\}'
_MARKUP_TAG = r"""<(/?)([A-Za-z][^\s/<>]*)(?:=\s*(?:"[^"]*"|'[^']*'|\{[^}]*\})|[^<>])*>?"""
_MARKUP = re.compile('|'.join((_MARKUP_COMMENT, _MARKUP_INTERPOLATION, _MARKUP_TAG)), re.S)
_MAYBE_V_PRE = re.compile(r'(?<=\s)v-pre(?=[\s/>=])')
_CDATA = r'<!\[CDATA\[.*?(?:\]\]>|\Z)'
_VUE_MARKUP = re.compile(
    '|'.join((_MARKUP_COMMENT, _CDATA, _MARKUP_INTERPOLATION, _MARKUP_TAG)), re.S)
_V_PRE_MARKUP = re.compile('|'.join((_MARKUP_COMMENT, _CDATA, _MARKUP_TAG)), re.S)
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
_VOID_TAGS = frozenset((
    'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link',
    'meta', 'param', 'source', 'track', 'wbr',
))
_TEXT_END = {name: re.compile(r'</(?ai:' + name + r')(?=[\t\n\f\r >])')
             for name in _TEXT_ELEMENTS}
_HTML, _SVG, _MATH = range(3)
_SVG_HTML_PARENTS = ('foreignObject', 'desc', 'title')
_MATH_TEXT_PARENTS = ('mi', 'mo', 'mn', 'ms', 'mtext')
_HTML_ENCODINGS = ('text/html', 'application/xhtml+xml')


def _attributes(tag):
    """The name and the value of each attribute of a start tag."""
    for match in _ATTRIBUTE.finditer(tag, 1):
        yield match.group(1), next(
            (v for v in match.group(2, 3, 4) if v is not None), '')


def _attribute(tag, name):
    """The value of an attribute of a start tag, or None without it."""
    return next((value for key, value in _attributes(tag)
                 if key.lower() == name), None)


class _ScriptTag(str):
    """The opening tag of a script block; it gives the lang of the code."""

    @property
    def lang(self):
        return (_attribute(self, 'lang') or 'js').lower()


def _script_blocks(source, find_blocks, end_pattern):
    """(tag start, tag, code, code end) of each top-level script block."""
    key = (id(source), find_blocks, end_pattern)
    cached = _BLOCK_CACHE.get(key)
    if cached is not None and cached[0] is source:
        return cached[1]
    blocks = [
        (tag_start, _ScriptTag(source[tag_start:code_start]),
         source[code_start:code_end], code_end)
        for tag_start, code_start, code_end in find_blocks(source, end_pattern)]
    _BLOCK_CACHE[key] = (source, blocks)
    return blocks


def _find_script_blocks(source, end_pattern):
    """(tag start, code start, code end) of each top-level script block.

    As in the Vue compiler, a top-level HTML <template> holds nested markup,
    and the other top-level blocks hold text up to their closing tag."""
    return _find_component_scripts(source, end_pattern, template_v_pre=True)


def _find_component_scripts(source, end_pattern, *, template_v_pre):
    pos = 0
    template_v_pre = template_v_pre and _MAYBE_V_PRE.search(source) is not None
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
            if template_v_pre:
                end = _vue_template_end(source, pos, match.group())
            else:
                end = _markup_end(source, pos)
            pos = end or pos
        elif name == 'script':
            end = _script_end(source, pos, end_pattern)
            yield match.start(), pos, end
            pos = end
        else:
            end = source.find('</' + name, pos)
            if end >= 0:
                pos = end


def _script_end(source, pos, end_pattern):
    """Where the code ends: at the first closing tag, as in Vue. When another
    closing tag follows before the next script, the first one was text in
    the code, and the first closing tag outside strings, comments and
    templates ends it."""
    end = end_pattern.search(source, pos)
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


def _has_v_pre(tag):
    return 'v-pre' in tag and any(name == 'v-pre' for name, _ in _attributes(tag))


def _namespace(name, parent):
    parent_name, namespace, html_encoding = parent
    if namespace == _SVG and parent_name in _SVG_HTML_PARENTS:
        namespace = _HTML
    elif namespace == _MATH:
        if parent_name == 'annotation-xml':
            if name == 'svg':
                return _SVG
            if html_encoding:
                namespace = _HTML
        elif parent_name in _MATH_TEXT_PARENTS and name not in ('mglyph', 'malignmark'):
            namespace = _HTML
    if namespace == _HTML:
        if name == 'svg':
            return _SVG
        if name == 'math':
            return _MATH
    return namespace


def _vue_template_end(source, pos, root):
    """Track the open tags so that v-pre ends at its own closing tag."""
    stack = [('template', _HTML, False)]
    v_pre = 1 if _has_v_pre(root) else 0
    in_xml = False
    while stack:
        markup = _V_PRE_MARKUP if v_pre else _VUE_MARKUP
        match = markup.search(source, pos)
        if match is None:
            return None
        pos = match.end()
        name, tag = match.group(2), match.group()
        if name is None or name in _VOID_TAGS or tag.endswith('/>'):
            continue
        if match.group(1):
            for index in range(len(stack) - 1, -1, -1):
                if stack[index][0].lower() == name.lower():
                    del stack[index:]
                    break
            if len(stack) < v_pre:
                v_pre = 0
            if stack and stack[-1][1] == _HTML:
                in_xml = False
            continue
        namespace = _namespace(name, stack[-1])
        text_name = name.lower()
        if not in_xml and name[0] in 'st' and text_name in _TEXT_ELEMENTS:
            close = _TEXT_END[text_name].search(source, pos)
            if close is None:
                return None
            pos = close.start()
        has_html_encoding = (name == 'annotation-xml'
                             and _attribute(tag, 'encoding') in _HTML_ENCODINGS)
        stack.append((name, namespace, has_html_encoding))
        if not v_pre and _has_v_pre(tag):
            v_pre = len(stack)
        in_xml = in_xml or namespace != _HTML
    return pos


class VueReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue', 'vuejs']
    # A lang="tsx" block keeps whole templates, as in a .tsx file.
    _scan_atomic_templates = True
    script_tokens = staticmethod(TypeScriptReader.generate_tokens)
    _script_end_pattern = _SCRIPT_END
    find_script_blocks = staticmethod(_find_script_blocks)

    def __init__(self, context):
        super(VueReader, self).__init__(context)

    @classmethod
    def generate_tokens(cls, source_code, addition='', token_class=None):
        """The code of the script blocks, tokenized by their lang, and the
        tokens of the markup around them."""
        pos = 0
        blocks = _script_blocks(source_code, cls.find_script_blocks, cls._script_end_pattern)
        for tag_start, tag, code, code_end in blocks:
            yield from cls.markup_tokens(source_code, pos, tag_start)
            yield from cls.tag_tokens(tag)
            tokenizer = (TSXReader.generate_tokens if tag.lang in ('tsx', 'jsx')
                         else TypeScriptReader.generate_tokens)
            yield from tokenizer(code, addition, token_class)
            pos = code_end
        yield from cls.markup_tokens(source_code, pos, len(source_code))

    @staticmethod
    def markup_tokens(source_code, start, end):
        """The markup keeps only its newlines, so the lines are those of
        the file."""
        return repeat('\n', source_code.count('\n', start, end))

    @staticmethod
    def tag_tokens(tag):
        """The tokens of a script tag: the tag, for code_tokens."""
        return (tag,)

    def code_tokens(self, tokens):
        """Code tokens; a script tag sets the lang of the code after it."""
        for token in tokens:
            if token.__class__ is _ScriptTag:
                self.context.class_field_annotations = token.lang in (
                    'ts', 'tsx', 'typescript')
                yield from repeat('\n', token.count('\n'))
            else:
                yield token
