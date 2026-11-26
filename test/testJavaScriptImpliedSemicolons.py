import unittest

from lizard import FileAnalyzer, get_extensions


NOTIFY = """
function notify(user, devices) {
  if (user.active)
    for (const device of devices)
      if (device.online) device.push(user)
  if (user.email) send(user.email)
  else log(user)
}
"""


class TestImpliedSemicolonsOfOneFile(unittest.TestCase):
    """The extensions on one file read one finder of the places where
    its statements end without a ';'."""

    def test_each_extension_gets_every_statement_end(self):
        first, second = StatementEnds(), StatementEnds()

        FileAnalyzer(get_extensions([first, second])).analyze_source_code(
            "a.js", NOTIFY)

        self.assertIs(first.finder, second.finder)
        ends = [('if', 6), ('else', 7), ('}', 8)]
        self.assertEqual(ends, first.ends)
        self.assertEqual(ends, second.ends)

    def test_nesting_depth_and_nested_structures_together(self):
        for extensions in (['nd', 'ns'], ['ns', 'nd']):
            functions = FileAnalyzer(get_extensions(extensions)) \
                .analyze_source_code("a.js", NOTIFY).function_list

            self.assertEqual(
                [(3, 3)],
                [(f.max_nesting_depth, f.max_nested_structures)
                 for f in functions])

    def test_each_file_gets_its_own_finder(self):
        extension = StatementEnds()
        analyzer = FileAnalyzer(get_extensions([extension]))

        analyzer.analyze_source_code("a.js", NOTIFY)
        first = extension.finder
        analyzer.analyze_source_code("b.ts", NOTIFY)

        self.assertIsNot(first, extension.finder)
        self.assertEqual([('if', 6), ('else', 7), ('}', 8)], extension.ends)


class StatementEnds:
    """An extension that records where the reader finds a statement end
    without a ';'."""

    def __call__(self, tokens, reader):
        tokens, self.finder = reader.implied_semicolons(tokens)
        self.ends = []
        return self._record(tokens, reader.context)

    def _record(self, tokens, context):
        for token in tokens:
            if self.finder.before_token:
                self.ends.append((token, context.current_line))
            yield token


class TestAssertedFunctionExpressionBoundaries(unittest.TestCase):
    def test_following_identifier_starts_a_sibling_statement(self):
        for signature in ('function ()', 'function fallback()', 'async function ()'):
            for assertion in ('!', '!!'):
                with self.subTest(signature=signature, assertion=assertion):
                    code = """\
function configure(options: { resolve?: () => boolean; debug: boolean }) {
  const resolve = options.resolve || %s { return true }%s
  options.debug && resolve()
}
""" % (signature, assertion)
                    analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
                    functions = analyzer.analyze_source_code('options.ts', code).function_list
                    configure = functions[-1]
                    self.assertEqual(('configure', 1, 0),
                                     (configure.name, configure.max_nesting_depth,
                                      configure.max_nested_structures))

    def test_function_declaration_followed_by_prefix_negation(self):
        code = """\
function configure() {
  function ready() { return true }!
    ready()
}
"""
        for filename in ('options.ts', 'options.js'):
            with self.subTest(filename=filename):
                extension = StatementEnds()
                analyzer = FileAnalyzer(get_extensions([extension]))
                analyzer.analyze_source_code(filename, code)
                self.assertNotIn(('ready', 3), extension.ends)

    def test_negation_on_a_new_line_after_a_function_expression(self):
        code = """\
function configure() {
  const ready = function () { return true }
  !
    ready()
}
"""
        for filename in ('options.ts', 'options.js'):
            with self.subTest(filename=filename):
                extension = StatementEnds()
                analyzer = FileAnalyzer(get_extensions([extension]))
                analyzer.analyze_source_code(filename, code)
                self.assertNotIn(('ready', 4), extension.ends)

    def test_following_identifier_after_the_alternate_of_a_conditional(self):
        code = """\
function configure(options: { resolve?: () => boolean; debug: boolean }) {
  const resolve = options.resolve ? options.resolve : function () { return true }!
  options.debug && resolve()
}
"""
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        for source in (code, code.replace('}!\n', '}!;\n')):
            with self.subTest(semicolon='!;' in source):
                functions = analyzer.analyze_source_code('options.ts', source).function_list
                configure = functions[-1]
                self.assertEqual(('configure', 1, 0),
                                 (configure.name, configure.max_nesting_depth,
                                  configure.max_nested_structures))

    def test_case_colon_is_not_a_conditional_alternate(self):
        code = """\
function configure(useDefault) {
  switch (0) {
    case useDefault ? 0 : 1: function ready() { return true }!
      ready()
  }
}
"""
        for filename in ('options.ts', 'options.js'):
            with self.subTest(filename=filename):
                extension = StatementEnds()
                analyzer = FileAnalyzer(get_extensions([extension]))
                analyzer.analyze_source_code(filename, code)
                self.assertNotIn(('ready', 4), extension.ends)
