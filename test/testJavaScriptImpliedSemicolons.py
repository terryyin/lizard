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
