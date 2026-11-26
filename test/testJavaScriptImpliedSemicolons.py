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

    def test_class_expression_can_end_a_braceless_body(self):
        source = """\
function setup(config, install, start) {
  if (install) config.factory = () => class Session {}
  if (start) config.run()
}
function choose(config, install, start) {
  if (install) config.Entry = class Session {}
  if (start) config.run()
}
function declaration(config) {
  class Session {}
  if (config.open) config.open()
}
function continueExpression(config) {
  if (config.install) config.Entry = class Session {}
    .prototype
  if (config.open) config.open()
}
"""
        for filename in ('session.js', 'session.ts'):
            with self.subTest(filename=filename):
                boundary = StatementEnds()
                analyzer = FileAnalyzer(get_extensions([boundary, 'nd']))
                functions = analyzer.analyze_source_code(filename, source).function_list
                self.assertEqual([3, 7, 16],
                                 [line for token, line in boundary.ends if token == 'if'])
                self.assertEqual(1, next(f.max_nesting_depth for f in functions
                                         if f.name == 'setup'))

    def test_base_class_brace_does_not_end_the_derived_class_expression(self):
        source = """\
function setup(config, install, start) {
  if (install) config.factory = () => class Session extends class Parent {}
  {}
  if (start) config.run()
}
"""
        boundary = StatementEnds()
        FileAnalyzer(get_extensions([boundary])).analyze_source_code('session.js', source)
        self.assertNotIn(('{', 3), boundary.ends)

    def test_nesting_depth_and_nested_structures_together(self):
        for extensions in (['nd', 'ns'], ['ns', 'nd']):
            functions = FileAnalyzer(get_extensions(extensions)) \
                .analyze_source_code("a.js", NOTIFY).function_list

            self.assertEqual(
                [(3, 3)],
                [(f.max_nesting_depth, f.max_nested_structures)
                 for f in functions])

    def test_else_after_an_assignment_that_ends_in_a_brace(self):
        cases = (
            ('function () {}', 'else'),
            ('() => {}', 'else if (jobs.length)'),
        )
        for value, clause in cases:
            with self.subTest(value=value, clause=clause):
                code = f"""
                function setup(flag, jobs) {{
                  if (flag) jobs.run = {value}
                  {clause} for (const job of jobs) job()
                }}
                """
                setup = FileAnalyzer(get_extensions(['nd', 'ns'])) \
                    .analyze_source_code("a.js", code).function_list[-1]
                self.assertEqual(
                    (2, 2), (setup.max_nesting_depth,
                             setup.max_nested_structures))

    def test_object_literal_preserves_statement_end_before_else(self):
        code = """
        function configure(useDefaults, settings, hooks) {
          if (useDefaults) settings.logging = { enabled: true }
          else for (const hook of hooks) hook(settings)
        }
        """
        for filename in ("a.js", "a.ts"):
            with self.subTest(filename=filename):
                extension = StatementEnds()
                FileAnalyzer(get_extensions([extension])).analyze_source_code(
                    filename, code)
                self.assertIn(('else', 4), extension.ends)

    def test_else_after_a_loop_with_an_object_literal_assignment(self):
        code = """
        function setup(retry, queue) {
          if (retry)
            for (const job of queue) job.options = {}
          else
            for (const job of queue) job.run()
        }
        """
        for filename in ("a.js", "a.ts"):
            with self.subTest(filename=filename):
                setup = FileAnalyzer(get_extensions(['ns'])) \
                    .analyze_source_code(filename, code).function_list[0]
                self.assertEqual(2, setup.max_nested_structures)

    def test_outer_else_after_an_object_literal_assignment(self):
        code = """
        function nestedElse(ready, cached, job, queue) {
          if (ready)
            if (cached) job.run()
            else job.options = {}
          else
            for (const next of queue) next.run()
        }
        """
        for filename in ("a.js", "a.ts"):
            with self.subTest(filename=filename):
                nested_else = FileAnalyzer(get_extensions(['ns'])) \
                    .analyze_source_code(filename, code).function_list[0]
                self.assertEqual(2, nested_else.max_nested_structures)

    def test_trailing_while_after_a_function_expression(self):
        code = """
        function setup(retry, jobs) {
          if (retry)
            do jobs.run = function () { return jobs.shift() }
            while (jobs.length)
          else for (const job of jobs) job()
        }
        """
        setup = FileAnalyzer(get_extensions(['nd', 'ns'])) \
            .analyze_source_code("a.js", code).function_list[-1]
        self.assertEqual(
            (2, 2), (setup.max_nesting_depth, setup.max_nested_structures))

    def test_function_body_that_returns_an_arrow(self):
        code = """
        function setup(replace, ready) {
          if (replace) create = function () { return () => 1 }
          if (ready) use(create)
        }
        """
        for filename in ("a.js", "a.ts"):
            with self.subTest(filename=filename):
                setup = FileAnalyzer(get_extensions(['nd'])) \
                    .analyze_source_code(filename, code).function_list[-1]
                self.assertEqual(1, setup.max_nesting_depth)

    def test_class_expression_at_the_end_of_an_arrow(self):
        code = """
        function setup(flag, ready) {
          if (flag) create = () => class {}
          if (ready) use(create)
        }
        """
        for filename in ("a.js", "a.ts"):
            with self.subTest(filename=filename):
                extension = StatementEnds()
                FileAnalyzer(get_extensions([extension])).analyze_source_code(
                    filename, code)
                self.assertIn(('if', 4), extension.ends)

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
