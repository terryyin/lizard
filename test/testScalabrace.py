import csv
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory
import unittest

from lizard import FileAnalyzer, get_extensions


class TestScalaBraceModes(unittest.TestCase):

    def test_source_api_uses_the_explicit_mode_for_the_same_code(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        sources = create_test_context()
        for mode, expected in expected_scopes().items():
            with self.subTest(mode=mode):
                extensions = [ScalaBraceMode(mode)] if mode else []
                analyzer = FileAnalyzer(get_extensions(extensions))
                result = [
                    (function.end_line, function.cyclomatic_complexity)
                    for filename, source in sources.items()
                    for function in analyzer.analyze_source_code(filename, source).function_list
                    if function.name.rsplit('::', 1)[-1] == 'total'
                ]
                self.assertEqual(expected, result)

    def test_cli_uses_the_mode_in_workers_and_keeps_java_metrics(self):
        script = Path(__file__).resolve().parents[1] / 'lizard.py'
        with TemporaryDirectory() as directory:
            paths = []
            for filename, source in create_test_context().items():
                path = Path(directory) / filename
                path.write_text(source, encoding='utf-8')
                paths.append(str(path))

            for mode, expected in expected_scopes().items():
                with self.subTest(mode=mode):
                    flags = ['-Escalamode', '--scala-brace-mode', mode] if mode else []
                    run = subprocess.run(
                        [sys.executable, str(script), '--csv', '-t', '2', *flags, *paths],
                        check=True, capture_output=True, text=True)
                    rows = {
                        Path(row[6]).name: (int(row[10]), int(row[1]))
                        for row in csv.reader(run.stdout.splitlines())
                        if row[7].rsplit('::', 1)[-1] == 'total'
                    }
                    self.assertEqual(dict(zip(create_test_context(), expected)), rows)

    def test_legacy_keeps_standalone_blocks_separate(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        source = """object Totals {
  def total(values: List[Int]): ((Int, Int) => Int) => Int = values.foldLeft { 0 } /* note

    */
    { (sum: Int, value: Int) => sum + value }
  def constant(): Int = { 1 }

    { if (true) println("outside") }
  def sum(values: List[Int]): Int = values.foldLeft { 0 } // initial sum
    { _ + _ }
}
"""
        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('legacy')]))
        functions = analyzer.analyze_source_code('Totals.scala', source).function_list
        self.assertEqual([('total', 2, 1), ('constant', 6, 1), ('sum', 10, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity) for f in functions])

    def test_comments_and_symbolic_selectors_in_both_layout_modes(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        source = """object Totals {
  def choose(a: Int)(b: Int): Int = a + b
  def +++(a: Int)(b: Int): Int = a + b
  def withLeadingComment(): Int = choose { 1 }
    /* note */ { if (true) 2 else 3 }
  def withTrailingComment(): Int = choose { 1 } /* note
    */
    { if (true) 2 else 3 }
  def afterMultilineComment(): Int = choose { 1 } /*
    */ { if (true) 2 else 3 }
  def withOperator(): Int = this.+++ { 1 }
    { if (true) 2 else 3 }
}
"""
        expected = [('withLeadingComment', 5, 2), ('withTrailingComment', 8, 2),
                    ('afterMultilineComment', 10, 2), ('withOperator', 12, 2)]
        names = {name for name, _, _ in expected}
        for mode in ('legacy', 'scala3-indented'):
            with self.subTest(mode=mode):
                analyzer = FileAnalyzer(get_extensions([ScalaBraceMode(mode)]))
                functions = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual(expected, [(f.name, f.end_line, f.cyclomatic_complexity)
                                            for f in functions if f.name in names])

    def test_scala3_indentation_scopes_restore_the_outer_width_on_dedent(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        source = """object Totals:
  object Nested:
    def inner(values: List[Int]): Int = values.foldLeft { 0 }
      { _ + _ }
  def sibling(values: List[Int]): Int = values.foldLeft { 0 }
    { _ + _ }
  def separate(values: List[Int]): Any = values.foldLeft { 0 }
  { (sum: Int, value: Int) => sum + value }

def top(values: List[Int]): Int = values.foldLeft { 0 }
  { _ + _ }
"""
        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
        functions = analyzer.analyze_source_code('Totals.scala', source).function_list
        self.assertEqual([('inner', 4), ('sibling', 6), ('separate', 7), ('top', 11)],
                         [(f.name, f.end_line) for f in functions])

    def test_scala3_private_def_can_be_the_first_member(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        source = """object Totals {
  private def total(values: List[Int]): Int = values.foldLeft { 0 }
    { _ + _ }
}
"""
        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
        functions = analyzer.analyze_source_code('Totals.scala', source).function_list
        self.assertEqual([('total', 3)], [(f.name, f.end_line) for f in functions])

    def test_qualified_first_member_confirms_the_braced_template_width(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        for modifier in ('private[qualified]', 'protected[qualified]',
                         'private[this]', 'protected[`qualified`] final'):
            for indent in ('  ', '    '):
                with self.subTest(modifier=modifier, indent=indent):
                    source = """package qualified

class Totals {
  MODIFIER def total(values: List[Int]): Any = values.foldLeft { 0 }
INDENT{ (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
""".replace('MODIFIER', modifier).replace('INDENT', indent)
                    analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
                    functions = analyzer.analyze_source_code('Totals.scala', source).function_list
                    expected = (5, 2) if len(indent) == 4 else (4, 1)
                    self.assertEqual([expected], [(f.end_line, f.cyclomatic_complexity)
                                                  for f in functions if f.name == 'total'])

    def test_annotations_before_the_first_member_preserve_its_width(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        prefixes = (
            '@deprecated("review control", "1") ',
            '@throws[IllegalArgumentException]\n  @scala.deprecated(("review control"), "1")\n  ',
        )
        for prefix in prefixes:
            with self.subTest(prefix=prefix):
                source = """package qualified

class Totals {
  PREFIXprivate[qualified] def total(values: List[Int]): Any = values.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
""".replace('PREFIX', prefix)
                analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
                function, = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual((5 + prefix.count('\n'), 2),
                                 (function.end_line, function.cyclomatic_complexity))

    def test_indented_template_starts_after_a_complete_declaration_prefix(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        prefixes = ('', 'private[qualified] ',
                    '@scala.deprecated("review control", "1")\nprivate[qualified] ')
        for prefix in prefixes:
            with self.subTest(prefix=prefix):
                source = """package qualified

PREFIXclass Totals:
  def total(values: List[Int]): Any = values.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
""".replace('PREFIX', prefix)
                analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
                function, = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual((5 + prefix.count('\n'), 2),
                                 (function.end_line, function.cyclomatic_complexity))

    def test_first_infix_method_confirms_the_braced_width(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        for annotation in ('', '@deprecated("review", "1") '):
            with self.subTest(annotation=annotation):
                source = """object Totals {
  PREFIXinfix def total(values: List[Int]): Any = values.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
""".replace('PREFIX', annotation)
                analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
                function, = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual((2, 3, 2), (function.start_line, function.end_line, function.cyclomatic_complexity))

    def test_case_templates_confirm_braced_and_indented_widths(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        braced = """object Totals {
  @deprecated("review", "1") case class Token(value: Int)
  def total(values: List[Int]): Any = values.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
"""
        indented = """package adjacent

@deprecated("review", "1") private[adjacent] case INTRO:
  def total(values: List[Int]): Any = values.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
"""
        sources = ((braced, 3), (indented.replace('INTRO', 'class Totals()'), 4),
                   (indented.replace('INTRO', 'object Totals'), 4))
        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
        for source, start in sources:
            with self.subTest(source=source):
                functions = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual([(start, start + 1, 2)],
                                 [(f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions if f.name == 'total'])

    def test_annotated_method_uses_the_enclosing_width_for_its_second_argument(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
        for brace_indent, expected in (('      ', (4, 2)), ('    ', (4, 2)), ('  ', (3, 1))):
            with self.subTest(brace_indent=brace_indent):
                source = """object Totals {
  @deprecated("review", "1")
    def total(values: List[Int]): Any = values.foldLeft { 0 }
INDENT{ (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
""".replace('INDENT', brace_indent)
                function, = analyzer.analyze_source_code('Totals.scala', source).function_list
                self.assertEqual(expected, (function.end_line, function.cyclomatic_complexity))

    def test_self_alias_cannot_confirm_a_braced_template_width(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        aliases = (('inline', 'object'), ('open', 'object'), ('inline: AnyRef', 'class'), ('self', 'object'))
        for alias, template in aliases:
            source = """TEMPLATE Totals {
  ALIAS =>
    val xs = List(1)
  def split(): Any = xs.foldLeft { 0 }
    { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
}
""".replace('ALIAS', alias).replace('TEMPLATE', template)
            for mode, expected in (
                    ('legacy', (5, 2)),
                    ('scala3-indented', (4, 1)),
                    ('scala3-no-indent', (4, 1))):
                with self.subTest(alias=alias, mode=mode):
                    analyzer = FileAnalyzer(get_extensions([ScalaBraceMode(mode)]))
                    functions = analyzer.analyze_source_code('Totals.scala', source).function_list
                    self.assertEqual([expected], [(f.end_line, f.cyclomatic_complexity)
                                                  for f in functions if f.name == 'split'])

    def test_nested_self_alias_preserves_the_enclosing_template_width(self):
        from lizard_ext.lizardscalamode import ScalaBraceMode

        source = """object Totals {
  object Inner {
    inline =>
      val xs = List(1)
    def split(): Any = xs.foldLeft { 0 }
      { (sum: Int, value: Int) => if (value > 0) sum + value else sum }
  }
  private final def total(values: List[Int]): Int = values.foldLeft { 0 }
    { _ + _ }
}
"""
        analyzer = FileAnalyzer(get_extensions([ScalaBraceMode('scala3-indented')]))
        functions = analyzer.analyze_source_code('Totals.scala', source).function_list
        self.assertEqual([('split', 5, 1), ('total', 9, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity)
                          for f in functions if f.name in ('split', 'total')])

    def test_cli_rejects_a_repeated_mode_extension(self):
        script = Path(__file__).resolve().parents[1] / 'lizard.py'
        run = subprocess.run(
            [sys.executable, str(script), '-Escalamode', '-Escalamode', '--help'],
            capture_output=True, text=True)
        self.assertEqual(2, run.returncode)
        self.assertIn('-Escalamode may be supplied only once', run.stderr)


def create_test_context():
    return {
        'Aligned.scala': """object Aligned {
  def choose(flag: Boolean)(a: => Int)(b: => Int): Int = if (flag) a else b
  def total(flag: Boolean): Any = choose(flag) { 1 }
  { if (true) 2 else 3 }
}
""",
        'Deeper.scala': """object Deeper {
  val values = List(1)
  def total(): Any = values.foldLeft { 0 }
    { (sum: Int, n: Int) => if (n > 0) sum + n else sum }
}
""",
        'Control.java': """class Control {
  int total(int input) {
    return input < 0 ? 0 : input;
  }
}
""",
    }


def expected_scopes():
    return {
        None: [(3, 1), (3, 1), (4, 2)],
        'legacy': [(4, 2), (4, 2), (4, 2)],
        'scala3-indented': [(3, 1), (4, 2), (4, 2)],
        'scala3-no-indent': [(3, 1), (3, 1), (4, 2)],
    }
