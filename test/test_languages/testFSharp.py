import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from lizard import analyze_file

from .fsharp_helpers import SCRIPT, analyze_fsharp, by_name, \
    get_fsharp_function_list, spans


class TestFSharpFunctionRecognition(unittest.TestCase):

    def test_straight_line_and_branching(self):
        fact, greet, g = get_fsharp_function_list('''\
let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
let greet (name: string) (punct: string) = printfn "%s%s" name punct
let g a b = a || b
''')
        self.assertEqual(('fact', 2, 1, 1),
                         (fact.name, fact.cyclomatic_complexity,
                          fact.parameter_count, fact.nloc))
        self.assertEqual(('greet', 1, 2, 1),
                         (greet.name, greet.cyclomatic_complexity,
                          greet.parameter_count, greet.nloc))
        self.assertEqual(('g', 2, 2, 1),
                         (g.name, g.cyclomatic_complexity,
                          g.parameter_count, g.nloc))

    def test_unit_parameter_counts_zero(self):
        f, = get_fsharp_function_list('let f () = ()\n')
        self.assertEqual(('f', 0), (f.name, f.parameter_count))

    def test_tuple_elements_each_count_one(self):
        functions = by_name('''\
let f (a, b) = a
let add (a: int, b: int) c = a + b + c
let m (x: Map<int, string>) = x
''')
        self.assertEqual(2, functions['f'].parameter_count)
        self.assertEqual(3, functions['add'].parameter_count)
        self.assertEqual(1, functions['m'].parameter_count)

    def test_return_annotation_and_generics_add_no_parameters(self):
        functions = by_name('''\
let size (xs: int list) : int = List.length xs
let f<'T when 'T : comparison> (x: 'T) : 'T = x
''')
        self.assertEqual(1, functions['size'].parameter_count)
        self.assertEqual(1, functions['f'].parameter_count)

    def test_fun_and_function_bodies_are_functions(self):
        functions = by_name('''\
let add = fun a b -> a + b
let f = function
    | Some x -> x
    | None -> 0
''')
        self.assertEqual(['add', 'f'], sorted(functions))
        self.assertEqual(0, functions['f'].parameter_count)
        self.assertEqual((2, 4), (functions['f'].start_line,
                                  functions['f'].end_line))

    def test_value_bindings_are_not_functions(self):
        functions = get_fsharp_function_list('''\
let x = 1
let mutable c = 0
let a, b = 1, 2
let (p, q) = 3, 4
let y: int = if c > 0 then 1 else 2
let after z = z
''')
        self.assertEqual(['after'], [f.name for f in functions])

    def test_each_control_flow_keyword_and_operator_adds_one(self):
        f, = get_fsharp_function_list('''\
let f a b =
    if a then 1
    elif b then 2
    else
        while a do ()
        for i in 1 .. 3 do ()
        if a && b || not a then 3 else 4
''')
        self.assertEqual(8, f.cyclomatic_complexity)


class TestFSharpCommentsAndStrings(unittest.TestCase):

    SOURCE = '''\
(* if (* while *) || *)
/// doc comment with if
// line comment with match
[<EntryPoint>]
let main argv =
    let s = "if \\" then" + @"c:\\if" + """x if y""" + $"v {argv} if"
    let c = '|'
    let t : 'a list = []
    0 // trailing while
let h y = y
'''

    def test_comments_strings_and_attributes_are_inert(self):
        main, h = get_fsharp_function_list(self.SOURCE)
        self.assertEqual(('main', 5, 9, 1, 5, 1),
                         (main.name, main.start_line, main.end_line,
                          main.cyclomatic_complexity, main.nloc,
                          main.parameter_count))
        self.assertEqual(('h', 10, 10), (h.name, h.start_line, h.end_line))

    def test_attribute_on_the_binding_line_belongs_to_no_function(self):
        t, u = get_fsharp_function_list('''\
[<Fact>] let t () = 1
[<Fact; Trait("a","b")>] let u x = if x then 1 else 2
[<Literal>]
let k = 3
''')
        self.assertEqual(('t', 1, 1, 0, 1, 5),
                         (t.name, t.start_line, t.end_line,
                          t.parameter_count, t.nloc, t.token_count))
        self.assertEqual(('u', 2, 2, 2, 1, 10),
                         (u.name, u.start_line, u.end_line,
                          u.cyclomatic_complexity, u.nloc, u.token_count))

    def test_comment_lines_are_out_of_file_nloc(self):
        self.assertEqual(7, analyze_fsharp(self.SOURCE).nloc)

    def test_leading_blank_lines_keep_line_numbers(self):
        self.assertEqual([('f', 3, 3, 1)], spans('\n\nlet f x = x\n'))


class TestFSharpCliDiscovery(unittest.TestCase):

    def setUp(self):
        self.lizard_dir = os.path.dirname(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        self.lizard_py = os.path.join(self.lizard_dir, 'lizard.py')

    def run_lizard(self, *args):
        result = subprocess.run(
            [sys.executable, self.lizard_py] + list(args),
            capture_output=True, text=True, cwd=self.lizard_dir)
        self.assertEqual(0, result.returncode,
                         msg=result.stderr or result.stdout)
        return result.stdout

    def write_project(self, directory):
        src_dir = Path(directory) / 'src'
        src_dir.mkdir()
        (src_dir / 'Lib.fs').write_text(
            'module Lib\n\nlet double x = x * 2\n')
        (src_dir / 'Lib.fsi').write_text(
            'module Lib\nval double : int -> int\nlet in_signature y = y\n')
        (Path(directory) / 'Script.fsx').write_text(
            SCRIPT)
        (Path(directory) / 'tool.py').write_text(
            'def tool_f():\n    return 1\n')

    def test_language_filter_and_help(self):
        with TemporaryDirectory() as directory:
            self.write_project(directory)
            everything = self.run_lizard(directory)
            fsharp_only = self.run_lizard('-l', 'fsharp', directory)

        for output in (everything, fsharp_only):
            self.assertIn('double@3-3@', output)
            self.assertIn('f@3-7@', output)
            self.assertIn('g@9-9@', output)
            self.assertNotIn('Lib.fsi', output)
            self.assertNotIn('in_signature', output)
        self.assertIn('tool_f@1-2@', everything)
        self.assertNotIn('tool_f', fsharp_only)
        self.assertIn('fsharp', self.run_lizard('-h'))

    def test_analyze_file_reads_an_fs_file(self):
        with TemporaryDirectory() as directory:
            self.write_project(directory)
            info = analyze_file(str(Path(directory) / 'src' / 'Lib.fs'))
        self.assertEqual([('double', 1)], [
            (f.name, f.parameter_count) for f in info.function_list])

    def test_readme_and_docs_describe_fsharp(self):
        readme = Path(self.lizard_dir) / 'README.rst'
        self.assertIn('-  F# (see `F# analysis <docs/fsharp-analysis.md>`_)',
                      readme.read_text())
        self.assertTrue(
            (Path(self.lizard_dir) / 'docs' / 'fsharp-analysis.md').is_file())
