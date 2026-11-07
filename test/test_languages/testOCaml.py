import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from lizard import analyze_file

from .ocaml_helpers import get_ocaml_function_list


def ccn_by_name(source_code):
    return {f.name: f.cyclomatic_complexity
            for f in get_ocaml_function_list(source_code)}


class TestOCamlFunctionRecognition(unittest.TestCase):

    def test_branching_and_curried_parameters(self):
        fact, greet = get_ocaml_function_list(
            'let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\n'
            'let greet ~name ?(punct = "!") () = '
            'print_endline (name ^ punct)\n')
        self.assertEqual(
            ('fact', 2, 1, 1),
            (fact.name, fact.cyclomatic_complexity,
             fact.parameter_count, fact.nloc))
        self.assertEqual(
            ('greet', 1, 3, 1),
            (greet.name, greet.cyclomatic_complexity,
             greet.parameter_count, greet.nloc))

    def test_or_operator_is_a_decision(self):
        g, = get_ocaml_function_list('let g a b = a || b\n')
        self.assertEqual(2, g.cyclomatic_complexity)

    def test_local_helpers_and_lambdas_stay_inside(self):
        functions = get_ocaml_function_list('''\
let sum l =
  let rec go acc = function
    | [] -> acc
    | x :: t -> go (acc + x) t
  in
  go 0 (List.filter (fun x -> x > 0) l)
let next = 1
let after x = x
''')
        self.assertEqual(
            [('sum', 1, 6, 2), ('after', 8, 8, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in functions])

    def test_leading_blank_lines_keep_line_numbers(self):
        functions = get_ocaml_function_list('\n\nlet f x = x\n')
        self.assertEqual(
            [('f', 3, 3)],
            [(f.name, f.start_line, f.end_line) for f in functions])

    def test_comments_and_strings_are_inert(self):
        result = analyze_file.analyze_source_code("a.ml", '''\
(* if (* while *) || *)
let f (x : 'a list) =
  let s = "if \\" then" in let c = '|' in
  (s, c, x)
let h y = y
''')
        f, h = result.function_list
        self.assertEqual(4, result.nloc)
        self.assertEqual(
            ('f', 1, 1, 2, 3),
            (f.name, f.cyclomatic_complexity, f.parameter_count,
             f.start_line, f.nloc))
        self.assertEqual('h', h.name)

    def test_binding_whose_body_starts_with_fun_is_a_function(self):
        add, = get_ocaml_function_list('let add = fun a b -> a + b\n')
        self.assertEqual(('add', 0), (add.name, add.parameter_count))

    def test_let_after_non_ending_tokens_is_local(self):
        functions = get_ocaml_function_list('''\
let f c =
  print_string "a"; let z = 1 in z
  + let q = 2 in q;
  if c then let a = 1 in a
  else let b = 2 in b
let g = fun x -> let y = x in y
let last x = x
''')
        self.assertEqual(
            [('f', 1, 5), ('g', 6, 6), ('last', 7, 7)],
            [(f.name, f.start_line, f.end_line) for f in functions])

    def test_let_inside_brackets_and_begin_end_is_local(self):
        functions = get_ocaml_function_list('''\
let f x =
  (f x
   let y = 1 in y);
  begin g ()
    let w = 2 in w end
let last x = x
''')
        self.assertEqual(
            [('f', 1, 5), ('last', 6, 6)],
            [(f.name, f.start_line, f.end_line) for f in functions])

    def test_local_let_and_fun_tokens_count_in_enclosing_function(self):
        f, = get_ocaml_function_list(
            'let f x = let g = fun y -> y in g x\n')
        # let f x = let g = fun y -> y in g x
        self.assertEqual(14, f.token_count)

    def test_loops_are_decisions_and_begin_end_is_not(self):
        f, = get_ocaml_function_list('''\
let f n =
  while n > 0 do () done;
  for i = 1 to n do begin print_int i end done
''')
        self.assertEqual(3, f.cyclomatic_complexity)

    def test_optional_argument_counts_one_parameter(self):
        opt, = get_ocaml_function_list('let opt ?x y = y\n')
        self.assertEqual(2, opt.parameter_count)

    def test_comment_inside_function_body_is_inert(self):
        f, = get_ocaml_function_list('''\
let f x =
  (* if (* let g y = y *) || *)
  x
''')
        self.assertEqual(
            ('f', 1, 2, 3),
            (f.name, f.cyclomatic_complexity, f.nloc, f.end_line))


class TestOCamlMatchArms(unittest.TestCase):

    def test_function_arms_without_leading_bar(self):
        f, = get_ocaml_function_list(
            'let f = function Some x -> x | None -> 0\n')
        self.assertEqual(
            ('f', 2, 0),
            (f.name, f.cyclomatic_complexity, f.parameter_count))

    def test_arms_guards_and_or_patterns(self):
        self.assertEqual({'classify': 6}, ccn_by_name('''\
let classify x = match x with
  | 0 | 1 -> "small"
  | n when n < 0 -> "neg"
  | n -> if n > 9 && n < 100 then "mid" else "big"
'''))

    def test_logical_or_inside_arm_is_not_an_arm(self):
        self.assertEqual({'g': 3}, ccn_by_name(
            'let g a b = match a with | true -> a || b | false -> not b\n'))

    def test_each_try_handler_arm_adds_one(self):
        self.assertEqual({'read': 3, 'lead': 3}, ccn_by_name('''\
let read k = try find k with Not_found -> None | Invalid_argument _ -> None
let lead k = try find k with | Not_found -> None | Exit -> None
'''))

    def test_match_scores_like_equivalent_if_chain(self):
        self.assertEqual({'m': 3, 'i': 3}, ccn_by_name('''\
let m x = match x with 0 -> "a" | 1 -> "b" | _ -> "c"
let i x = if x = 0 then "a" else if x = 1 then "b" else "c"
'''))

    def test_fun_and_type_arrows_add_nothing(self):
        self.assertEqual({'a': 2, 'h': 1, 'k': 1}, ccn_by_name('''\
let a x = match x with | A -> fun y -> y | B -> fun y -> y + 1
let h (k : int -> int) x = k x
let k : int -> int = fun x -> x
'''))

    def test_nested_arms_belong_to_inner_construct(self):
        self.assertEqual({'p': 4, 'b': 3, 'l': 3, 'w': 3}, ccn_by_name('''\
let p x y = match x with
  | A -> (match y with C -> 1 | D -> 2)
  | B -> 3
  | E -> 4
let b x y = match x with
  | A -> begin match y with C -> 1 | D -> 2 end
  | B -> 3
let l x y = match x with
  | A -> let v = match y with C -> 1 | D -> 2 in v
  | B -> 3
let w x = try match x with A -> 1 | B -> 2 with Exit -> 0
'''))

    def test_record_update_has_no_arms(self):
        self.assertEqual({'u': 2, 't': 3}, ccn_by_name('''\
let u r = match r with A -> { r with x = 1 } | B -> r
let t r = match (try { r with x = 1 } with E -> r) with A -> 1 | B -> 2
'''))


class TestOCamlCliDiscovery(unittest.TestCase):

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

    def test_language_filter_and_help(self):
        with TemporaryDirectory() as directory:
            src_dir = Path(directory) / 'src'
            src_dir.mkdir()
            (src_dir / 'main.ml').write_text('let double x = x * 2\n')
            (src_dir / 'main.mli').write_text(
                'val double : int -> int\nlet in_signature y = y\n')
            (Path(directory) / 'tool.py').write_text(
                'def tool_f():\n    return 1\n')

            everything = self.run_lizard(directory)
            ocaml_only = self.run_lizard('-l', 'ocaml', directory)

        self.assertIn('double@1-1@', everything)
        self.assertIn('tool_f@1-2@', everything)
        self.assertIn('double@1-1@', ocaml_only)
        self.assertNotIn('tool_f', ocaml_only)
        self.assertNotIn('main.mli', everything + ocaml_only)
        self.assertNotIn('in_signature', everything + ocaml_only)
        self.assertIn('ocaml', self.run_lizard('-h'))

    def test_readme_lists_ocaml(self):
        readme = Path(self.lizard_dir) / 'README.rst'
        self.assertIn('-  OCaml', readme.read_text())
