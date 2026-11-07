import unittest

from lizard import analyze_file

from .ocaml_helpers import get_ocaml_function_list, spans


class TestOCamlItemBoundaries(unittest.TestCase):

    def test_and_siblings_on_one_line(self):
        self.assertEqual(
            [('even', 1, 1, 2), ('odd', 1, 1, 2)], spans(
                'let rec even n = if n = 0 then true else odd (n - 1) '
                'and odd n = if n = 0 then false else even (n - 1)\n'))

    def test_and_siblings_over_lines_follow_the_function_rule(self):
        even, odd = get_ocaml_function_list('''\
let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1)
and limit = 10
''')
        self.assertEqual(
            [('even', 1, 2, 2, 2, 1), ('odd', 3, 4, 2, 2, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.parameter_count) for f in (even, odd)])

    def test_local_and_and_type_and_are_not_siblings(self):
        self.assertEqual(
            [('f', 1, 2, 2), ('after', 4, 4, 1)], spans('''\
let f x = let a = 1 and b = 2 in
  if x then a else b
type a = X and b = Y
let after x = x
'''))

    def test_module_struct_end_closes_its_last_function(self):
        self.assertEqual(
            [('g', 2, 3, 3), ('h', 5, 5, 1)], spans('''\
module M = struct let x = 1
  let g a b =
    a && b || not a end
type t = A | B
let h x = x
'''))

    def test_one_line_module_example(self):
        self.assertEqual(
            [('g', 1, 1, 3), ('h', 3, 3, 1)], spans(
                'module M = struct let g a b = a && b || not a end\n'
                'type t = A | B\n'
                'let h x = x\n'))

    def test_type_after_match_arm_adds_nothing(self):
        self.assertEqual(
            [('k', 1, 1, 2), ('h', 3, 3, 1)], spans('''\
let k x = match x with A -> 1 | B -> 2
type u = C | D of (int -> int) | E of int -> int
let h x = x
'''))

    def test_each_item_kind_ends_the_function(self):
        for item in ('type t = A | B', 'module N = M', 'open M',
                     'include M', 'exception E of int', ';;',
                     'external e : int -> int = "c_e"'):
            result = analyze_file.analyze_source_code(
                "a.ml", 'let f x =\n  if x then 1 else 2\n'
                + item + '\nlet g y = y\n')
            # let f x = if x then 1 else 2 / let g y = y
            self.assertEqual(
                [('f', 1, 2, 2, 2, 10), ('g', 4, 4, 1, 1, 5)],
                [(fn.name, fn.start_line, fn.end_line,
                  fn.cyclomatic_complexity, fn.nloc, fn.token_count)
                 for fn in result.function_list],
                msg=item)
            self.assertEqual(4, result.nloc, msg=item)

    def test_item_on_the_function_line_goes_to_no_function(self):
        f, = get_ocaml_function_list('let f x = x type t = A | B\n')
        self.assertEqual((1, 1, 5), (f.end_line, f.nloc, f.token_count))

    def test_signatures_and_constraints_add_no_functions(self):
        self.assertEqual(
            [('f', 6, 9, 2), ('b', 11, 12, 1)], spans('''\
module type S = sig
  type t
  val f : int -> int
end
module Inner : S with type t = int = struct
  let f x =
    match x with
    | 0 -> 1
    | _ -> 2 ;;
  type t = int
  let b (x : t) = x
end
'''))

    def test_local_module_stays_inside_function(self):
        self.assertEqual([('c', 1, 3, 2)], spans('''\
let c x =
  let module L = struct let y = 1 let w a = a end in
  if x then L.y else L.w 2
'''))

    def test_realistic_sample(self):
        self.assertEqual([
            ('empty', 6, 6, 1),
            ('insert', 7, 11, 3),
            ('size', 12, 16, 2),
            ('even', 18, 19, 2),
            ('odd', 20, 21, 2),
            ('main', 25, 27, 2),
        ], spans('''\
open Printf

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

module Tree = struct
  let empty () = Leaf
  let insert cmp x t =
    match t with
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) ->
        if cmp x v < 0 then Node (insert cmp x l, v, r) else t
  let size t =
    match t with
    | Leaf -> 0
    | Node (l, _, r) -> 1 + size l + size r
end

let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1)

;;

let main args =
  let n = Array.length args in
  if even n then printf "even\\n" else printf "odd\\n"

let () = main Sys.argv
'''))
