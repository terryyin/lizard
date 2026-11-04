import unittest

from .fsharp_helpers import spans


class TestFSharpItemBoundaries(unittest.TestCase):

    def test_locals_lambdas_and_values_stay_inside(self):
        self.assertEqual([('sum', 1, 6, 2), ('after', 12, 12, 1)], spans('''\
let sum l =
    printfn "start"
    let rec go acc = function
        | [] -> acc
        | x :: t -> go (acc + x) t
    go 0 (List.filter (fun x -> x > 0) l)
let next = 1
let a, b = 1, 2
let table =
    let build n = [1..n]
    build 10
let after x = x
'''))

    def test_decisions_of_locals_and_lambdas_count_in_the_binding(self):
        self.assertEqual([('outer', 1, 4, 3)], spans('''\
let outer xs =
    let inner y = if y then 1 else 0
    xs |> List.filter (fun x -> x > 0 && x < 9)
    |> List.map inner
'''))

    def test_blank_and_comment_lines_do_not_end_a_binding(self):
        self.assertEqual([('f', 1, 6, 1), ('g', 7, 7, 1)], spans('''\
let f x =

(* a comment at column 0 *)
// another
/// and a doc comment
    x
let g y = y
'''))

    def test_bar_and_infix_lines_continue_a_binding(self):
        self.assertEqual([('f', 1, 4, 1), ('g', 5, 5, 1)], spans('''\
let f xs =
    xs
|> List.map id
+ 0
let g y = y
'''))

    def test_brackets_suspend_the_offside_rule(self):
        self.assertEqual([('f', 1, 4, 1), ('eval', 5, 7, 2)], spans('''\
let f xs = [
    1
    2
]
let rec eval = function
| 0 -> 0
| n -> eval (n - 1)
let k = 1
'''))

    def test_multi_line_string_stays_inside_a_binding(self):
        self.assertEqual([('f', 1, 5, 1), ('g', 6, 6, 1)], spans('''\
let f x =
    let s = """first
if second
"""
    x
let g y = y
'''))

    def test_unfinished_heading_is_not_a_function(self):
        self.assertEqual([('g', 2, 2, 1)], spans('let f x\nlet g y = y\nlet h z'))

    def test_module_level_statement_ends_a_binding(self):
        self.assertEqual([('g', 1, 2, 1)], spans('''\
let g y =
    y
printfn "%d" (g 1)
'''))
