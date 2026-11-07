import unittest

from lizard import analyze_file

from .fsharp_helpers import SCRIPT, by_name, spans


class TestFSharpMembers(unittest.TestCase):

    def test_type_members_and_type_level_lets(self):
        functions = by_name('''\
type Counter(start: int) =
    let mutable count = start
    let bump n = if n > 0 then n + 1 else n
    member this.Increment() =
        count <- bump count
        count
    member _.Value = count
    member this.Add(a: int, b: int) = if a > b then a else b
    static member Create() = Counter(0)
    override x.ToString() = ""
    member val Name = "" with get, set
    abstract member Foo: int -> int
    default this.Foo n = if n = 0 then 1 else n
''')
        self.assertEqual(
            {'bump': (3, 3, 2, 1), 'Increment': (4, 6, 1, 0),
             'Add': (8, 8, 2, 2), 'Create': (9, 9, 1, 0),
             'ToString': (10, 10, 1, 0), 'Foo': (13, 13, 2, 1)},
            {name: (f.start_line, f.end_line, f.cyclomatic_complexity,
                    f.parameter_count) for name, f in functions.items()})

    def test_union_members_and_case_bars(self):
        self.assertEqual([('Scale', 5, 6, 2)], spans('''\
type Shape =
    | Circle of float
    | Square of float
    member this.Area = 1.0
    member this.Scale k =
        match this with Circle r -> Circle (r * k) | Square s -> Square (s * k)
'''))

    def test_lambda_valued_member_is_a_property(self):
        self.assertEqual([('Run', 3, 3, 1)], spans('''\
type T() =
    member _.Handler = fun x -> x
    member _.Run x = x
'''))

    def test_attributed_member_and_static_let(self):
        functions = by_name('''\
type Tests() =
    static let helper x = x
    [<Fact>] member _.Works() = if true then () else ()
''')
        self.assertEqual({'helper': (2, 1), 'Works': (3, 2)},
                         {name: (f.start_line, f.cyclomatic_complexity)
                          for name, f in functions.items()})


class TestFSharpNestedItems(unittest.TestCase):

    def test_nested_modules(self):
        self.assertEqual(
            [('f', 2, 2, 1), ('g', 4, 4, 2), ('h', 5, 5, 1),
             ('top', 6, 6, 1)], spans('''\
module Outer =
    let f x = x
    module Inner =
        let g y = if y then 1 else 2
    let h z = z
let top w = w
'''))

    def test_and_continuing_a_type_group_is_a_type(self):
        self.assertEqual(
            [('M', 2, 2, 2), ('N', 4, 4, 2), ('after', 5, 5, 1)], spans('''\
type A() =
    member this.M x = if x then 1 else 2
and B() =
    member this.N y = if y then 1 else 2
let after z = z
'''))

    def test_rec_and_group(self):
        self.assertEqual([('even', 1, 1, 2), ('odd', 2, 2, 2)], spans('''\
let rec even n = if n = 0 then true else odd (n - 1)
and odd n = if n = 0 then false else even (n - 1)
'''))

    def test_declarations_are_not_functions(self):
        self.assertEqual([('f', 5, 7, 1), ('g', 8, 8, 1)], spans('''\
namespace N
module M
open System
exception Oops of string
let f x =
    use r = new System.IO.StringReader(x)
    r.ReadToEnd()
let g y = y
'''))


class TestFSharpDirectives(unittest.TestCase):

    def test_directive_lines_are_inert(self):
        result = analyze_file.analyze_source_code('Script.fsx', SCRIPT)
        self.assertEqual([('f', 3, 7, 2, 3), ('g', 9, 9, 1, 1)], [
            (f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
             f.nloc) for f in result.function_list])
