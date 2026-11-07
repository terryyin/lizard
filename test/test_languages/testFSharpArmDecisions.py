import unittest

from .fsharp_helpers import by_name, get_fsharp_function_list


class TestFSharpArmDecisions(unittest.TestCase):

    def test_match_arms_guards_and_elif(self):
        classify, = get_fsharp_function_list('''\
let classify x =
    match x with
    | 0 | 1 -> "small"
    | n when n < 0 -> "neg"
    | n -> if n > 9 && n < 100 then "mid" elif n = 5 then "five" else "big"
''')
        self.assertEqual(('classify', 7),
                         (classify.name, classify.cyclomatic_complexity))

    def test_or_patterns_add_nothing(self):
        f, = get_fsharp_function_list('''\
let f x =
    match x with
    | 0 | 1 | 2 -> "small"
    | _ -> "big"
''')
        self.assertEqual(2, f.cyclomatic_complexity)

    def test_function_arms_on_one_line(self):
        f, = get_fsharp_function_list(
            'let f = function Some x -> x | None -> 0\n')
        self.assertEqual(('f', 2, 0),
                         (f.name, f.cyclomatic_complexity, f.parameter_count))

    def test_pipelines_and_bitwise_bars_are_not_arms(self):
        g, = get_fsharp_function_list('''\
let g a b =
    match a with
    | true -> [a; b] |> List.map not |> List.head
    | false -> (1 ||| 2) = 3 || not b
''')
        self.assertEqual(3, g.cyclomatic_complexity)

    def test_other_pipelines_add_nothing(self):
        f, = get_fsharp_function_list('''\
let f xs ys =
    (xs, ys) ||> List.map2 (+) |> ignore
    printfn "%d" <| List.length xs
''')
        self.assertEqual(1, f.cyclomatic_complexity)

    def test_try_handler_arms_each_add_one(self):
        functions = by_name('''\
let read k =
    try
        find k
    with
    | :? System.ArgumentException -> None
    | Failure _ -> None
let safe k =
    try
        find k
    finally
        close ()
''')
        self.assertEqual(3, functions['read'].cyclomatic_complexity)
        self.assertEqual(1, functions['safe'].cyclomatic_complexity)

    def test_single_try_handler_adds_one(self):
        f, = get_fsharp_function_list(
            'let f k = try find k with e -> None\n')
        self.assertEqual(2, f.cyclomatic_complexity)

    def test_constraint_when_and_type_arrows_add_nothing(self):
        functions = by_name('''\
let f<'T when 'T : comparison> (x: 'T) : 'T = x
let apply (g: int -> int) x =
    let h : int -> int = fun y -> y + 1
    let inner<'U when 'U : equality> (u: 'U) = u
    xs |> List.map (fun v -> g v) |> List.map h
''')
        self.assertEqual((1, 1), (functions['f'].cyclomatic_complexity,
                                  functions['f'].parameter_count))
        self.assertEqual((1, 2), (functions['apply'].cyclomatic_complexity,
                                  functions['apply'].parameter_count))

    def test_record_copy_with_and_type_tests_add_nothing(self):
        functions = by_name('''\
let bump r =
    let r2 = { r with X = 1 }
    let r3 = {| r2 with Y = 2 |}
    if (box r3) :? string then 1 else 0
let pick r =
    match { r with X = 1 } with
    | { X = 1 } -> 1
    | _ -> 0
''')
        self.assertEqual(2, functions['bump'].cyclomatic_complexity)
        self.assertEqual(2, functions['pick'].cyclomatic_complexity)

    def test_then_else_do_add_nothing(self):
        f, = get_fsharp_function_list('''\
let f a =
    if a then () else ()
    for i in 1 .. 3 do printfn "%d" i
''')
        self.assertEqual(3, f.cyclomatic_complexity)

    def test_with_of_augmentation_and_auto_property_starts_no_arms(self):
        functions = by_name('''\
type Point with
    member p.Norm = 1
type Person() =
    member val Name = "" with get, set
let after x =
    match x with
    | 0 -> 0
    | _ -> 1
''')
        self.assertEqual(['after'], sorted(functions))
        self.assertEqual(2, functions['after'].cyclomatic_complexity)

    def test_match_bang_in_a_computation_expression(self):
        run, = get_fsharp_function_list('''\
let run xs = async {
    match! fetch xs with
    | Some v -> return v
    | None -> return 0 }
''')
        self.assertEqual(('run', 2), (run.name, run.cyclomatic_complexity))

    def test_nested_match_in_a_lambda_counts_its_own_arms(self):
        f, = get_fsharp_function_list('''\
let f xs =
    match xs with
    | [] -> 0
    | _ -> xs |> List.sumBy (function Some v -> v | None -> 0)
''')
        self.assertEqual(3, f.cyclomatic_complexity)
