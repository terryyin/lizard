import unittest
import inspect
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_go_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.go", source_code).function_list


class Test_parser_for_Go(unittest.TestCase):

    def test_empty(self):
        functions = get_go_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_go_function_list('''
        for name, ok := range names; ok {
                print("Hello, \\(name)!")
            }
                ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_go_function_list('''
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_with_parameter(self):
        result = get_go_function_list('''
            func sayGoodbye(personName string, alreadyGreeted chan bool) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_return_value(self):
        result = get_go_function_list('''
            func sayGoodbye() string { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_one_function_with_two_return_values(self):
        result = get_go_function_list('''
            func sayGoodbye(p int) (string, error) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_one_function_defined_on_a_struct(self):
        result = get_go_function_list('''
            func (s Stru) sayGoodbye(){ }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual("(s Stru)sayGoodbye", result[0].long_name)

    def test_one_function_with_complexity(self):
        result = get_go_function_list('''
            func sayGoodbye() { if ++diceRoll == 7 { diceRoll = 1 }}
                ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_one_function_with_return_empty_interface(self):
        result = get_go_function_list('''
            func sayGoodbye() interface{} {
                if ++diceRoll == 7 { diceRoll = 1 }
            }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(3, result[0].length)

    def test_nest_function(self):
        result = get_go_function_list('''
            func sayGoodbye() {
                f1 := func() {}
                f2 := func(n int) {}
                f3 := func() int {
                    return 0
                }
            }
                ''')
        self.assertEqual(4, len(result))

        self.assertEqual("", result[0].name)
        self.assertEqual("", result[0].long_name)
        self.assertEqual(1, result[0].length)

        self.assertEqual("", result[1].name)
        self.assertEqual(" n int", result[1].long_name)
        self.assertEqual(1, result[1].length)
        self.assertEqual(['n int'], result[1].full_parameters)

        self.assertEqual("", result[2].name)
        self.assertEqual("", result[2].long_name)
        self.assertEqual(3, result[2].length)

        self.assertEqual("sayGoodbye", result[3].name)
        self.assertEqual(7, result[3].length)

    def test_interface(self):
        result = get_go_function_list('''
			type geometry interface{
					 area()  float64
					 perim()  float64
			 }
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_followed_by_a_class(self):
        result = get_go_function_list('''
			type geometry interface{
					 area()  float64
					 perim()  float64
			 }
            class c { }
                ''')
        self.assertEqual(0, len(result))

    def test_struct_with_func_followed_by_function_with_receiver(self):
        result = get_go_function_list('''
            type Geometry struct {
                isEqual func(float64, float64) error
            }

            func (g *Geometry) sayGoodbye() { }
                ''')

        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_with_func_followed_by_function_with_receiver(self):
        result = get_go_function_list('''
            type MyComparator struct{}

            type Comparator interface {
                Handle(func(int) string)
            }

            func (m MyComparator) Handle(f func(int) string) {}
                ''')

        self.assertEqual(1, len(result))
        self.assertEqual("Handle", result[0].name)

    def test_sql_query_with_question_marks(self):
        result = get_go_function_list('''
            func getQuery(dbIndex uint32, tbIndex uint32) string {
                query := fmt.Sprintf(`INSERT INTO online_docs_%d.online_docs_notify_%d
                (a, b, c, d, e, f, g, h, i, j)
                VALUES (?, ?, ?, ?, ?, ?, ?, FROM_UNIXTIME(?), ?, %d)`,
                dbIndex, tbIndex, notifyStatusNew)
                return query
            }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("getQuery", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_generic_function_with_type_param(self):
        result = get_go_function_list('''
            func Map[T any](x T) T { return x }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("Map", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_generic_function_with_multiple_type_params(self):
        result = get_go_function_list('''
            func Reduce[T any, U any](xs []T, acc U) U { return acc }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("Reduce", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_generic_function_with_nested_slice_constraint(self):
        result = get_go_function_list('''
            func Clone[S ~[]E, E any](s S) S { return s }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("Clone", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_generic_method_with_receiver(self):
        result = get_go_function_list('''
            func (r *Box) Get[T any](x T) T { return x }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("Get", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_package_level_function_literal_is_named_by_var(self):
        result = get_go_function_list('''
            var handler = func(a int) { if a > 0 { } }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].parameter_count)

    def test_package_level_typed_function_var(self):
        result = get_go_function_list('''
            var handler func(int) error = func(a int) error { return nil }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])

    def test_function_type_var_without_initializer(self):
        result = get_go_function_list('''
            var handler func(int) error
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_function_literal_in_package_level_composite(self):
        result = get_go_function_list('''
            var table = []Handler{ func() { }, }
            func after() { }
                ''')
        self.assertEqual(["", "after"], [f.name for f in result])

    def test_bare_func_literal_does_not_swallow_following_functions(self):
        result = get_go_function_list('''
            var x = func
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_type_switch_does_not_end_function_early(self):
        result = get_go_function_list('''
            func f(x interface{}) {
                switch v := x.(type) {
                case int:
                    if v > 0 { }
                }
                if true { }
            }
            func g() { }
                ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_func_type_in_composite_literal_type(self):
        result = get_go_function_list('''
            var table = map[string]func(){ "a": func() { if true { } }, }
            var list = []func() error{ func() error { return nil } }
            func after() {
                hs := make([]func(), 0)
                var c chan func()
                if true { }
            }
                ''')
        self.assertEqual(["", "", "after"], [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[2].cyclomatic_complexity)
