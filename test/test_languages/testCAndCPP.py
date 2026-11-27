import unittest
from lizard import analyze_file
from lizard_languages import CLikeReader
from ..testHelpers import get_cpp_fileinfo, get_cpp_function_list, get_cpp_function_list_with_extension

class Test_C_Token_extension(unittest.TestCase):

    def test_connecting_macro(self):
        extended = CLikeReader(None).preprocess(("a##b c", ))
        #tbd

class Test_c_cpp_lizard(unittest.TestCase):

    def test_empty(self):
        result = get_cpp_function_list("")
        self.assertEqual(0, len(result))

    def test_no_function(self):
        result = get_cpp_function_list("#include <stdio.h>\n")
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)

    def test_two_function(self):
        result = get_cpp_function_list("int fun(){}\nint fun1(){}\n")
        self.assertEqual(2, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun1", result[1].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(1, result[0].end_line)
        self.assertEqual(2, result[1].start_line)
        self.assertEqual(2, result[1].end_line)

    def test_two_simplest_function(self):
        result = get_cpp_function_list("f(){}g(){}")
        self.assertEqual(2, len(result))
        self.assertEqual("f", result[0].name)
        self.assertEqual("g", result[1].name)

    def test_function_with_content(self):
        result = get_cpp_function_list("int fun(xx oo){int a; a= call(p1,p2);}")
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( xx oo)", result[0].long_name)

    def test_old_style_c_function(self):
        result = get_cpp_function_list("""int fun(param) int praram; {}""")
        self.assertEqual(1, len(result))

    def test_not_old_style_c_function(self):
        result = get_cpp_function_list("m()"*1500+ "a(){}")
        self.assertEqual(1, len(result))

    def test_complicated_c_function(self):
        result = get_cpp_function_list("""int f(int(*)()){}""")
        self.assertEqual('f', result[0].name)

    def test_function_dec_with_throw(self):
        result = get_cpp_function_list("""int fun() throw();void foo(){}""")
        self.assertEqual(1, len(result))

    def test_function_dec_with_noexcept(self):
        result = get_cpp_function_list("int fun() noexcept(true);void foo(){}")
        self.assertEqual(1, len(result))

    def test_function_dec_followed_with_one_word_is_ok(self):
        result = get_cpp_function_list("""int fun() no_throw {}""")
        self.assertEqual(1, len(result))

    def test_function_declaration_is_not_counted(self):
        result = get_cpp_function_list("""int fun();class A{};""")
        self.assertEqual(0, len(result))

    def test_old_style_c_function_has_semicolon(self):
        result = get_cpp_function_list("""{(void*)a}{}""")
        self.assertEqual(0, len(result))

    def test_typedef_is_not_old_style_c_function(self):
        result = get_cpp_function_list('''typedef T() nT; foo(){}''')
        self.assertEqual("foo", result[0].name)

    def test_stupid_macro_before_function(self):
        result = get_cpp_function_list('''T() foo(){}''')
        self.assertEqual("foo", result[0].name)

    def test_only_word_can_be_function_name(self):
        result = get_cpp_function_list("""[(){}""")
        self.assertEqual(0, len(result))

    def test_double_slash_within_string(self):
        result = get_cpp_function_list("""int fun(){char *a="\\\\";}""")
        self.assertEqual(1, len(result))

    def test_range_for_rvalue_reference(self):
        result = get_cpp_function_list("""int sum(const int (&items)[4]) {
    int total = 0;
    for (auto&& item : items) total += item;
    return total;
}
""")

        self.assertEqual([("sum", 2)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_rvalue_references_in_template_arguments(self):
        result = get_cpp_function_list("""#include <type_traits>
#include <utility>

template <class T>
bool same_reference(T& item, bool ready) {
    return std::is_same_v<T&&, T&&> && ready && std::forward<T&&>(item);
}
""")

        self.assertEqual([("same_reference", 3)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_rvalue_references_and_logical_and_in_lambda_parameters(self):
        result = get_cpp_function_list("""#include <utility>

bool ready();
bool available();

template <class T>
bool scan(T value) {
    auto check = [](int&& number, T&&, bool enabled = ready() && available()) {
        return number != 0 && enabled;
    };
    return check(1, std::move(value));
}
""")

        self.assertEqual([("scan", 3)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_rvalue_reference_pack_and_logical_fold(self):
        result = get_cpp_function_list("""#include <tuple>

template <class... Flags>
bool all_enabled(Flags... flags) {
    auto check = [](auto&&... values) { return (values && ...); };
    return check(flags...);
}

template <class... Items>
bool has_items() {
    return std::tuple_size_v<std::tuple<Items&&...>> != 0;
}
""")

        self.assertEqual(
            [("all_enabled", 2), ("has_items", 1)],
            [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_named_rvalue_reference_in_lambda_parameters(self):
        result = get_cpp_function_list("""struct Widget { int value; };
template <class T> struct Result { T value; };

int read() {
    int factor = 2;
    auto get = [factor](Result<Widget>&& item) { return factor * item.value.value; };
    return get(Result<Widget>{{1}});
}

template <class... Flags>
bool all_enabled(Flags... flags) {
    auto check = []<class... Args>(Args&&... values) { return (values && ...); };
    return check(flags...);
}
""")

        self.assertEqual([("read", 1), ("all_enabled", 2)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_inside_and_after_lambda_parameter_types(self):
        result = get_cpp_function_list("""struct Widget {};
template <bool> struct Policy {};
bool first();
bool second();

bool read() {
    auto check = [](Policy<true && true>, Widget&&, bool enabled = first() && second()) {
        return enabled;
    };
    return check(Policy<true>{}, Widget{}) && second();
}
""")

        self.assertEqual([("read", 4)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_passed_to_a_macro(self):
        result = get_cpp_function_list("""#define APPLY_BINARY(left, op, right) ((left) op (right))
#define APPLY_LAST(left, right, op) ((left) op (right))
#define APPLY_FIRST(op, left, right) ((left) op (right))
#define APPLY_TO_FLAGS(op) (active op ready)

bool ready(bool active, bool ready) {
    return APPLY_BINARY(active, &&, ready) || APPLY_LAST(active, ready, &&) ||
           APPLY_FIRST(&&, active, ready) || APPLY_TO_FLAGS(&&);
}
""")

        self.assertEqual([("ready", 8)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_unnamed_array_rvalue_reference(self):
        result = get_cpp_function_list("""#include <type_traits>

bool check_arrays() {
    return std::is_rvalue_reference_v<int (&&)[2]> && std::is_rvalue_reference_v<const int (&&)[2]>;
}
""")

        self.assertEqual([("check_arrays", 2)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_with_common_operands(self):
        result = get_cpp_function_list("""struct State { bool enabled; };
bool check(bool value);

bool scan(const State *state, bool active, bool ready) {
    bool simple = active && ready;
    bool grouped = active && (ready);
    bool called = check(active) && ready;
    bool member = state->enabled && ready;
    return simple && grouped && called && member &&
           ready;
}
""")

        self.assertEqual([("scan", 9)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_rvalue_reference_with_initializer_cancels_once(self):
        result = get_cpp_function_list("""int fetch();

int scan() {
    int&& value = fetch();
    return value;
}
""")

        self.assertEqual([("scan", 1)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_rvalue_reference_in_ignored_assertion(self):
        result = get_cpp_function_list_with_extension("""int checked() {
    static_assert(sizeof(int&&) == sizeof(int));
    return 0;
}
""", "ignoreassert")

        self.assertEqual([("checked", 1)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_reference_and_logical_and_in_assertion(self):
        source = """int checked() {
    static_assert(sizeof(int&&) == sizeof(int) && true);
    return 0;
}
"""

        for functions, expected in (
            (get_cpp_function_list(source), 2),
            (get_cpp_function_list_with_extension(source, "ignoreassert"), 1),
        ):
            with self.subTest(expected=expected):
                self.assertEqual([("checked", expected)], [(fn.name, fn.cyclomatic_complexity) for fn in functions])

    def test_nonstrict_does_not_deduct_uncounted_references(self):
        result = get_cpp_function_list_with_extension("""int list_initialized() {
    int&& value{0};
    return value;
}

int copy_initialized() {
    int&& value = 0;
    return value;
}

int alias_size() {
    typedef int&& Ref;
    return sizeof(Ref);
}
""", "nonstrict")

        self.assertEqual(
            [("list_initialized", 1), ("copy_initialized", 1), ("alias_size", 1)],
            [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_extension_counts_logical_and_outside_assertion(self):
        source = """bool can_run(bool active, bool ready) {
    static_assert(sizeof(int&&) == sizeof(int));
    return active && ready;
}
"""

        for extension, expected in [("ignoreassert", 2), ("nonstrict", 1)]:
            with self.subTest(extension=extension):
                result = get_cpp_function_list_with_extension(source, extension)
                self.assertEqual([("can_run", expected)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_with_identifier_in_c_header(self):
        result = analyze_file.analyze_source_code("flags.h", """static int scan(int bool, int ready) {
    return bool && ready;
}
""").function_list

        self.assertEqual([("scan", 2)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_after_array_designators_in_gnu_c_header(self):
        result = analyze_file.analyze_source_code("flags.h", """static int scan(int left, int right) {
    enum { first, second };
    int table[2] = { [first] (left && right), [second] (right && left) };
    return table[0] + table[1];
}
""").function_list

        self.assertEqual([("scan", 3)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_inside_block_call_in_objective_c_header(self):
        result = analyze_file.analyze_source_code("validation.h", """typedef int (^Validation)(int);
@protocol Validator
- (Validation)validation;
@end

static inline int check(id<Validator> validator, int active, int ready) {
    return [validator validation](active && ready);
}

typedef struct { int ready; } Result;
typedef Result * (^ResultCallback)(int);
@protocol Provider
- (ResultCallback)result;
@end

static inline int check_result(id<Provider> provider, int active, int ready) {
    return [provider result](active && ready)->ready;
}
""").function_list

        self.assertEqual(
            [("check", 2), ("check_result", 2)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_logical_and_after_block_pointer_member_in_objective_cpp_header(self):
        result = analyze_file.analyze_source_code("validation.h", """struct Result { bool ready; };
typedef Result* (^ResultCallback)(int);
@protocol Provider
- (ResultCallback)result;
@end
struct Flag {
    bool ready;
    explicit operator bool() const { return ready; }
};
static inline bool check_result(id<Provider> provider, int input, bool enabled) {
    return [provider result](input)->ready && Flag{enabled};
}
template<class T>
bool has_result(id<Provider> provider, int input) {
    return [provider result](input)->ready && requires { typename T::result_type; };
}
template<class T>
bool has_result_with_parameter(id<Provider> provider, int input) {
    return [provider result](input)->ready && requires(T value) { value.result(); };
}
template<class T>
bool has_indexed_result(id<Provider>* providers, int input) {
    return [providers[0] result](input)->ready && requires(T value) { value.result(); };
}
""").function_list

        names = ("check_result", "has_result", "has_result_with_parameter", "has_indexed_result")
        self.assertEqual(
            [(name, 2) for name in names],
            [(fn.name, fn.cyclomatic_complexity) for fn in result if fn.name in names])

    def test_named_lambda_references_in_callback_argument_in_cpp_header(self):
        result = analyze_file.analyze_source_code("jobs.h", """struct Job { int weight; };

int run_jobs() {
    auto submit = [](int base, auto finish) { return finish(Job{base}, Job{base}); };
    return submit(7, [weight = 2](Job&& first, Job&& second) mutable {
        return weight * (first.weight + second.weight);
    });
}
""").function_list

        self.assertEqual([("run_jobs", 1)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_lambda_reference_return_keeps_body_decisions(self):
        result = get_cpp_function_list("""struct Job {};
Job&& relay(Job& job, bool ready, bool enabled) {
    auto forward = [](Job& pending, bool ready, bool enabled) -> Job&& {
        if (ready && enabled) return static_cast<Job&&>(pending);
        return static_cast<Job&&>(pending);
    };
    return forward(job, ready, enabled);
}
""")

        self.assertEqual([("relay", 3)], [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_named_lambda_references_with_declarator_suffixes(self):
        result = get_cpp_function_list("""#include <vector>
struct Job { int weight; };

std::vector<int> weights() {
    return [](Job&& job) -> std::vector<int> { return {job.weight}; }(Job{1});
}

int weight() {
    constexpr bool safe = true;
    return [](Job&& job) noexcept(noexcept(job.weight) && safe) { return job.weight; }(Job{1});
}

int constrained_weight() {
    return []<class T>(T&& job) requires requires { job.weight; } {
        return job.weight;
    }(Job{1});
}
""")

        self.assertEqual(
            [("weights", 1), ("weight", 2), ("constrained_weight", 1)],
            [(fn.name, fn.cyclomatic_complexity) for fn in result])

    def test_number_with_thousands_separator_since_cpp14(self):
        result = get_cpp_function_list("""int fun(){
            int a= 100'000; if(b) c; return 123'456'789;
        }""")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_hex_number_with_thousands_separator_since_cpp14(self):
        result = get_cpp_function_list("""int fun(){
            int a= 0x12ab'34cd; if(b) c; return 0xEF56'7890'1A2B;
        }""")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_bin_number_with_thousands_separator_since_cpp14(self):
        result = get_cpp_function_list("""int fun(){
            int a= 0b0101'1100; if(b) c; return 0b1111'0000'1100'1110;
        }""")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_function_with_no_param_omitted(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(0, result[0].parameter_count)

    def test_function_with_no_param_void(self):
        result = get_cpp_function_list("int fun(void){}")
        self.assertEqual(0, result[0].parameter_count)

    def test_function_with_1_param(self):
        result = get_cpp_function_list("int fun(aa bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(["bb"], result[0].parameters)

    def test_function_with_1_ref_param(self):
        result = get_cpp_function_list("int fun(aa * bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(["bb"], result[0].parameters)

    def test_function_with_param(self):
        result = get_cpp_function_list("int fun(aa * bb, cc dd){}")
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_strang_param(self):
        result = get_cpp_function_list("int fun(aa<mm, nn> bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual("fun( aa<mm,nn> bb)", result[0].long_name)

    def test_function_with_strang_param2(self):
        result = get_cpp_function_list("int fun(aa<x<mm,(x, y)>, nn> bb, (cc)<xx, oo> d){}")
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_namespace(self):
        result = get_cpp_function_list("int abc::fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun()", result[0].long_name)

    def test_one_function_with_const(self):
        result = get_cpp_function_list("int abc::fun()const{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun() const", result[0].long_name)

    def test_one_function_with_throw(self):
        result = get_cpp_function_list("""int fun() throw() {}""")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)
        result = get_cpp_function_list("""int fun() throw(Exception) {}""")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)

    def test_one_function_with_noexcept(self):
        result = get_cpp_function_list("int abc::fun()noexcept{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        result = get_cpp_function_list("int fun() noexcept(true) {}")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)
        result = get_cpp_function_list(
                "int fun() noexcept(noexcept(foo()) && noexcept(Bar())) {}")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)

    def test_two_functions_in_class(self):
        result = get_cpp_function_list("class c {~c(){}}; int d(){}")
        self.assertEqual(2, len(result))
        self.assertEqual("c::~c", result[0].name)
        self.assertEqual("d", result[1].name)

    def test_one_macro_in_class(self):
        result = get_cpp_function_list("class c {M()}; int d(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("d", result[0].name)

    def test_pre_class(self):
        result = get_cpp_function_list("class c; int d(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("d", result[0].name)

    def test_class_with_inheritance(self):
        result = get_cpp_function_list("class c final:public b {int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

    def test_nested_class(self):
        result = get_cpp_function_list("class c {class d {int f(){}};};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::d::f", result[0].name)

    def test_template_class(self):
        result = get_cpp_function_list("template<typename T> class c {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<class T> class c {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<typename T> class c {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T, typename S>"
                                       "class c {void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<class T>"
                                       "class c {void f(T t) {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_template_class_full_specialization(self):
        result = get_cpp_function_list("template<> class c<double> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<> class c<double> {"
                                       "void f() {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<>"
                                       "class c<double, int> {void f() {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<>"
                                       "class c<double> {void f() {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_template_class_partial_specialization(self):
        result = get_cpp_function_list(
                "template<typename T> class c<int,T> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<class T> class c<int,T> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<typename T> class c<int,T> {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c<int,T> {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T, typename S>"
                                       "class c<int,T,S> {void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<class T>"
                                       "class c<int,T> {void f(T t) {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_template_function(self):
        result = get_cpp_function_list("template<typename T> void f(T t) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        result = get_cpp_function_list("template<class T> void f(T t) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        result = get_cpp_function_list("namespace ns {"
                                       "template<class T> void f(T t) {}}")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::f", result[0].name)

    def test_template_function_specialization(self):
        result = get_cpp_function_list("template<> void f<double>() {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f<double>", result[0].name)
        result = get_cpp_function_list("namespace ns {"
                                       "template<> void f<double>() {}}")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::f<double>", result[0].name)

    def test_nested_template_function(self):
        result = get_cpp_function_list("template<typename T> class c { "
                                       "template<typename S> void f() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c { "
                                       "template<class S> void f() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { "
                                       "template<class T> class c { "
                                       "template<class S> void f() {} }; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_templated_code_with_question_mark(self):
        result = get_cpp_function_list("void a(){Class<?>[];}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_class_as_an_attribute(self):
        result = get_cpp_function_list("void a(){{String.class}}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_1(self):
        result = get_cpp_function_list("class c {{}}")
        self.assertEqual(0, len(result))

    def test_bracket_that_is_not_a_namespace(self):
        result = get_cpp_function_list("class c { {};int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

    def test_nested_class_middle(self):
        result = get_cpp_function_list("class c {class d {};int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

    def test_template_as_reference(self):
        result = get_cpp_function_list("abc::def(a<b>& c){}")
        self.assertEqual(1, len(result))

    def test_less_then_is_not_template(self):
        result = get_cpp_function_list("def(<); foo(){}")
        self.assertEqual(1, len(result))

    def test_template_with_pointer(self):
        result = get_cpp_function_list("abc::def (a<b*> c){}")
        self.assertEqual(1, len(result))

    def test_nested_template(self):
        result = get_cpp_function_list("abc::def (a<b<c>> c){}")
        self.assertEqual(1, len(result))

    def test_double_nested_template(self):
        result = get_cpp_function_list("abc::def (a<b<c<d>>> c){}")
        self.assertEqual(1, len(result))

    def test_template_with_reference(self):
        result = get_cpp_function_list("void fun(t<int &>b){} ")
        self.assertEqual(1, len(result))

    def test_template_with_reference_as_reference(self):
        result = get_cpp_function_list("void fun(t<const int&>&b){} ")
        self.assertEqual(1, len(result))

    def test_template_as_part_of_function_name(self):
        result = get_cpp_function_list("void fun<a,b<c>>(){} ")
        self.assertEqual('fun<a,b<c>>', result[0].name)

    def test_operator_overloading(self):
        result = get_cpp_function_list("bool operator +=(int b){}")
        self.assertEqual("operator +=", result[0].name)

    def test_operator_overloading_shift(self):
        result = get_cpp_function_list("bool operator <<(int b){}")
        self.assertEqual("operator < <", result[0].name)

    def test_operator_with_complicated_name(self):
        result = get_cpp_function_list("operator MyStruct&(){}")
        self.assertEqual("operator MyStruct &", result[0].name)

    def test_operator_overloading_with_namespace(self):
        result = get_cpp_function_list("bool TC::operator !(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator !", result[0].name)

    def test_function_operator(self):
        result = get_cpp_function_list("bool TC::operator ()(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator ( )", result[0].name)

    def test_inline_operator(self):
        result = get_cpp_function_list("class A { bool operator ()(int b) {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::operator ( )", result[0].name)

    def test_namespace_alias(self):
        result = get_cpp_function_list(
                "namespace p;"
                "namespace real { bool foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("real::foo", result[0].name)

    def test_nested_unnamed_namespace(self):
        result = get_cpp_function_list(
                "namespace real { namespace { bool foo() {} } }")
        self.assertEqual(1, len(result))
        self.assertEqual("real::foo", result[0].name)

    def test_constructor_initialization_list(self):
        result = get_cpp_function_list('''A::A():a(1){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_constructor_initialization_list_noexcept(self):
        result = get_cpp_function_list('''A::A()noexcept:a(1){}''')

    def test_constructor_initializer_list(self):
        result = get_cpp_function_list('''A::A():a({1}),value(true){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_constructor_uniform_initialization(self):
        result = get_cpp_function_list('''A::A():a{1}{}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_parentheses_before_function(self):
        result = get_cpp_function_list('''()''')
        self.assertEqual(0, len(result))

    def test_destructor_implementation(self):
        result = get_cpp_function_list('''A::~A(){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::~A", result[0].name)

    def test_function_that_returns_function_pointers(self):
        result = get_cpp_function_list('''int (*fun())(){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("int( * fun())", result[0].name)

    def test_struct_in_return_type(self):
        result = get_cpp_function_list(''' struct a b() { a(1>2); }''')
        self.assertEqual(1, len(result))
        self.assertEqual("b", result[0].name)

    def test_function_name_class(self):
        result = get_cpp_function_list('''int class(){}''');
        self.assertEqual(1, len(result))

    def test_underscore(self):
        from lizard_languages.code_reader import CodeReader
        generate_tokens = CodeReader.generate_tokens
        result = get_cpp_function_list(''' a() _() { }''')
        self.assertEqual(1, len(result))

    def test_global_var_constructor(self):
        result = get_cpp_function_list('''std::string s("String");''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::string s = "String";''')
        self.assertEqual(0, len(result))

    def test_non_function_initializer_list(self):
        result = get_cpp_function_list('''v={}''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''v = {};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::vector<int> v = {1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''v = {1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''namespace n { v = {}; }''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''class n { int v = {0}; }''')
        self.assertEqual(0, len(result))

    def test_non_function_uniform_initialization(self):
        result = get_cpp_function_list('''std::vector<int> v{1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::vector<int> v{};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''namespace n { int v{0}; }''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''class n { int v{0}; }''')
        self.assertEqual(0, len(result))

    def test_struct_in_param(self):
        result = get_cpp_function_list('''int fun(struct a){}''')
        self.assertEqual(1, len(result))

    def test_trailing_return_type(self):
        """C++11 trailing return type for functions."""
        result = get_cpp_function_list("auto foo() -> void {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("auto foo(int a) -> decltype(a) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_ref_qualifiers(self):
        """C++11 ref qualifiers for member functions."""
        result = get_cpp_function_list("struct A { void foo() & {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() const & {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() && {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() const && {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

    def test_union_as_qualifier(self):
        """Union as namespace for functions."""
        result = get_cpp_function_list("union A { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

    def test_aggregate_initialization_with_double(self):
        result = get_cpp_function_list('''
            struct Bar {
                double value;
            };

            void foo() {
                Bar bar = {1.};
                double x = 1.0;
                double y = .5;
                double z = 1.e-10;
            }

            void bar() {}
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual("bar", result[1].name)

class Test_cpp11_Attributes(unittest.TestCase):
    """C++11 extendable attributes can appear pretty much anywhere."""

    def test_namespace(self):
        result = get_cpp_function_list(
            "namespace [[visibility(hidden)]] ns { void foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::foo", result[0].name)
        result = get_cpp_function_list(
            "namespace ns [[deprecated]] { void foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::foo", result[0].name)

    def test_class(self):
        result = get_cpp_function_list(
            "struct [[alignas(8)]] A { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list(
            "struct A [[deprecated]] { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

    def test_function(self):
        result = get_cpp_function_list("void foo() [[noreturn]] {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_function_parameters(self):
        result = get_cpp_function_list("void foo(int a [[unused]]) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("void foo(int a [[unused]], int b) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("void foo(int b, int a [[unused]]) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_function_return_type(self):
        result = get_cpp_function_list(
            "int [[warn_unused_result]] foo(int a) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_control_structures(self):
        result = get_cpp_function_list(
            "int foo() { [[likely(true)]] if (a) return 1; else return 2; }")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
        result = get_cpp_function_list(
            """int foo() {
                 for [[omp::parallel()]] (int i{}; i < n; ++i)
                   sum += i; }""")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)


class Test_Preprocessing(unittest.TestCase):

    def test_content_macro_should_be_ignored(self):
        result = get_cpp_function_list(r'''
                    #define MTP_CHEC                    \
                       int foo () {                     \
                        }
               ''')
        self.assertEqual(0, len(result))

    def test_preprocessors_should_be_ignored_outside_function_implementation(self):
        result = get_cpp_function_list('''
                      #ifdef MAGIC
                      #endif
                      void foo()
                      {}
                    ''')
        self.assertEqual(1, len(result))

    def test_preprocessor_is_not_function(self):
        result = get_cpp_function_list('''
                #ifdef A
                #elif (defined E)
                #endif
                ''')
        self.assertEqual(0, len(result))

    def test_body_with_function_like(self):
        '''in the following example 'xws' is a macro defined somewhere else'''
        result = get_cpp_function_list("""int a() { xws (a) if(){} }""")
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_body_with_macro_call_after_if(self):
        result = get_cpp_function_list("""int a() { if (a) b(){} }""")
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_body_with_macro_call_after_if_and_no_semicolon_before_the_closing_br(self):
        result = get_cpp_function_list("""int a() { if (a) b() } int c(){}""")
        self.assertEqual(2, len(result))
        self.assertEqual('c', result[1].name)

    def test_body_with_macro_call_after_if_and_no_semicolon_before_the_closing_br2(self):
        result = get_cpp_function_list("""int a() { if (a) if(x) b() } int c(){}""")
        self.assertEqual(2, len(result))
        self.assertEqual('c', result[1].name)

    def test_body_with_macro_and_class(self):
        result = get_cpp_function_list("""class A{int a() { if (a) if(x) b() } int c(){}}""")
        self.assertEqual(2, len(result))
        self.assertEqual('A::c', result[1].name)

    def test_body_with_function_like2(self):
        '''in the following example 'b' is a macro defined somewhere else'''
        result = get_cpp_function_list("""
void myFunction()
{
  IGNORE_FLAGS("w-maybe")
  if(2+2==4)
  END_IGNORE_FLAGS("w-maybe")
  {
    mySecondFunction()
  }
}

int mySecondFunction()
{
  return 2;
}
                """)
        self.assertEqual(2, len(result))
        self.assertEqual('mySecondFunction', result[1].name)

    def test_macro_continuation_with_empty_line_issue_439(self):
        """Test issue from PR #439: macro continuation followed by empty line.
        
        This test verifies that line counting is correct when a macro definition
        uses a continuation marker (\\) followed by an empty line before a function.
        """
        # Using explicit newlines to ensure an empty line exists after the macro
        code = "#define  SQ(x) ((x)*(x)) \\\n\nbool foo(status *pstat)\n{\n    return true;\n}\n"
        result = get_cpp_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('foo', result[0].name)
        # The function should start at line 3 (after #define line 1 and empty line 2)
        # and end at line 6 (the closing brace)
        self.assertEqual(3, result[0].start_line, 
                        "Function should start at line 3 after macro and empty line")
        self.assertEqual(6, result[0].end_line,
                        "Function should end at line 6")


class Test_Big(unittest.TestCase):

    def test_trouble(self):
        code = "foo<y () >> 5> r;"
        result = get_cpp_function_list(code)
        self.assertEqual(0, len(result))

    def test_typedef(self):
        code = """
        typedef struct tagAAA
        {
        }AAA;

        int func_a(int size)
        {
            if(ccc && eee)
            {
                return 1;
            }
        }
        """
        result = get_cpp_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)


class Test_Dialects(unittest.TestCase):

    def test_cuda_kernel_launch(self):
        """Special triple < and > for Nvidia CUDA C/C++ code."""
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, 0 >>> (d_data, height, width);
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, (bar ? 0 : 1) >>> (x, y, z);
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, 0 >>> (x, y, (bar ? w : z));
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_lambda_expression_issue_429(self):
        """Test lambda expression parsing issue from GitHub issue #429."""
        # This is the exact code from the issue that causes problems
        code = """
auto CombPairHash = [](const CombPair &pair) {
  return pair.first ^ (pair.second<<1); };

int main() {
    return 0;
}
"""
        result = get_cpp_function_list(code)
        # Should find the main function, lambda should not be treated as a function
        self.assertEqual(1, len(result))
        self.assertEqual("main", result[0].name)

    def test_lambda_expression_with_capture(self):
        """Test lambda expression with capture list."""
        code = """
auto lambda = [&](int x) { return x * 2; };

void foo() {
    auto result = lambda(5);
}
"""
        result = get_cpp_function_list(code)
        # Should find the foo function, lambda should not be treated as a function
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_lambda_expression_in_function(self):
        """Test lambda expression inside a function."""
        code = """
void process() {
    auto lambda = [](int x) { return x + 1; };
    int result = lambda(10);
}
"""
        result = get_cpp_function_list(code)
        # Should find the process function
        self.assertEqual(1, len(result))
        self.assertEqual("process", result[0].name)

    def test_multiple_functions_with_static_cast_issue_443(self):
        """Test multiple void functions with multiple static_cast calls (issue #443)."""
        # This reproduces a bug where lizard.analyze would freeze when iterating
        # through the result map when analyzing files with multiple void functions
        # containing multiple static_cast<...> calls
        code = """
void func1() {
    int a = static_cast<int>(1.5);
    double b = static_cast<double>(2);
    long c = static_cast<long>(3.0);
}

void func2() {
    int x = static_cast<int>(4.5);
    double y = static_cast<double>(5);
    long z = static_cast<long>(6.0);
}

void func3() {
    int p = static_cast<int>(7.5);
    double q = static_cast<double>(8);
    long r = static_cast<long>(9.0);
}
"""
        result = get_cpp_function_list(code)
        # Should find all three functions without freezing
        self.assertEqual(3, len(result))
        self.assertEqual("func1", result[0].name)
        self.assertEqual("func2", result[1].name)
        self.assertEqual("func3", result[2].name)


class Test_cpp_raw_string_literals(unittest.TestCase):
    """Issue #478: C++ raw string literals must not corrupt function boundaries."""

    _RAW_PATTERNS = (
        '    const auto r_pattern{pattern0};\n'
        '    const auto e_pattern{pattern1};\n',
        '    const auto r_pattern = pattern0;\n'
        '    const auto e_pattern = pattern1;\n',
    )
    _PATTERN0 = 'R"(\\b\\w*R\\w*\\b)"'
    _PATTERN1 = 'R"(<Blah>(\\d+)</Blah>)"'

    def _source_with_raw_strings(self, body_lines):
        return (
            'auto process()\n'
            '{\n'
            + body_lines.format(pattern0=self._PATTERN0, pattern1=self._PATTERN1)
            + '}\n'
            '\n'
            'int main()\n'
            '{\n'
            '    auto pat = "{}";\n'
            '}\n')

    def _assert_process_and_main(self, result):
        self.assertEqual(2, len(result))
        self.assertEqual("process", result[0].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(5, result[0].end_line)
        self.assertEqual("main", result[1].name)
        self.assertEqual(7, result[1].start_line)
        self.assertEqual(10, result[1].end_line)

    def test_raw_string_init_keeps_following_function(self):
        for name, body_lines in (
                ('brace', self._RAW_PATTERNS[0]),
                ('equals', self._RAW_PATTERNS[1]),
        ):
            with self.subTest(init=name):
                result = get_cpp_function_list(
                    self._source_with_raw_strings(body_lines))
                self._assert_process_and_main(result)

    def test_single_raw_string_in_function_keeps_following_function(self):
        result = get_cpp_function_list(
            'auto process()\n'
            '{\n'
            '    const auto r_pattern{R"(\\b\\w*R\\w*\\b)"};\n'
            '}\n'
            '\n'
            'int main() { return 0; }\n')
        self.assertEqual(2, len(result))
        self.assertEqual("process", result[0].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(4, result[0].end_line)
        self.assertEqual("main", result[1].name)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(6, result[1].end_line)

    def test_adjacent_raw_string_literals_tokenize_separately(self):
        src = (
            'const auto a{R"(\\b\\w*R\\w*\\b)"};\n'
            '    const auto b{R"(<Blah>(\\d+)</Blah>)"};')
        raw_tokens = [
            t for t in CLikeReader.generate_tokens(src)
            if t.startswith('R"(') and t.endswith(')"')]
        self.assertEqual(
            ['R"(\\b\\w*R\\w*\\b)"', 'R"(<Blah>(\\d+)</Blah>)"'],
            raw_tokens)

    def test_raw_string_with_encoding_prefix(self):
        result = get_cpp_function_list(
            'int a()\n'
            '{\n'
            '    auto w = LR"(wide)";\n'
            '    auto u = u8R"(utf8)";\n'
            '}\n'
            'int b()\n'
            '{\n'
            '    return 0;\n'
            '}\n')
        self.assertEqual(2, len(result))
        self.assertEqual("a", result[0].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(5, result[0].end_line)
        self.assertEqual("b", result[1].name)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(9, result[1].end_line)

    def test_custom_delimiter_raw_string_keeps_following_function(self):
        # Custom delimiters are not matched by the raw-string pattern; they fall
        # through to the normal string rule. That is fine as long as they carry
        # no embedded double quote, which is the overwhelmingly common case
        # (JSON/HTML bodies with balanced braces). Lock that boundary here.
        result = get_cpp_function_list(
            'int a()\n'
            '{\n'
            '    auto j = R"json({"x": [1, 2]})json";\n'
            '}\n'
            'int b()\n'
            '{\n'
            '    return 0;\n'
            '}\n')
        self.assertEqual(2, len(result))
        self.assertEqual("a", result[0].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(4, result[0].end_line)
        self.assertEqual("b", result[1].name)
        self.assertEqual(5, result[1].start_line)
        self.assertEqual(8, result[1].end_line)
