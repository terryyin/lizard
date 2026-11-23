import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

import lizard
from lizard import analyze_file
from lizard_languages import get_reader_for


def get_groovy_function_list(source_code, filename="a.groovy"):
    return analyze_file.analyze_source_code(
        filename, source_code).function_list


def summaries(functions):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions]


def language_of(filename):
    reader = get_reader_for(filename)
    return reader and reader.language_names[0]


def ccn_of_method(body):
    functions = get_groovy_function_list(
        "class A {\n  def f(x) {\n" + body + "\n  }\n}\n")
    return [f.cyclomatic_complexity for f in functions]


class TestGroovyFiles(unittest.TestCase):

    def test_groovy_extensions_and_jenkinsfile(self):
        for name in ("a.groovy", "a.gvy", "a.gy", "a.gsh", "build.gradle",
                     "Jenkinsfile", "ci/Jenkinsfile"):
            self.assertEqual('groovy', language_of(name), name)

    def test_other_names_are_not_groovy(self):
        for name in ("Jenkinsfile.txt", "MyJenkinsfile", "a.gradle.kts"):
            self.assertNotEqual('groovy', language_of(name), name)

    def test_directory_scan_includes_groovy_gradle_and_jenkinsfile(self):
        with TemporaryDirectory() as directory:
            for name in ("call.groovy", "build.gradle", "Jenkinsfile"):
                Path(directory, name).write_text(
                    "def %s(Map args) {\n  echo 'x'\n}\n" % name.replace('.', '_'))
            result = lizard.analyze([directory], lans=['groovy'])
            names = sorted(Path(info.filename).name for info in result)
        self.assertEqual(["Jenkinsfile", "build.gradle", "call.groovy"], names)


class TestGroovyFunctions(unittest.TestCase):

    def test_class_methods_with_and_without_def(self):
        functions = get_groovy_function_list('''\
class OrderService {
    def total(List items, BigDecimal discount = 0) {
        def sum = items.findAll { it.active && it.price > 0 }.sum { it.price } ?: 0
        if (discount) {
            sum -= sum * discount
        }
        return sum
    }

    def "rejects an empty order"() {
        expect:
        service.total([]) == 0
    }
}
''')
        self.assertEqual([('OrderService::total', 2, 8, 4),
                          ('OrderService::"rejects an empty order"', 10, 13, 1)],
                         summaries(functions))
        self.assertEqual([2, 0], [f.parameter_count for f in functions])

    def test_typed_methods_with_modifiers_generics_and_throws(self):
        functions = get_groovy_function_list('''\
@CompileStatic
abstract class Repo<T> implements Closeable {
    @Override
    public static <K> Map<K, List<T>> group(Map<String, List<T>> rows, int... sizes)
            throws IOException, IllegalStateException {
        rows
    }
    protected String[] names() { [] }
    private void 'single quoted'(String a = 'x', b) {}
    abstract void close()
    synchronized boolean ready()
    {
        true
    }
}
''')
        self.assertEqual([('Repo::group', 4, 7), ('Repo::names', 8, 8),
                          ('Repo::\'single quoted\'', 9, 9), ('Repo::ready', 11, 14)],
                         [(f.name, f.start_line, f.end_line) for f in functions])
        self.assertEqual([2, 0, 2, 0], [f.parameter_count for f in functions])

    def test_default_values_with_operators_and_generic_types(self):
        functions = get_groovy_function_list('''\
def first(xs, x = xs?[0]) {}
def compare(boolean less = count < limit, int other = 0) {}
def build(Map<String, List<Integer>> rows = new HashMap<String, List<Integer>>(), int size) {}
def limit(boolean small = MIN_SIZE < 2, int other = 0) {}
def flag(x = enabled ?: false, y = a && b) {}
def pick(x = N < 2 ? 3 : 4, y = 5) {}
def pickClosure = { x = N < 2 ? 3 : 4, y = 5 -> x }
''')
        self.assertEqual([('first', 2, 1), ('compare', 2, 1), ('build', 2, 1),
                          ('limit', 2, 1), ('flag', 2, 3), ('pick', 2, 2), ('pickClosure', 2, 2)],
                         [(f.name, f.parameter_count, f.cyclomatic_complexity) for f in functions])

    def test_annotated_methods_without_a_type(self):
        functions = get_groovy_function_list('''\
class Renderer {
    @Autowired
    setMessageSource(List<MessageSource> sources) {
        messageSource = sources.first()
    }
    @Override // keep in sync
    grab(String module) {
        grab(module: module)
    }
}
''')
        self.assertEqual([('Renderer::setMessageSource', 3, 5), ('Renderer::grab', 7, 9)],
                         [(f.name, f.start_line, f.end_line) for f in functions])

    def test_methods_after_a_bodiless_method_with_throws(self):
        functions = get_groovy_function_list('''\
trait TokenStorage {
    abstract UserDetails load(String token) throws TokenNotFoundException

    void store(String token, UserDetails principal) {}
}
''')
        self.assertEqual([('TokenStorage::store', 4, 4)],
                         [(f.name, f.start_line, f.end_line) for f in functions])

    def test_constructors_interfaces_traits_and_enums(self):
        functions = get_groovy_function_list('''\
class Point {
    Point(int x) { this.x = x }
    public Point() { this(0) }
}
interface Shape {
    double area()
    default String label() { 'shape' }
}
trait Named {
    String greet() { "hi $name" }
}
enum Color {
    RED(1) {
        String hex() { '#f00' }
    },
    GREEN(2),
    BLUE {
        @Override String hex() { '#00f' }
    }
    static { init() }
    Color(int code) { this.code = code }
    String hex() { '' }
}
''')
        self.assertEqual(['Point::Point', 'Point::Point', 'Shape::label', 'Named::greet',
                          '(anonymous)::hex', '(anonymous)::hex', 'Color::Color', 'Color::hex'],
                         [f.name for f in functions])

    def test_script_methods_and_top_level_code(self):
        functions = get_groovy_function_list('''\
#!/usr/bin/env groovy
package ci.tools
import static java.lang.Math.max
import java.util.concurrent.TimeUnit as TU

def call(Map config = [:], Closure body) {
    if (config.skip) {
        return
    }
    body()
}

if (args && args[0] == 'x') {
    call(skip: true) {}
}
String describe(int n) { n > 1 ? 'many' : 'one' }
''')
        self.assertEqual([('call', 6, 11, 2), ('describe', 16, 16, 2)],
                         summaries(functions))
        self.assertEqual([2, 1], [f.parameter_count for f in functions])

    def test_closure_variables_and_fields_are_functions(self):
        functions = get_groovy_function_list('''\
def retry = { int times, Closure action ->
    for (i in 1..times) {
        if (action()) return true
    }
    false
}
def noArgs = {
    println 'x'
}
class Book {
    static constraints = {
        title blank: false, validator: { it ? null : 'empty' }
    }
    def onClick = { e -> e.ok && e.visible }
}
''')
        self.assertEqual([('retry', 1, 6, 3), ('noArgs', 7, 9, 1),
                          ('Book::constraints', 11, 13, 2), ('Book::onClick', 14, 14, 2)],
                         summaries(functions))
        self.assertEqual([2, 0, 0, 1], [f.parameter_count for f in functions])

    def test_closure_parameters_with_defaults_and_generic_types(self):
        functions = get_groovy_function_list('''\
def step = { n = 1, List<Integer> seen = [0, 1] -> n }
def byKey = { Map<String, List<?>> rows, String key -> rows[key] }
def lookup = { key = items[0], limit = count < 2 ? 3 : 4 -> key }
def none = { a = 1; a }
def guarded = { action = {}, Map options = [:] -> action() }
''')
        self.assertEqual([('step', 2), ('byKey', 2), ('lookup', 2), ('none', 0), ('guarded', 2)],
                         [(f.name, f.parameter_count) for f in functions])
        self.assertEqual([1, 1, 2, 1, 1], [f.cyclomatic_complexity for f in functions])

    def test_a_closure_that_starts_a_field_initializer_is_a_function(self):
        functions = get_groovy_function_list('''\
class Matchers {
    static final Closure<Boolean> PREFIX =
            { String s, String p -> s.startsWith(p) } as Closure<Boolean>
    final namer = { it?.name ?: 'none' } as Namer
    def cached = { compute() }.memoize()
    String path = {
        reload ? reloadDir : baseDir
    }()
}
''')
        self.assertEqual([('Matchers::PREFIX', 3, 3, 1), ('Matchers::namer', 4, 4, 2),
                          ('Matchers::cached', 5, 5, 1), ('Matchers::path', 6, 8, 2)],
                         summaries(functions))

    def test_closures_in_a_method_belong_to_the_method(self):
        functions = get_groovy_function_list('''\
class A {
    def check(items) {
        def isValid = { item -> item && item.size() > 0 }
        items.each { if (it) println it }
        items.collect(x -> x ?: 0)
    }
}
''')
        self.assertEqual([('A::check', 2, 6, 4)], summaries(functions))

    def test_calls_with_trailing_closures_are_not_functions(self):
        functions = get_groovy_function_list('''\
plugins { id 'java' }
task clean(type: Delete) {
    if (project.hasProperty('x')) { delete buildDir }
}
tasks.register('docs', Copy) { from 'src' }
dependencies {
    implementation(libs.guava) { exclude group: 'x' }
}
node('linux') {
    stage('Build') {
        steps { sh 'make' }
    }
    withEnv(['A=1']) { foo(a) { } }
}
println total(items) { it }
''', filename="build.gradle")
        self.assertEqual([], summaries(functions))

    def test_property_declarations_are_not_methods(self):
        functions = get_groovy_function_list('''\
class Config {
    String name = compute()
    List<String> tags = ['a', 'b'].collect { it.toUpperCase() }
    final Map<String, Integer> sizes = [:]
    def (a, b) = [1, 2]
    int size() { sizes.size() }
}
''')
        self.assertEqual([('Config::size', 6, 6, 1)], summaries(functions))

    def test_annotations_with_arguments_and_closures(self):
        functions = get_groovy_function_list('''\
class Spec {
    @Requires({ os.linux && jvm.java17Compatible })
    @Timeout(value = 5, unit = TimeUnit.SECONDS)
    def "runs on linux"() {
        expect: true
    }
}
''')
        self.assertEqual([('Spec::"runs on linux"', 4, 6, 1)], summaries(functions))

    def test_inner_anonymous_and_local_classes(self):
        functions = get_groovy_function_list('''\
class Outer {
    static class Inner {
        void m() { if (a) {} }
    }
    def make() {
        if (b) {}
        def r = new Runnable() {
            void run() { while (c) {} }
        }
        class Local { int n() { d ? 1 : 2 } }
        def s = new Supplier<String[]>() {
            String[] get() { e ? [] : null }
        }
        def a = new int[3]
        return r
    }
    void after() {}
}
''')
        self.assertEqual([('Inner::m', 3, 3, 2), ('(anonymous)::run', 8, 8, 2),
                          ('Local::n', 10, 10, 2), ('(anonymous)::get', 12, 12, 2),
                          ('Outer::make', 5, 16, 2), ('Outer::after', 17, 17, 1)],
                         summaries(functions))

    def test_initializer_blocks_are_not_functions(self):
        functions = get_groovy_function_list('''\
class A {
    static { if (x) init() }
    { if (y) setup() }
    void run() {}
}
''')
        self.assertEqual([('A::run', 4, 4, 1)], summaries(functions))

    def test_records_and_contextual_keywords(self):
        functions = get_groovy_function_list('''\
record Point(int x, int y) {
    int sum() { x + y }
    void record(String trait) {}
}
def save(record) {
    def trait = record.trait
    records.each { record -> record.save() }
    for (record in records) { if (record) {} }
}
''')
        self.assertEqual([('Point::sum', 2, 2, 1), ('Point::record', 3, 3, 1),
                          ('save', 5, 9, 3)],
                         summaries(functions))


class TestGroovyDecisions(unittest.TestCase):

    def test_branches_loops_and_catches(self):
        self.assertEqual([9], ccn_of_method('''
            if (x) {} else if (y) {}
            for (int i = 0; i < 3; i++) {}
            for (a in list) {}
            for (a : list) {}
            while (x) {}
            do { x-- } while (x > 0)
            try { f() } catch (IOException | SQLException e) {} finally {}
            assert x != null'''))

    def test_switch_cases(self):
        self.assertEqual([5], ccn_of_method('''
            switch (x) {
                case 1..5: break
                case [7, 8]: break
                case String: break
                case ~/a+/: break
                default: break
            }'''))

    def test_switch_with_arrow_cases(self):
        self.assertEqual([3], ccn_of_method('''
            def y = switch (x) {
                case 1, 2 -> 'low'
                case 3 -> { 'mid' }
                default -> 'high'
            }'''))

    def test_logical_ternary_and_elvis_operators(self):
        self.assertEqual([8], ccn_of_method('''
            def a = x && y || z
            def b = x ? 1 : 2
            def c = x ?: 'default'
            x ?= 5
            def d = x ? (y ?: 1) : 0'''))

    def test_operators_that_do_not_count(self):
        self.assertEqual([1], ccn_of_method('''
            def a = x?.y?.z?.@w
            def b = list?[0]
            def c = list*.name + map.collect(this.&f) + list.collect(String::valueOf)
            def d = (1..10) + (0..<5) + (x <=> y) + (s =~ /a/) + (s ==~ /b/)
            def e = 2 ** 3 + (8 >>> 1) + (x as int)
            def f = x in list
            def g = x !in list
            def h = x instanceof String
            def i = x !instanceof Integer
            def j = { a, b -> a <=> b }
            def k = (a) -> a'''))

    def test_generic_wildcards_are_not_ternaries(self):
        self.assertEqual([1], ccn_of_method('''
            List<?> a = []
            Map<String, ? extends Number> b = [:]'''))

    def test_generic_wildcards_in_parameter_types_are_not_ternaries(self):
        functions = get_groovy_function_list('''\
void addAll(Provider<? extends Iterable<T>> provider, Action<? super T> action) {}
def configure = { Action<? super Task> action -> action }
def merge = { Map<? extends List<String>, String> rows -> rows }
def apply = { Map<String, ? extends Number> options -> options }
void wrap(A<String, Integer> pair) {}
''')
        self.assertEqual([('addAll', 1, 1, 1, 2), ('configure', 2, 2, 1, 1), ('merge', 3, 3, 1, 1),
                          ('apply', 4, 4, 1, 1), ('wrap', 5, 5, 1, 1)],
                         [summary + (f.parameter_count,)
                          for summary, f in zip(summaries(functions), functions)])


class TestGroovyStrings(unittest.TestCase):

    def test_string_contents_do_not_count(self):
        self.assertEqual([1], ccn_of_method('''
            def a = 'if (a && b) { c ? d : e }'
            def b = "while (x || y) { }"
            def c = \'\'\'case 1: for (;;) { ' " \'\'\'
            def d = """catch (e) { ' " } && ||"""
            def e = /if (a) { b } \\/ c ? d/
            def f = $/while { x } / y $$ $/ ? /$
            def g = "escaped \\" if && \\${not} || code"'''))

    def test_interpolated_expressions_count(self):
        self.assertEqual([5], ccn_of_method('''
            def a = "total: ${x ? x : 0} and $name.size ${'nested ' + (y && z)}"
            def b = """multi ${ items.findAll { it.ok }.size() > 0 ? 'some' : "none" }
                       line"""
            def c = /slashy ${ m ?: n } text/'''))

    def test_interpolation_with_braces_quotes_and_closures(self):
        functions = get_groovy_function_list('''\
class A {
    def label(items) {
        "${items.collect { "${it.name}: ${it.ok ? '}' : "{"}" }.join(', ')}"
    }
    def after() { if (a) {} }
}
''')
        self.assertEqual([('A::label', 2, 4, 2), ('A::after', 5, 5, 2)],
                         summaries(functions))

    def test_multiline_strings_keep_line_numbers(self):
        functions = get_groovy_function_list('''\
def a() {
    """
    one ${ x }
    two
    """
}
def b() {
    \'\'\'
    three
    \'\'\'
}
def c() { $/
    four
/$ }
''')
        self.assertEqual([('a', 1, 6, 1), ('b', 7, 11, 1), ('c', 12, 14, 1)],
                         summaries(functions))

    def test_slashy_strings_and_division(self):
        functions = get_groovy_function_list('''\
def ratio(a, b) {
    def half = a / 2 / b
    def parts = (a / b) / (b / a)
    def m = a =~ /it's {/ && b ==~ /\\d+/
    if (half > 1 && [/'/, /"/].size() / 2 > 0) {}
}
def next() { if (x) {} }
''')
        self.assertEqual([('ratio', 1, 6, 4), ('next', 7, 7, 2)],
                         summaries(functions))

    def test_slashy_string_at_a_line_start(self):
        functions = get_groovy_function_list('''\
class RlikeSpec extends Specification {
    def "matches a pattern"() {
        expect:
        Pet.findAllByNameRlike(search) == expected
        where:
        search | expected
        'foo'  | ['foo']
        /f\\w+/ | ['foo', 'foobar']
    }
    def total(a, b) {
        (a
            / b)
    }
}
''')
        self.assertEqual([('RlikeSpec::"matches a pattern"', 2, 9, 1),
                          ('RlikeSpec::total', 10, 13, 1)],
                         summaries(functions))

    def test_a_slash_without_a_closing_slash_divides(self):
        functions = get_groovy_function_list('''\
def ratio() {
    9
    / 3
}
def other() {}
''')
        self.assertEqual([('ratio', 1, 4, 1), ('other', 5, 5, 1)], summaries(functions))

    def test_line_comment_ending_with_backslash(self):
        functions = get_groovy_function_list('''\
// copied to D:\\
def a() { if (x) {} }

// D:\\

def b() {}
''')
        self.assertEqual([('a', 2, 2, 2), ('b', 6, 6, 1)], summaries(functions))

    def test_comments(self):
        functions = get_groovy_function_list('''\
// def notAFunction() { if (x) {} }
/* def neither() {
   if (y) {} } */
/** Doc with 'apostrophe' and "quote */
def real() { // if (z) {}
    true
}
''')
        self.assertEqual([('real', 5, 7, 1)], summaries(functions))


class TestGroovyNames(unittest.TestCase):

    def test_keywords_as_member_and_map_names(self):
        functions = get_groovy_function_list('''\
class A {
    def f(x) {
        def a = x.class.name + foo.default + x?.for
        def b = [if: 1, for: 2, while: 3, case: 4, class: 5]
        obj.'quoted name'()
        call(catch: true, in: list)
        x.class
    }
    def g() { if (y) {} }
}
''')
        self.assertEqual([('A::f', 2, 8, 1), ('A::g', 9, 9, 2)],
                         summaries(functions))

    def test_dollar_in_names(self):
        functions = get_groovy_function_list('''\
class A {
    static $static_methodMissing(String name, args) { name }
    def run$0() { this$0 ? 1 : 2 }
}
''')
        self.assertEqual([('A::$static_methodMissing', 2, 2, 1), ('A::run$0', 3, 3, 2)],
                         summaries(functions))

    def test_labels_and_colons(self):
        self.assertEqual([4], ccn_of_method('''
            outer:
            for (a in list) {
                given:
                def m = [a: 1, (k): v, 'x': [b: 2]]
                foo a: 1, b: 2
                if (m) continue outer
            }
            def t = a ? b : c'''))

    def test_spock_data_table_columns_add_nothing(self):
        functions = get_groovy_function_list('''\
class MathSpec extends Specification {
    def "max of #a and #b"() {
        expect:
        Math.max(a, b) == c

        where:
        a | b || c
        1 | 3 || 3
        7 | 4 || 7
        x = a > 0 || b > 0
        n << [true || false]
        y =
            a < 0 || b < 0
    }
    def other() { a || b }
}
''')
        self.assertEqual([('MathSpec::"max of #a and #b"', 2, 14, 4),
                          ('MathSpec::other', 15, 15, 2)],
                         summaries(functions))

    def test_a_statement_that_continues_on_the_next_line_is_no_table_row(self):
        functions = get_groovy_function_list('''\
class DerivedSpec extends Specification {
    def "derived"() {
        expect: selected
        where:
        selected = false &&
            true || true
        other =
            // a fallback
            false || true
        third = /* a
        */ false || true
        input = true
            || false
    }
    def "rows"() {
        expect: a == b
        where:
        a || b
        1 || 1; /* two
                   lines */ 2 || 2
        a ; fn
        false ||
            true ; { ->
            false || true
        }
        true || true; }; def after() { if (true) 1 }
}
''')
        self.assertEqual([('DerivedSpec::"derived"', 2, 14, 6),
                          ('DerivedSpec::"rows"', 15, 26, 3),
                          ('DerivedSpec::after', 26, 26, 2)],
                         summaries(functions))

    def test_comments_in_a_data_table(self):
        self.assertEqual([1], ccn_of_method('''
            expect:
            a == b
            where /* rows */ :
            a /* || */ | b
            // filter:
            1 /* || */ || 1'''))


    def test_spock_where_block_ends_at_filter_and_semicolon_tables_count(self):
        functions = get_groovy_function_list('''\
class TableSpec extends Specification {
    def "filters rows"() {
        expect:
        a == b
        where:
        a | b
        [1]?[x || y ? 0 : 1] | 1
        filter:
        a == 1 || a == 2
    }
    def "reads a semicolon table"() {
        expect:
        input == result
        where:
        input ; result
        true || false ; true
    }
}
''')
        self.assertEqual([('TableSpec::"filters rows"', 2, 10, 4),
                          ('TableSpec::"reads a semicolon table"', 11, 17, 2)],
                         summaries(functions))

class TestGroovyExtensions(unittest.TestCase):

    def test_nesting_depth_and_nested_structures(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
class A {
    def f(x) {
        if (x) {
            x.each {
                while (it) { it-- }
            }
        }
    }
}
'''
        result = FileAnalyzer(get_extensions(["nd", "ns"])).analyze_source_code(
            "a.groovy", code)
        function = result.function_list[0]
        self.assertEqual(("A::f", 2, 2),
                         (function.name, function.max_nesting_depth,
                          function.max_nested_structures))

    def test_tables_with_semicolon_columns_and_borders(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
class BorderSpec extends Specification {
    def "mixes table styles"() {
        expect:
        a + b == c
        where: a ; b ;; c
        1 ; 2 ;; 3
        true || false ; 1 ;; 1
        __
        ; d | f || g ;
        ; 1 | 2 || 3 ;
        "${true || false}" | 'true' || x
        "${flag = true}" | 'true' || x
        ({ ->
            filter:
            true || false
        }) || true
    }
}
'''
        result = FileAnalyzer(get_extensions(["cognitive"])).analyze_source_code(
            "a.groovy", code)
        function = result.function_list[0]
        self.assertEqual((4, 3), (function.cyclomatic_complexity, function.cognitive_complexity))

    def test_data_table_columns_add_no_nesting_or_cognitive_complexity(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
class MathSpec extends Specification {
    def "max"() {
        expect:
        Math.max(a, b) == c
        where:
        a | b || c
        1 | 3 || 3
        7 | 4 || 7
    }
}
'''
        result = FileAnalyzer(get_extensions(["nd", "cognitive"])).analyze_source_code(
            "a.groovy", code)
        function = result.function_list[0]
        self.assertEqual((1, 0, 0),
                         (function.cyclomatic_complexity, function.max_nesting_depth,
                          function.cognitive_complexity))

    def test_a_line_end_ends_a_statement_for_nesting(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
class Sw {
    int count(String kind, List xs) {
        def label = kind == 'a' ? 'first' : 'other'
        switch (kind) {
            case 'a': println label
            default: println 'none'
        }
        for (String s : xs)
            if (s != null) total++
        return total
    }
}
'''
        result = FileAnalyzer(get_extensions(["nd", "ns"])).analyze_source_code(
            "a.groovy", code)
        function = result.function_list[0]
        self.assertEqual(("Sw::count", 2, 2),
                         (function.name, function.max_nesting_depth,
                          function.max_nested_structures))

    def test_a_braceless_body_on_the_next_line_nests(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
def check(List xs) {
    xs.each { x ->
        if (x.ready)
            println x
        def y = x.size ?: 0
        if (y) { println y }
    }
    def a = xs ?: []
    new Report(a).empty ? warn() : print()
    def all = xs.every { it.ready } &&
        xs.size() > 2
}
'''
        result = FileAnalyzer(get_extensions(["nd", "ns"])).analyze_source_code(
            "a.groovy", code)
        function = result.function_list[0]
        self.assertEqual(("check", 1, 1),
                         (function.name, function.max_nesting_depth,
                          function.max_nested_structures))

    def test_each_nesting_extension_reads_the_statement_ends(self):
        from lizard import FileAnalyzer, get_extensions
        code = '''\
int count(List xs) {
    for (String s : xs)
        if (s != null) total++
    if (total) println total
    return total
}
'''
        for extensions in (["nd"], ["ns"], ["nd", "ns"], ["ns", "nd"]):
            with self.subTest(extensions=extensions):
                function = FileAnalyzer(get_extensions(extensions)) \
                    .analyze_source_code("a.groovy", code).function_list[0]
                if "nd" in extensions:
                    self.assertEqual(2, function.max_nesting_depth)
                if "ns" in extensions:
                    self.assertEqual(2, function.max_nested_structures)
