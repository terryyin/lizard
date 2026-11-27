import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags
from lizard_ext.lizardmodified import LizardExtension as Modified
from .php_helpers import get_php_function_list


def php_metrics(source_code):
    analyzer = FileAnalyzer(get_extensions(
        ['cognitive', 'nd', 'ns', ComplexTags()]))
    return [(f.name, f.cyclomatic_complexity, f.cognitive_complexity,
             f.max_nesting_depth, f.max_nested_structures, f.complex_tags)
            for f in analyzer.analyze_source_code(
                "a.php", source_code).function_list]


class Test_PHP_keyword_as_the_name_of_a_member(unittest.TestCase):
    """Since PHP 7 a keyword can name a member: `$promise->catch($f)` calls
    a method, and `User::class` names a class."""

    def test_keyword_after_a_member_access_adds_no_structure(self):
        self.assertEqual(
            [('CreateTeamsTable::up', 1, 0, 0, 0, []),
             ('CreateTeamsTable::down', 2, 1, 1, 1, [['if', 13]])],
            php_metrics("""<?php
class CreateTeamsTable {
  public function up() {
    $table->string("x")->default("y");
    $user = User::factory()->for($team)->create();
    Bus::batch($jobs)->catch(function ($b, $e) { report($e); })
        ->finally(function ($b) { done(); })->dispatch();
    $query?->match($pattern)->foreach($rows)->while($open);
    $loop->do(self::else, static::switch, $node->case, $node->try);
    $node->elseif->break->continue->goto;
  }
  public function down() {
    if ($this->exists) {
      Schema::drop($this->table)->catch($handler);
    }
  }
}
"""))

    def test_keyword_after_a_member_access_is_no_switch_or_case(self):
        result = FileAnalyzer(get_extensions([Modified()])) \
            .analyze_source_code("a.php", """<?php
function pick($list, $node) {
  return $list->switch($node->case);
}
""")
        self.assertEqual([('pick', 1)],
                         [(f.name, f.cyclomatic_complexity)
                          for f in result.function_list])

    def test_method_named_like_a_keyword(self):
        self.assertEqual(
            [('Promise::catch', 1, 0, 0, 0, []),
             ('Promise::finally', 1, 0, 0, 0, []),
             ('Promise::for', 1, 0, 0, 0, []),
             ('Promise::match', 1, 0, 0, 0, [])],
            php_metrics("""<?php
class Promise {
  public function catch(callable $onRejected) {
    return $this->then(null, $onRejected);
  }
  public function finally(callable $onFulfilledOrRejected) {
    return $this->then($onFulfilledOrRejected);
  }
  public static function for($team) {
    return new static($team);
  }
  public function match($subject) {
    return $this->pattern->matches($subject);
  }
}
"""))

    def test_comparison_after_a_member_named_function(self):
        self.assertEqual(
            [('limit', 3, 2, 0, 0, [['&&', 3], ['match', 3]])],
            php_metrics("""<?php
function limit($options, $x) {
    return $options->/* legacy */function < 1 && $x > match ($x) {
        default => 0,
    };
}
"""))

    def test_class_constant_declares_no_class(self):
        self.assertEqual(
            ['UserFactory::definition'],
            [f.name for f in get_php_function_list("""<?php
class UserFactory extends Factory {
  protected $model = User::class;
  public function definition() {
    return ['name' => $this->faker->name()];
  }
}
""")])

    def test_member_call_outside_a_function_is_no_statement(self):
        result = FileAnalyzer(get_extensions(["outside"])) \
            .analyze_source_code("a.php", """<?php
$app->use(function ($request, $next) {
  return $next($request);
});
$router->match(['GET', 'POST'], '/', $handler);
$routes = ['home' => '/', 'about' => '/about'];
$config->{'app.name'} = 'Laravel';
class Kernel {
  public function handle() {
    return 1;
  }
}
function boot() {
  return 1;
}
""")
        self.assertEqual(
            [('(anonymous)', 1), ('Kernel::handle', 1), ('boot', 1),
             ('*global*', 1)],
            [(f.name, f.cyclomatic_complexity)
             for f in result.function_list])
