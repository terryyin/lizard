import unittest

from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import get_reader_for
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_ext.lizardexitcount import LizardExtension as ExitCount
from lizard_ext.lizardgotocount import LizardExtension as GotoCount
from lizard_ext.lizardnd import LizardExtension as NestDepth
from lizard_ext.lizardns import LizardExtension as NestedStructures
from lizard_ext.lizardoutside import LizardExtension as Outside


def analyze(source, extensions=()):
    analyzer = FileAnalyzer(get_extensions(list(extensions)))
    return analyzer.analyze_source_code('page.html.twig', source)


def functions(source):
    return analyze(source).function_list


def summary(source):
    return [(f.name, f.cyclomatic_complexity, f.start_line, f.end_line)
            for f in functions(source)]


def ccn_of(source):
    return dict((f.name, f.cyclomatic_complexity) for f in functions(source))


ORDER_TEMPLATE = '''\
{% extends 'layout.html.twig' %}

{% macro price(amount, currency = 'EUR') %}
    {{ amount is null ? '-' : amount|format_currency(currency) }}
{% endmacro %}

{% block content %}
    {% for item in order.items %}
        {% if item.visible and not item.cancelled %}
            <li>{{ item.name }} {{ _self.price(item.total) }}</li>
        {% elseif loop.first %}
            <li class="empty">...</li>
        {% endif %}
    {% else %}
        <li>No items</li>
    {% endfor %}
{% endblock %}
'''


class TestTwigRegistration(unittest.TestCase):

    def test_twig_extension_selects_the_twig_reader(self):
        reader = get_reader_for('page.html.twig')
        self.assertIsNotNone(reader)
        self.assertEqual('twig', reader.language_names[0])
        self.assertIs(reader, get_reader_for('mail.txt.twig'))
        self.assertIs(reader, get_reader_for('Base.TWIG'))


class TestTwigUnits(unittest.TestCase):

    def test_macros_and_blocks_are_functions(self):
        result = functions(ORDER_TEMPLATE)
        self.assertEqual(
            [('price', 2, 2, 3, 5, 3), ('content', 0, 5, 7, 17, 11)],
            [(f.name, f.parameter_count, f.cyclomatic_complexity,
              f.start_line, f.end_line, f.nloc) for f in result])

    def test_short_blocks_end_with_their_tag(self):
        self.assertEqual(
            [('title', 1, 1, 1), ('heading', 1, 2, 2),
             ('flag', 2, 3, 3), ('body', 1, 4, 4)],
            summary('''\
{% block title 'Home' %}
{% block heading page.title|upper %}
{% block flag a ? 'on' : 'off' %}
{% block body %}{% endblock %}
'''))

    def test_tags_belong_to_the_innermost_block(self):
        self.assertEqual(
            [('header', 3, 3, 7), ('page', 2, 1, 9)],
            summary('''\
{% block page %}
    {% if a %}
        {% block header %}
            {% if b or c %}
                {{ b }}
            {% endif %}
        {% endblock header %}
    {% endif %}
{% endblock page %}
'''))

    def test_blocks_inside_embed_are_functions(self):
        self.assertEqual(
            [('title', 2, 2, 4), ('(template)', 1, 1, 5)],
            summary('''\
{% embed 'card.html.twig' with {size: 'big'} %}
    {% block title %}
        {{ a ?? b }}{{ c ? d }}
    {% endblock %}
{% endembed %}
'''))

    def test_block_and_parent_calls_are_not_blocks(self):
        self.assertEqual(
            [('sidebar', 1, 1, 4)],
            summary('''\
{% block sidebar %}
    {{ parent() }}
    {{ block('menu') }}{{ block('menu', 'other.html.twig') is defined }}
{% endblock %}
'''))

    def test_names_with_non_ascii_characters(self):
        self.assertEqual(
            [('§', 1, 1, 1), ('größe', 2, 2, 2)],
            summary("{% block § %}{% endblock § %}\n"
                    "{% block größe %}{{ ä ? ö : ü }}{% endblock %}\n"))

    def test_macro_parameters(self):
        result = functions('''\
{% macro none() %}{% endmacro %}
{% macro trailing(a, b,) %}{% endmacro %}
{% macro defaults(a, b = {x: 1, y: [1, 2]}, c = f(1, 2), d = 'x') %}{% endmacro %}
{% macro keywords(if, for = 1) %}{% endmacro %}
{% block if %}{% endblock if %}
{% block and %}{% endblock and %}
''')
        self.assertEqual(
            [('none', 0, 1), ('trailing', 2, 1), ('defaults', 4, 1),
             ('keywords', 2, 1), ('if', 0, 1), ('and', 0, 1)],
            [(f.name, f.parameter_count, f.cyclomatic_complexity)
             for f in result])


class TestTwigTemplateBody(unittest.TestCase):

    def test_partial_with_logic_is_one_template_unit(self):
        self.assertEqual([('(template)', 2, 1, 5)], summary('''\
<ul>
{% for x in xs %}
    <li>{{ x }}</li>
{% endfor %}
</ul>
'''))

    def test_child_template_that_only_overrides_blocks_has_no_template_unit(self):
        self.assertEqual([('body', 1, 6, 6)], summary('''\
{% extends 'base.html.twig' %}
{% use 'blocks.html.twig' %}
{% import 'forms.html.twig' as forms %}
{% from 'macros.twig' import input as field %}
{# a comment #}
{% block body %}{{ x }}{% endblock %}
'''))

    def test_layout_with_text_and_blocks_only_has_no_template_unit(self):
        self.assertEqual([('body', 1, 2, 2)], summary('''\
<html>
<body>{% block body %}{% endblock %}</body>
</html>
'''))

    def test_logic_around_blocks_belongs_to_the_template_unit(self):
        self.assertEqual(
            [('main', 1, 3, 3), ('(template)', 2, 1, 5)],
            summary('''\
<html>
{% if user %}
    {% block main %}{% endblock %}
{% endif %}
</html>
'''))

    def test_set_outside_blocks_makes_a_template_unit(self):
        self.assertEqual(
            [('main', 1, 3, 3), ('(template)', 1, 1, 3)],
            summary('''\
{% extends 'base.html.twig' %}
{% set title = 'Orders' %}
{% block main %}{% endblock %}
'''))

    def test_core_tags_outside_blocks_make_a_template_unit(self):
        for tag in ("{% include 'row.twig' %}", "{% do f() %}",
                    "{% with %}{% endwith %}", "{% apply upper %}x{% endapply %}",
                    "{% embed 'e.twig' %}{% endembed %}", "{% verbatim %}{% endverbatim %}"):
            self.assertEqual(['(template)'],
                             [f.name for f in functions(tag)], tag)

    def test_tags_that_twig_does_not_define_make_no_template_unit(self):
        self.assertEqual([], summary('''\
{% sw_extends '@Storefront/base.html.twig' %}
{% hook 'sylius_shop.base' %}
{% trans %}Hello{% endtrans %}
'''))

    def test_empty_and_text_only_templates_have_no_units(self):
        self.assertEqual([], summary(''))
        self.assertEqual([], summary('<p>Hello</p>\n{# note #}\n'))


class TestTwigDecisionPoints(unittest.TestCase):

    def ccn(self, body):
        return ccn_of('{% block b %}' + body + '{% endblock %}')['b']

    def test_ternary_forms(self):
        self.assertEqual(2, self.ccn("{{ a ? b : c }}"))
        self.assertEqual(1, self.ccn("{{ a ? : b }}{{ a ?  : b }}{{ a ?\n: b }}"))
        self.assertEqual(2, self.ccn('{{ a ? "b" : c }}'))
        self.assertEqual(2, self.ccn("{{ a ? b }}"))
        self.assertEqual(1, self.ccn("{{ a ?: b }}"))
        self.assertEqual(1, self.ccn("{{ a ?? b }}"))
        self.assertEqual(1, self.ccn("{{ a?.b }}"))

    def test_word_operators(self):
        self.assertEqual(3, self.ccn("{{ a and b or c }}"))
        self.assertEqual(1, self.ccn(
            "{{ a b-and b }}{{ a b-or b }}{{ a b-xor b }}{{ not a }}"
            "{{ a in b }}{{ a not in b }}{{ a is defined }}"
            "{{ a is not null }}{{ a|default(b) }}"))

    def test_tags(self):
        self.assertEqual(2, self.ccn("{% if a %}x{% else %}y{% endif %}"))
        self.assertEqual(
            4, self.ccn("{% if a %}{% elseif b %}{% elseif c %}{% endif %}"))
        self.assertEqual(
            2, self.ccn("{% for x in xs %}{{ x }}{% else %}-{% endfor %}"))
        self.assertEqual(3, self.ccn("{% for x in xs if x.ok %}{% endfor %}"))

    def test_arrow_functions_count_for_the_enclosing_unit(self):
        result = functions('''\
{% block b %}
    {{ items|filter(v => v.active and v.price > 0)|map((v, k) => k ? v : 0)|join }}
{% endblock %}
''')
        self.assertEqual([('b', 3)],
                         [(f.name, f.cyclomatic_complexity) for f in result])

    def test_keywords_as_keys_and_attributes_add_nothing(self):
        self.assertEqual(1, self.ccn(
            "{{ item.if }}{{ item?.for }}{{ {if: 1, for: 2, and: 3}|length }}"
            "{{ f(if: 1, or: 2) }}"))

    def test_keywords_as_variables_add_nothing(self):
        self.assertEqual(2, self.ccn("{% if for %}{{ if }}{% set else = 1 %}{% endif %}"))
        self.assertEqual(3, self.ccn("{% for x in xs if x %}{% endfor %}"))
        self.assertEqual(2, self.ccn("{% for if in xs %}{{ if }}{% endfor %}"))

    def test_colons_of_hashes_and_named_arguments(self):
        self.assertEqual(2, self.ccn("{{ {a: 1, 'b': c ? d : e} }}"))
        self.assertEqual(2, self.ccn("{{ f(a: b ? 1 : 2, c = 3) }}"))
        self.assertEqual(1, self.ccn("{{ 1..5 }}{{ [1, 2]|merge([...xs]) }}"))

    def test_text_outside_tags_is_not_code(self):
        self.assertEqual(1, self.ccn(
            "<a if='x' and=\"y\">it's { a ? b : c } && d || e for if</a>"
            "<script>if (a) { b() }</script>"))


class TestTwigLexicalForms(unittest.TestCase):

    def ccn(self, body):
        return ccn_of('{% block b %}' + body + '{% endblock %}')['b']

    def test_delimiters_inside_strings(self):
        self.assertEqual(2, self.ccn(
            "{{ '%}' }}{{ \"}}\" }}{% set x = '%} {{ if' %}"
            "{{ 'it\\'s' ~ \"a \\\"q\\\" }}\" }}{% if a %}{% endif %}"))

    def test_interpolation_is_an_expression(self):
        self.assertEqual(3, self.ccn('{{ "a #{b ? c : d} e #{f({g: \'}\'}) ? 1 : 2}" }}'))
        self.assertEqual(1, self.ccn("{{ 'a #{b ? c : d} e' }}"))

    def test_hash_literals_inside_output(self):
        self.assertEqual(3, self.ccn(
            "{{ {a: {b: c ? 1 : 2}} }}{% if d %}{% endif %}"))

    def test_whitespace_control(self):
        self.assertEqual(3, self.ccn(
            "{%- if a -%}{{- b ? c : d -}}{%~ endif ~%}{#- x ? y : z -#}"))

    def test_comments_can_span_lines_and_hold_tags(self):
        self.assertEqual([('b', 2, 1, 6)], summary('''\
{% block b %}
{# {% if a %} {{ b ? c : d }}
   {% block fake %}
#}
    {% if e %}{% endif %}
{% endblock %}
'''))

    def test_inline_comments_run_to_the_end_of_the_line(self):
        self.assertEqual([('b', 3, 1, 5)], summary('''\
{% block b %}
{{ x # }}{% endblock %}
}}
{% if a %}{{ "# no comment" ~ '#' ~ "#{ c ? d : e }" }}{% endif %}
{% endblock %}
'''))
        self.assertEqual([('b', 1, 1, 4)], summary('''\
{% block b %}
{{ x # a ? b : c
}}
{% endblock %}
'''))

    def test_verbatim_and_raw_content_is_text(self):
        self.assertEqual(1, self.ccn(
            "{% verbatim %}{% if a %}{{ b ? c : d }}{% block x %}"
            "{% endverbatim %}"
            "{%- raw -%}{% for x in y %}{% endraw %}"))

    def test_verbatim_opening_tag_on_two_lines(self):
        self.assertEqual(
            [('b', 4, 4), ('c', 7, 7), ('(template)', 1, 7)],
            [(f.name, f.start_line, f.end_line) for f in functions(
                '{%\n verbatim %}x\n{% endverbatim %}\n{% block b "x" %}\n'
                '{%\n raw %}x\n{% endraw %}{% block c "x" %}\n')])

    def test_set_with_a_body(self):
        self.assertEqual(2, self.ccn(
            "{% set x %}{% if a %}y{% endif %}{% endset %}{% set y = 1 %}"))

    def test_tags_that_the_reader_does_not_know(self):
        self.assertEqual(
            [('a', 2, 3, 12)],
            summary('''\
{% sw_extends '@Storefront/base.html.twig' %}
{% form_theme form 'theme.html.twig' %}
{% block a %}
    {% cache 'key' %}{% if b %}{% endif %}{% endcache %}
    {% trans with {'%n%': 1} %}text{% endtrans %}
    {% switch x %}{% case 1 %}one{% default %}other{% endswitch %}
    {% stopwatch 'x' %}{% endstopwatch %}
    {% apply upper %}{% endapply %}{% with {a: 1} %}{% endwith %}
    {% autoescape 'js' %}{% endautoescape %}{% guard function f %}{% endguard %}
    {% types {a: 'int'} %}{% do f() %}{% flush %}{% deprecated 'x' %}
    {% include 'x.twig' ignore missing with {a: 1} only %}{% sandbox %}{% endsandbox %}
{% endblock %}
{% endsw_extends %}
'''))


class TestTwigLinesAndTokens(unittest.TestCase):

    def test_text_lines_count_and_each_text_run_is_one_token(self):
        result = functions('''\
{% block a %}
<div class="x">

  <p>it's "q" {</p>
{{ x }}
{% endblock %}
''')
        self.assertEqual([('a', 5, 10, 1, 6)],
                         [(f.name, f.nloc, f.token_count,
                           f.start_line, f.end_line) for f in result])

    def test_multi_line_tags_keep_line_numbers(self):
        self.assertEqual([('m', 1, 1, 4), ('after', 2, 5, 7)], summary('''\
{% macro m(
    a,
    b = 'x'
) %}{% endmacro %}
{% block after %}
    {% if c %}{% endif %}
{% endblock %}
'''))


class TestTwigDamagedTemplates(unittest.TestCase):

    def test_cut_off_templates_keep_the_units_before_the_damage(self):
        intact = '{% block a %}{% if x %}{{ y }}{% endif %}{% endblock %}\n'
        for tail in ('{% block b %}{% if z', '{{ "abc #{d', "{{ 'abc",
                     '{# abc', '{% verbatim %}{% if', '{% macro m(a, {',
                     '{% block', '{{', '{%', '{% block b %}{{ {a: ['):
            result = functions(intact + tail)
            self.assertEqual(('a', 2), (result[0].name,
                                        result[0].cyclomatic_complexity),
                             tail)

    def test_unbalanced_brackets_end_with_their_tag(self):
        self.assertEqual(
            [('a', 2), ('c', 2), ('d', 1)],
            [(f.name, f.cyclomatic_complexity) for f in functions(
                '{% block a %}{% if (b %}{% endif %}{% endblock %}\n'
                '{% block c %}{{ f(a ? b }}{% endblock %}\n'
                '{% block d %}{{ [1, 2 }}{% endblock %}\n')])

    def test_unbalanced_end_tags(self):
        self.assertEqual(
            [('a', 2, 2, 2)],
            [(f.name, f.cyclomatic_complexity, f.start_line, f.end_line)
             for f in functions(
                 '{% endblock %}{% endmacro %}\n'
                 '{% block a %}{% if b %}{% endif %}{% endmacro %}'
                 '{% endblock %}')
             if f.name != '(template)'])


class TestTwigExtensions(unittest.TestCase):

    NESTED = '''\
{% block a %}
    {% for x in xs %}
        {% if x %}
            {% if y %}{{ y }}{% endif %}
        {% else %}
            {{ z }}
        {% endif %}
    {% endfor %}
{% endblock %}
'''

    def test_nesting_depth_as_for_php(self):
        result = analyze(self.NESTED, [NestDepth()]).function_list
        self.assertEqual([3], [f.max_nesting_depth for f in result])

    def test_nested_structures_as_for_php(self):
        result = analyze(self.NESTED, [NestedStructures()]).function_list
        self.assertEqual([3], [f.max_nested_structures for f in result])

    def test_cognitive_complexity_as_for_php(self):
        result = analyze(self.NESTED, [Cognitive()]).function_list
        self.assertEqual([7], [f.cognitive_complexity for f in result])

    def test_cognitive_complexity_of_conditions(self):
        result = analyze(
            '{% block a %}'
            '{% if a and b and c %}{% elseif d or (e and f) %}{% endif %}'
            '{{ g ? h : i }}'
            '{% endblock %}', [Cognitive()]).function_list
        self.assertEqual([6], [f.cognitive_complexity for f in result])

    def test_twig_has_no_break_or_continue(self):
        result = analyze(
            '{% block a %}'
            '{% set continue = item.break %}'
            '{% if continue %}{{ break }}{% endif %}'
            '{% endblock %}', [Cognitive()]).function_list
        self.assertEqual([1], [f.cognitive_complexity for f in result])

    def test_hash_literals_are_no_blocks(self):
        result = analyze(
            '{% block a %}'
            '{% if a in {one: 1} %}{% if b %}{% endif %}{% endif %}'
            '{% for k, v in {x: {y: 1}} %}{% if v %}{% endif %}{% endfor %}'
            '{% endblock %}', [NestDepth(), NestedStructures(), Cognitive()]
        ).function_list
        self.assertEqual([(2, 2, 6)], [
            (f.max_nesting_depth, f.max_nested_structures,
             f.cognitive_complexity) for f in result])

    def test_keywords_as_names_are_no_structures(self):
        result = analyze(
            '{% block a %}'
            '{{ {if: 1, for: 2, and: 3}|length }}{{ item.if }}'
            '{{ range(if = 1, or = 2) }}{{ for }}'
            '{% endblock %}', [NestDepth(), NestedStructures(), Cognitive()]
        ).function_list
        self.assertEqual([(1, 0, 0, 0)], [
            (f.cyclomatic_complexity, f.max_nesting_depth,
             f.max_nested_structures, f.cognitive_complexity)
            for f in result])

    def test_words_of_other_languages_are_no_structures(self):
        result = analyze(
            '{% block a %}{% if ok %}'
            "{% include 'card.twig' with {x: 1} %}{% do f() %}"
            '{% with {a: 1} %}{{ case }}{{ while }}{{ try }}{% endwith %}'
            '{% endif %}{% endblock %}',
            [NestDepth(), NestedStructures(), Cognitive()]).function_list
        self.assertEqual([(1, 1, 1)], [
            (f.max_nesting_depth, f.max_nested_structures,
             f.cognitive_complexity) for f in result])

    def test_else_of_guard_is_no_branch_of_the_enclosing_if(self):
        result = analyze(
            '{% block a %}{% if ok %}'
            '{% guard function f %}{% else %}{% if x %}y{% endif %}'
            '{% endguard %}{% if z %}{% endif %}'
            '{% endif %}{% endblock %}',
            [NestDepth(), NestedStructures(), Cognitive()]).function_list
        self.assertEqual([(2, 2, 5)], [
            (f.max_nesting_depth, f.max_nested_structures,
             f.cognitive_complexity) for f in result])

    def test_recursive_macro_call_through_self(self):
        result = analyze(
            '{% macro tree(items) %}{% for item in items %}'
            '{{ _self.tree(item.children) }}{% endfor %}{% endmacro %}',
            [Cognitive()]).function_list
        self.assertEqual([2], [f.cognitive_complexity for f in result])

    def test_recursive_macro_call_through_an_import(self):
        result = analyze(
            '{% macro tree(items) %}{% from _self import tree %}'
            '{{ tree(items.children) }}{% endmacro %}',
            [Cognitive()]).function_list
        self.assertEqual([1], [f.cognitive_complexity for f in result])

    def test_functions_and_filters_named_like_the_unit_are_no_recursion(self):
        result = analyze(
            "{% macro default(x) %}{{ x|default('') }}{% endmacro %}"
            '{% block head %}{{ head() }}{% endblock %}',
            [Cognitive()]).function_list
        self.assertEqual([('default', 0), ('head', 0)], [
            (f.name, f.cognitive_complexity) for f in result])

    def test_property_named_return_is_no_exit(self):
        result = analyze(
            '{% block panel %}{% if call.return is defined %}'
            '{{ call.return }}{{ goto }}{% endif %}{% endblock %}',
            [ExitCount(), GotoCount()]).function_list
        self.assertEqual([(1, 0)], [
            (f.exit_count, f.goto_count) for f in result])

    def test_return_tag_of_shopware_scripts_is_an_exit(self):
        result = analyze(
            '{% if a %}{% return 1 %}{% endif %}{% return 2 %}',
            [ExitCount()]).function_list
        self.assertEqual([2], [f.exit_count for f in result])

    def test_outside_code_is_the_template_unit(self):
        def names(source):
            return [f.name for f in analyze(source, [Outside()]).function_list]
        self.assertEqual(['(template)'], names('{{ value }}'))
        self.assertEqual(['b', '(template)'],
                         names('{% block b %}{% endblock %}'))
        self.assertEqual(['c', 'b', '(template)'],
                         names('{% block b %}{% block c %}{{ x }}'))
