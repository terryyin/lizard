import pytest

from lizard import FileAnalyzer, get_extensions


@pytest.mark.parametrize('extensions', [['nd'], ['ns'], ['nd', 'ns']])
def test_statement_extensions_work_across_script_and_markup(extensions):
    source = '<script>if (ready) run()</script>{#if visible}<p>yes</p>{/if}'
    result = FileAnalyzer(get_extensions(extensions)).analyze_source_code(
        'sample.svelte', source)
    component, = result.function_list
    assert component.name == '(component)'
    assert component.cyclomatic_complexity == 3
    if 'nd' in extensions:
        assert component.max_nesting_depth == 1
    if 'ns' in extensions:
        assert component.max_nested_structures == 1
