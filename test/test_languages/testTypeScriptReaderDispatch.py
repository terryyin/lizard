"""Reader extensions retain their hooks while TypeScript grammar stays real."""
from types import MethodType
import multiprocessing
from pathlib import Path

import pytest

from lizard import FileInfoBuilder, analyze_files, get_extensions
from lizard_languages.javascript_bindings import JavaScriptBindings
from lizard_languages.typescript import TypeScriptReader
from lizard_languages.typescript_states import TypeScriptStates


SOURCE = 'const selected = x => x || fallback'


class ObservedReader(TypeScriptReader):
    def __init__(self, context):
        super().__init__(context)
        self.finished = None

    def eof(self):
        self.finished = [(function.name, function.cyclomatic_complexity)
                         for function in self.context.fileinfo.function_list]


class DecisionReader(ObservedReader):
    def process_token(self, token):
        if token == 'fallback':
            self.context.add_condition()
        return token == 'fallback'


class InheritedDecisionReader(DecisionReader):
    pass


class PropertyDecisionReader(DecisionReader):
    @property
    def process_token(self):
        return super().process_token


class DecisionCounter:
    """An additional reader state supplies a decision-word extension."""
    def __init__(self, context):
        self.context = context
        self.finished = None

    def __call__(self, token):
        if token == 'fallback':
            self.context.add_condition()

    def statemachine_before_return(self):
        self.finished = [function.name for function in self.context.fileinfo.function_list]


def read(reader_class=ObservedReader, configure=None, source=SOURCE):
    context = FileInfoBuilder('dispatch.ts')
    reader = reader_class(context)
    if configure:
        configure(reader)
    tokens = reader.generate_tokens(source)
    for processor in get_extensions([]):
        tokens = processor(tokens, reader)
    emitted = list(reader(tokens, reader))
    return context.fileinfo, reader, emitted


def test_default_reader_finalizes_expression_before_eof():
    info, reader, emitted = read()
    assert reader.finished == [('selected', 2)]
    assert [(function.name, function.parameter_count, function.start_line,
             function.end_line) for function in info.function_list] == [('selected', 1, 1, 1)]
    assert emitted == ['const', 'selected', '=', 'x', '=>', 'x', '||', 'fallback']
    assert info.token_count == len(emitted)


@pytest.mark.parametrize('reader_class', [DecisionReader, InheritedDecisionReader,
                                        PropertyDecisionReader], ids=lambda reader: reader.__name__)
def test_reader_decision_hook_changes_public_ccn(reader_class):
    info, reader, _ = read(reader_class)
    assert [(function.name, function.cyclomatic_complexity)
            for function in info.function_list] == [('selected', 3)]
    assert reader.finished == [('selected', 3)]


@pytest.mark.parametrize('bound', [False, True])
def test_instance_decision_hook_changes_public_ccn(bound):
    def configure(reader):
        if bound:
            reader.process_token = MethodType(DecisionReader.process_token, reader)
        else:
            def hook(token):
                return DecisionReader.process_token(reader, token)
            reader.process_token = hook
    info, reader, _ = read(configure=configure)
    assert [(function.name, function.cyclomatic_complexity)
            for function in info.function_list] == [('selected', 3)]
    assert reader.finished == [('selected', 3)]


def test_parallel_decision_state_and_finalization_are_preserved():
    def configure(reader):
        reader.parallel_states.append(DecisionCounter(reader.context))
    info, reader, _ = read(configure=configure)
    assert [(function.name, function.cyclomatic_complexity)
            for function in info.function_list] == [('selected', 3)]
    assert reader.parallel_states[1].finished == ['selected']
    assert reader.finished == [('selected', 3)]


def test_configured_root_keeps_binding_owner_tracking():
    binding = JavaScriptBindings(')')

    def configure(reader):
        reader.parallel_states[0]._binding_candidate = binding
    info, reader, _ = read(configure=configure)
    assert [(function.name, function.cyclomatic_complexity,
             function.parameter_count) for function in info.function_list] == [('selected', 2, 1)]
    assert binding.defaults == 1
    assert binding.previous == 'fallback'
    assert reader.finished == [('selected', 2)]


def test_configured_state_reads_quoted_property_arrows():
    class ConfiguredState(TypeScriptStates):
        pass

    def configure(reader):
        reader.parallel_states[0] = ConfiguredState(reader.context)

    source = '''declare const enabled: boolean;
const hooks = {
  'event:ready': (count: number) => count,
  'event:submit': (enabled
    ? (item: string): string => item
    : (item: string) => item),
};'''

    info, _, _ = read(configure=configure, source=source)

    assert [(f.start_line, f.parameters) for f in info.function_list] == [
        (3, ['count']), (5, ['item']), (6, ['item']),
    ]


def test_configured_token_hook_normalizes_quoted_and_ordinary_formals():
    class ConfiguredState(TypeScriptStates):
        def __call__(self, token, reader=None):
            if token == 'input' and not isinstance(self._state, TypeScriptStates):
                token = 'normalized'
            return super().__call__(token, reader)

    def configure(reader):
        reader.parallel_states[0] = ConfiguredState(reader.context)

    source = '''const hooks = {
  'event:ready': (input: number) => input,
  submit: (input: number) => input,
};'''

    info, _, _ = read(configure=configure, source=source)

    assert [(f.start_line, f.parameters) for f in info.function_list] == [
        (2, ['normalized']), (3, ['normalized']),
    ]


class StateIterable:
    def __init__(self, states):
        self.states = states

    def __iter__(self):
        return iter(self.states)

    def __getitem__(self, index):
        return self.states[index]


def test_generic_iterable_state_configuration_retains_decisions():
    def configure(reader):
        reader.parallel_states = StateIterable([
            reader.parallel_states[0], DecisionCounter(reader.context)])
    info, reader, _ = read(configure=configure)
    assert [(function.name, function.cyclomatic_complexity)
            for function in info.function_list] == [('selected', 3)]
    assert reader.finished == [('selected', 3)]


def test_empty_state_configuration_yields_tokens_and_finishes():
    context = FileInfoBuilder('empty.ts')
    reader = ObservedReader(context)
    reader.parallel_states = []
    tokens = TypeScriptReader.generate_tokens('value;')
    assert list(reader(tokens, reader)) == ['value', ';']
    assert reader.finished == []


def test_hook_failure_propagates_without_eof():
    def configure(reader):
        def hook(token):
            if token == 'fallback':
                raise ValueError('decision extension failed')
        reader.process_token = hook
    context = FileInfoBuilder('failed.ts')
    reader = ObservedReader(context)
    configure(reader)
    tokens = reader.generate_tokens(SOURCE)
    for processor in get_extensions([]):
        tokens = processor(tokens, reader)
    with pytest.raises(ValueError, match='decision extension failed'):
        list(reader(tokens, reader))
    assert reader.finished is None


def test_public_parallel_analysis_preserves_function_results(tmp_path):
    files = [tmp_path / name for name in ('left.js', 'right.ts')]
    for path in files:
        path.write_text(SOURCE)
    children = set(multiprocessing.active_children())
    results = list(analyze_files([str(path) for path in files], threads=2))
    assert sorted((Path(info.filename).name, function.name,
                   function.cyclomatic_complexity, function.parameter_count)
                  for info in results for function in info.function_list) == [
        ('left.js', 'selected', 2, 1), ('right.ts', 'selected', 2, 1)]
    assert set(multiprocessing.active_children()) == children
