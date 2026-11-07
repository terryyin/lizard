import multiprocessing
from pathlib import Path
import sys
import time

import pytest

from lizard import analyze_files, get_extensions


def wait_for_file(path):
    deadline = time.monotonic() + 10
    while not path.exists():
        if time.monotonic() >= deadline:
            raise TimeoutError('Worker barrier timed out: ' + str(path))
        time.sleep(0.01)


class WorkerBarrier:
    """Keep one real worker busy until it is cancelled or released."""

    def __init__(self, directory, failure=None):
        self.directory = directory
        self.failure = failure

    def __call__(self, tokens, reader):
        directory = Path(self.directory)
        if Path(reader.context.fileinfo.filename).name == 'slow.c':
            (directory / 'started').touch()
            try:
                wait_for_file(directory / 'release')
            finally:
                (directory / 'completed').touch()
        else:
            wait_for_file(directory / 'started')
            with (directory / 'fast_calls').open('a') as calls:
                calls.write('called\n')
            if self.failure:
                raise self.failure('cannot analyze fast.c')
        yield from tokens


class FailingCrossFileExtension:
    def __call__(self, tokens, reader):
        yield from tokens

    def cross_file_process(self, fileinfos):
        for fileinfo in fileinfos:
            if fileinfo.filename.endswith('fast.c'):
                raise ValueError('cannot combine fast.c')
            yield fileinfo


@pytest.fixture
def worker_files(tmp_path):
    before = set(multiprocessing.active_children())
    files = []
    for name in ('fast.c', 'slow.c'):
        path = tmp_path / name
        path.write_text('int foo() { return 1; }')
        files.append(str(path))
    try:
        yield files, tmp_path, before
    finally:
        (tmp_path / 'release').touch()
        for worker in set(multiprocessing.active_children()) - before:
            worker.terminate()
            worker.join(timeout=5)
            if worker.is_alive():
                worker.kill()
                worker.join(timeout=5)


def assert_cancelled(directory, before):
    assert not (set(multiprocessing.active_children()) - before)
    assert not (directory / 'completed').exists()


def test_workers_exit_after_all_results_are_consumed(worker_files):
    files, _, before = worker_files
    result = list(analyze_files(files, threads=2))
    assert {f.filename for f in result} == set(files)
    assert not (set(multiprocessing.active_children()) - before)


def test_unstarted_analysis_does_not_create_workers(worker_files):
    files, _, before = worker_files
    result = analyze_files(files, threads=2)
    assert not (set(multiprocessing.active_children()) - before)
    result.close()
    assert not (set(multiprocessing.active_children()) - before)


@pytest.mark.parametrize('extension_names', [
    [], ['duplicate'], ['duplicate', 'boolcount'],
])
@pytest.mark.parametrize('cancellation', ['close', 'interrupt'])
def test_cancellation_stops_unfinished_work(
        worker_files, extension_names, cancellation):
    files, directory, before = worker_files
    extensions = get_extensions(
        extension_names + [WorkerBarrier(str(directory))])
    result = analyze_files(files, threads=2, exts=extensions)
    assert next(result).filename == files[0]
    assert (directory / 'started').exists()
    assert len(set(multiprocessing.active_children()) - before) == 2
    if cancellation == 'close':
        result.close()
    else:
        with pytest.raises(KeyboardInterrupt):
            result.throw(KeyboardInterrupt)
    assert_cancelled(directory, before)


@pytest.mark.parametrize('failure', [ValueError, ImportError])
def test_worker_error_cancels_pending_work_without_retry(worker_files, failure):
    files, directory, before = worker_files
    extensions = get_extensions([WorkerBarrier(str(directory), failure)])
    result = analyze_files(files, threads=2, exts=extensions)
    with pytest.raises(failure, match='cannot analyze fast.c'):
        list(result)
    assert (directory / 'fast_calls').read_text().splitlines() == ['called']
    assert_cancelled(directory, before)


def test_extension_error_cancels_workers_with_retained_traceback(worker_files):
    files, directory, before = worker_files
    extensions = get_extensions([
        WorkerBarrier(str(directory)), FailingCrossFileExtension()])
    result = analyze_files(files, threads=2, exts=extensions)
    with pytest.raises(ValueError, match='cannot combine fast.c') as error:
        list(result)
    assert error.value.__traceback__ is not None
    result.close()
    assert_cancelled(directory, before)


@pytest.mark.parametrize('unavailable_module', [
    'multiprocessing', 'multiprocessing.synchronize',
])
@pytest.mark.parametrize('threads', [1, 2])
def test_unavailable_multiprocessing_uses_serial_analysis(
        worker_files, monkeypatch, unavailable_module, threads):
    files, _, before = worker_files
    with monkeypatch.context() as unavailable:
        unavailable.setitem(sys.modules, unavailable_module, None)
        result = list(analyze_files(files, threads=threads))
    assert [f.filename for f in result] == files
    assert [len(f.function_list) for f in result] == [1, 1]
    assert not (set(multiprocessing.active_children()) - before)
