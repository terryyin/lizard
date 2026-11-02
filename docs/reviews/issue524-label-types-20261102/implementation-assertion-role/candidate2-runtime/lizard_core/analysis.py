"""
Batch analysis: discovering the source files under the given paths and
analyzing them, serially or in worker processes, with cross-file extensions.
"""
from lizard_core.file_discovery import get_all_source_files
from lizard_core.file_analyzer import FileAnalyzer
from lizard_core.extensions import get_extensions


# pylint: disable-msg=too-many-arguments
def analyze(paths, exclude_pattern=None, threads=1, exts=None,
            lans=None, use_gitignore=True):
    '''
    returns an iterator of file information that contains function
    statistics.
    '''
    exclude_pattern = exclude_pattern or []
    files = get_all_source_files(paths, exclude_pattern, lans, use_gitignore)
    return analyze_files(files, threads, exts)


def analyze_files(files, threads=1, exts=None):
    extensions = exts or get_extensions([])
    file_analyzer = FileAnalyzer(extensions)
    source = result = map_files_to_analyzer(files, file_analyzer, threads)
    try:
        for extension in extensions:
            if hasattr(extension, 'cross_file_process'):
                result = extension.cross_file_process(result)
        yield from result
    finally:
        close = getattr(source, 'close', None)
        if close is not None:
            close()


def map_files_to_analyzer(files, analyzer, working_threads):
    if working_threads == 1:
        return map(analyzer, files)
    return _map_in_pool(analyzer, files, working_threads)


def _map_in_pool(analyzer, files, working_threads):
    try:
        import multiprocessing
        pool = multiprocessing.Pool(processes=working_threads)
    except ImportError:
        yield from map(analyzer, files)
        return
    try:
        for result in pool.imap_unordered(analyzer, files):
            yield result
    except BaseException:
        pool.terminate()
        raise
    else:
        pool.close()
    finally:
        pool.join()


analyze_file = FileAnalyzer(get_extensions([]))  # pylint: disable=C0103
