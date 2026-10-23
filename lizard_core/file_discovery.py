"""
File discovery: walking the given paths, applying exclusion patterns,
language selection and .gitignore rules, and dropping duplicate files.
"""
import os
import hashlib
from fnmatch import fnmatch
from lizard_languages import get_reader_for
from lizard_ext import auto_open, auto_read


def md5_hash_file(full_path_name):
    ''' return md5 hash of a file '''
    try:
        with auto_open(full_path_name, mode='r') as source_file:
            code_md5 = hashlib.md5(source_file.read().encode('utf-8'))
        return code_md5.hexdigest()
    except IOError:
        return None
    except UnicodeDecodeError:
        return None
    except UnicodeEncodeError:
        return None


def _build_gitignore_spec(patterns):
    '''
    Builds a PathSpec from gitignore patterns, tolerating lines pathspec
    can't parse (e.g. Windows-style paths with unescaped backslashes) by
    normalizing separators or, failing that, dropping just that line.
    '''
    try:
        import pathspec
        from pathspec.patterns.gitwildmatch import GitWildMatchPatternError
    except ImportError:
        return None

    def _valid_form(pattern):
        for candidate in (pattern, pattern.replace('\\', '/')):
            try:
                pathspec.PathSpec.from_lines('gitwildmatch', [candidate])
                return candidate
            except GitWildMatchPatternError:
                continue
        return None

    valid_patterns = [p for p in (_valid_form(p) for p in patterns) if p]
    if not valid_patterns:
        return None
    return pathspec.PathSpec.from_lines('gitwildmatch', valid_patterns)


def get_all_source_files(paths, exclude_patterns, lans, use_gitignore=True):
    '''
    Function counts md5 hash for the given file and checks if it isn't a
    duplicate using set of hashes for previous files.

    If use_gitignore is true and a .gitignore file is found in any of the
    given paths, it will be used to filter out files that match the gitignore
    patterns.
    '''
    hash_set = set()
    gitignore_spec = None
    base_path = None

    def _load_gitignore():
        nonlocal gitignore_spec, base_path
        for path in paths:
            gitignore_path = os.path.join(path, '.gitignore')
            if os.path.exists(gitignore_path):
                gitignore_file = auto_read(gitignore_path)
                # Read lines and strip whitespace and empty lines
                patterns = [line.strip() for line in gitignore_file.splitlines()]
                patterns = [p for p in patterns if p and not p.startswith('#')]
                gitignore_spec = _build_gitignore_spec(patterns)
                base_path = path
                break

    def _support(reader):
        return not lans or set(lans).intersection(
            reader.language_names)

    def _validate_file(pathname):
        if gitignore_spec is not None and base_path is not None:
            rel_path = os.path.relpath(pathname, base_path)
            # Normalize path separators for consistent matching
            rel_path = rel_path.replace(os.sep, '/')
            if gitignore_spec.match_file(rel_path):
                return False
        return (
            pathname in paths or (
                get_reader_for(pathname) and
                _support(get_reader_for(pathname)) and
                all(not fnmatch(pathname, p) for p in exclude_patterns) and
                _not_duplicate(pathname)))

    def _not_duplicate(full_path_name):
        fhash = md5_hash_file(full_path_name)
        if not fhash or fhash not in hash_set:
            hash_set.add(fhash)
            return True

    def all_listed_files(paths):
        for path in paths:
            if os.path.isfile(path):
                yield path
            else:
                for root, _, files in os.walk(path, topdown=False):
                    for filename in files:
                        yield os.path.join(root, filename)

    if use_gitignore:
        _load_gitignore()
    return filter(_validate_file, all_listed_files(paths))
