"""
File discovery: walking the given paths, applying exclusion patterns,
language selection and .gitignore rules, and dropping duplicate files.
"""
import os
import hashlib
import re
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


_SEGMENT_PART = re.compile(r"\\(?P<escape>.|$)|(?P<bracket>\[)|\*{2,}")


def _compatible_gitignore_segment(segment):
    if segment == '**':
        return segment

    parts = []
    position = 0
    while True:
        match = _SEGMENT_PART.search(segment, position)
        if match is None:
            parts.append(segment[position:])
            return ''.join(parts)

        parts.append(segment[position:match.start()])
        position = match.end()
        if match.group('escape') is not None:
            if not match.group('escape'):
                return None
            parts.append(match[0])
        elif match.group('bracket'):
            # PathSpec allows a literal ']' at the start of a character class.
            class_start = position
            if segment[class_start:class_start + 1] in ('!', '^'):
                class_start += 1
            if segment[class_start:class_start + 1] == ']':
                class_start += 1
            close = segment.find(']', class_start)
            if close == -1:
                return None
            end = close + 1
            body = segment[position:end].replace('\\', r'\\')
            # PathSpec treats a nested POSIX class opener as literal text.
            parts.append('[' + body.replace('[:', r'\[:'))
            position = end
        else:
            parts.append('*')


def _compatible_gitignore_pattern(pattern):
    """Keep the wildcard and bracket semantics used by PathSpec."""
    negation = '!' if pattern.startswith('!') else ''
    glob = pattern[len(negation):]
    # A leading globstar can match zero directories, even after '/'.
    if glob.startswith('/**/'):
        glob = glob[1:]

    segments = []
    # PathSpec splits at every slash, including inside escapes and brackets.
    for segment in glob.split('/'):
        compatible = _compatible_gitignore_segment(segment)
        if compatible is None:
            return None
        segments.append(compatible)
    return negation + '/'.join(segments)


def _build_gitignore_spec(patterns):
    '''Compile gitignore patterns while tolerating malformed lines.'''
    try:
        import gitmatch
    except ImportError:
        return None

    compiled = []
    for pattern in patterns:
        compatible = _compatible_gitignore_pattern(pattern)
        rules = gitmatch.compile([compatible]).patterns if compatible is not None else []
        if not rules and '\\' in pattern:
            fallback = _compatible_gitignore_pattern(pattern.replace('\\', '/'))
            if fallback is not None:
                rules = gitmatch.compile([fallback]).patterns
        compiled.extend(rules)
    return _GitignoreSpec(compiled) if compiled else None


class _GitignoreSpec:
    def __init__(self, patterns):
        self.patterns = patterns

    def match_file(self, path):
        # Apply the last matching rule across the file and its parent directories.
        parents = [path[:i] for i, char in enumerate(path) if char == '/']
        for rule in reversed(self.patterns):
            if rule.match(path) or any(rule.match(parent, is_dir=True) for parent in parents):
                return not rule.negative
        return False


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
