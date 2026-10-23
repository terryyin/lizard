"""
Per-file analysis: the token processors every analysis runs and the file
analyzer that reads one source file and drives its language reader.
"""
import re
import sys
from lizard_languages import get_reader_for, CLikeReader
from lizard_ext import auto_read
from lizard_core.function_info import FileInformation
from lizard_core.file_info_builder import FileInfoBuilder


def preprocessing(tokens, reader):
    if hasattr(reader, "preprocess"):
        return reader.preprocess(tokens)
    return (t for t in tokens if not t.isspace() or t == '\n')


def comment_counter(tokens, reader):
    for token in tokens:
        comment = reader.get_comment_from_token(token)
        if comment is not None:
            for _ in comment.splitlines()[1:]:
                yield '\n'
            stripped = comment.strip()
            if stripped.startswith("#lizard forgive global"):
                reader.context.forgive_global = True
            elif stripped.startswith("#lizard forgives("):
                match = re.search(r'#lizard forgives?\(([^)]*)\)', stripped)
                if match:
                    metrics = {m.strip() for m in match.group(1).split(',') if m.strip()}
                    reader.context.current_function.forgiven_metrics.update(metrics)
            elif stripped.startswith("#lizard forgive"):
                reader.context.forgive = True
            if "GENERATED CODE" in comment:
                return
        else:
            yield token


def line_counter(tokens, reader):
    context = reader.context
    context.current_line = 1
    newline = 1
    for token in tokens:
        if token != "\n":
            count = token.count('\n')
            context.current_line += count
            context.add_nloc(count + newline)
            newline = 0
            yield token
        else:
            context.current_line += 1
            newline = 1


def token_counter(tokens, reader):
    context = reader.context
    for token in tokens:
        context.fileinfo.token_count += 1
        context.current_function.token_count += 1
        yield token


def condition_counter(tokens, reader):
    conditions = reader.conditions
    for token in tokens:
        if token in conditions:
            reader.context.add_condition()
        yield token


class FileAnalyzer(object):  # pylint: disable=R0903

    def __init__(self, extensions):
        self.processors = extensions

    def __call__(self, filename):
        try:
            return self.analyze_source_code(
                filename, auto_read(filename))
        except UnicodeDecodeError:
            sys.stderr.write("Error: doesn't support none utf encoding '%s'\n"
                             % filename)
        except IOError:
            sys.stderr.write("Error: Fail to read source file '%s'\n"
                             % filename)
        except IndexError:
            sys.stderr.write("Error: Fail to parse file '%s'\n"
                             % filename)
            raise
        return FileInformation(filename, 0, [])

    def analyze_source_code(self, filename, code):
        context = FileInfoBuilder(filename)
        reader = (get_reader_for(filename) or CLikeReader)(context)
        tokens = reader.generate_tokens(code)
        try:
            for processor in self.processors:
                tokens = processor(tokens, reader)
            for _ in reader(tokens, reader):
                pass
        except RecursionError as e:
            sys.stderr.write(
                "[skip] fail to process '%s' with RecursionError - %s\n" %
                (filename, e))
        return context.fileinfo
