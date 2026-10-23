"""
The output schema: the data columns shown for each function, including
those contributed by extensions.
"""
from lizard_core.function_info import FileInformation


class OutputScheme(object):
    '''
    Collect the schema of the data columns.
    Each extension can define some additional data columns to
    the FunctionInfo structure, or even add properties to
    the FileInformation structure.

    In any extension class, define a class level variable:

        FUNCTION_INFO = {
            'column_name' : {
                'caption': 'if defined, will show the column in result',
                'average_caption': 'if defined, will add average function
                                    to FileInformation and show in the
                                    end result.
            }
        }
    '''

    def __init__(self, ext):
        self.extensions = ext
        self.items = [
            {
                'caption': "  NLOC  ", 'value': "nloc",
                'avg_caption': ' Avg.NLOC '},
            {
                'caption': "  CCN  ", 'value': "cyclomatic_complexity",
                'avg_caption': ' AvgCCN '},
            {
                'caption': " token ", 'value': "token_count",
                'avg_caption': ' Avg.token '},
            {'caption': " PARAM ", 'value': "parameter_count"},
            {'caption': " length ", 'value': "length"},
            ] + [
            {
                'caption': caption,
                'value': part,
                'avg_caption': average
            }
            for caption, part, average in self._ext_member_info()]
        self.items.append({'caption': " location  ", 'value': 'location'})

    def patch_for_extensions(self):
        def _patch(name):
            setattr(FileInformation, "average_" + name,
                    property(lambda self: self.functions_average(name)))
        for item in self.items:
            if 'avg_caption' in item:
                _patch(item["value"])

    def any_silent(self):
        return any(hasattr(ex, 'silent_all_others') for ex in self.extensions)

    def value_columns(self):
        return [item['value'] for item in self.items]

    def _ext_member_info(self):
        for ext in self.extensions:
            if hasattr(ext, "FUNCTION_INFO"):
                for key in ext.FUNCTION_INFO:
                    yield (
                        ext.FUNCTION_INFO[key].get("caption", None),
                        key,
                        ext.FUNCTION_INFO[key].get("average_caption", None))

    def captions(self):
        caps = [item.get('caption') for item in self.items]
        return "".join(caption for caption in caps if caption)

    @staticmethod
    def _head(captions):
        return "\n".join(("=" * len(captions), captions, "-" * len(captions)))

    def function_info_head(self):
        return self._head(self.captions())

    def function_info(self, fun):
        return ''.join(
            str(getattr(fun, item['value'])).rjust(len(item['caption']))
            for item in self.items if item['caption'])

    def average_captions(self):
        return "".join([
            e['avg_caption'] for e in self.items
            if e.get("avg_caption", None)])

    def average_formatter(self):
        return "".join([
            "{{module.average_{ext[value]}:{size}.1f}}"
            .format(ext=e, size=len(e['avg_caption']))
            for e in self.items
            if e.get("avg_caption", None)])

    def clang_warning_format(self):
        return ("{f.filename}:{f.start_line}: warning: {f.name} has {f.nloc} NLOC, "
                "{f.cyclomatic_complexity} CCN, {f.token_count} token, {f.parameter_count} PARAM, "
                "{f.length} length, {f.max_nesting_depth} ND")

    def msvs_warning_format(self):
        return (
            "{f.filename}({f.start_line}): warning: {f.name} ({f.long_name}) has " +
            ", ".join([
                "{{f.{ext[value]}}} {caption}"
                .format(ext=e, caption=e['caption'].strip())
                for e in self.items[:-1]
                ]))
