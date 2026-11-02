'''Open file with automatic encoding'''
import io
import os
import codecs


def auto_open(*args, **kwargs):
    '''
    Ok. I believe a big can of worms has just been opened.
    Codecs of text file is very hard to detect.
    So far lizard hasn't include any other dependencies,
    so I'm not too comfortable to introduce the first dependency
    (chardet) only for this. And it won't be a perfect solution
    any way. Let's see how far we can go by just patching for
    new requests.

    So far it can handle:
        UTF-8 With BOM

    '''
    size = min(32, os.path.getsize(args[0]))
    with io.open(args[0], 'rb') as binary:
        if binary.read(size).startswith(codecs.BOM_UTF8):
            kwargs["encoding"] = 'utf-8-sig'
            return io.open(*args, **kwargs)
    return io.open(*args, **kwargs)


def auto_read(filename):
    try:
        with auto_open(filename, 'r') as current_file:
            return current_file.read()

    except UnicodeDecodeError:
        with open(filename, 'rb') as current_file:
            return current_file.read().decode('utf8', 'ignore')
