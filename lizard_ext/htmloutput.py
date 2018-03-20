# -*- coding: utf-8 -*-

'''
This module extends the default output formatting to include HTML.
'''

import os
import sys
import datetime


def html_output(result, options, _):
    try:
        from jinja2 import Environment, FileSystemLoader, select_autoescape
    except ImportError:
        sys.stderr.write(
                "HTML Output depends on jinja2. `pip install jinja2` first")
        sys.exit(2)

    file_list = []
    for source_file in result:
        if source_file:
            source_file_dict = {"filename": source_file.filename}
            func_list = []
            for source_function in source_file.function_list:
                if source_function:
                    source_function_dict = _create_dict(source_function)
                    func_list.append(source_function_dict)
                    source_file_dict["functions"] = func_list
        file_list.append(source_file_dict)

    cwd = os.path.join(sys.prefix, 'lizard_ext')
    if not os.path.exists(cwd):
        cwd = os.path.dirname(os.path.abspath(__file__))
    env = Environment(loader=FileSystemLoader(cwd),
                      autoescape=select_autoescape(['html']))

    time = datetime.datetime.now()
    date = time.strftime('%Y-%m-%d %H:%M')
    output = env.get_template('template.html').render(
            title='Lizard code complexity report',
            date=date, thresholds=options.thresholds, files=file_list)
    print(output)


def _create_dict(obj):

    return obj.__dict__
