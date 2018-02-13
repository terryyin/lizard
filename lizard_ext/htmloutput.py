# -*- coding: utf-8 -*-

'''
This module extends the default output formatting to include HTML.
'''

from jinja2 import Template, Environment, FileSystemLoader, select_autoescape
import os
import datetime

def html_output(result, verbose, _):

    file_list = []
    for source_file in result:
        if source_file:
            source_file_dict = { "filename": source_file.filename }
            func_list = []
            for source_function in source_file.function_list:
                if source_function:
		    source_function_dict = _create_dict(source_function)
		    func_list.append(source_function_dict)
            source_file_dict["functions"] = func_list
        file_list.append(source_file_dict)

    cwd = os.path.dirname(os.path.abspath(__file__))
    env = Environment(loader=FileSystemLoader(cwd),
                    autoescape=select_autoescape(['html']))

    t = datetime.datetime.now()
    date = t.strftime('%Y-%m-%d')
    output = env.get_template('template.html').render(
		title='Lizard code complexity report', date = date, files = file_list)
    print(output)


def _create_dict(obj):

    return obj.__dict__
