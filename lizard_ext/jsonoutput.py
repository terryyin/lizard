'''
This module extends the default output formatting to include JSON.

'''

import json

def json_output(result, verbose):
    to_json = {
        "header": [
            "NLOC", "CCN", "token", "PARAM", "length", "location", "file",
            "function", "start", "end"
        ],
        "files": []
    }

    for source_file in result:
        if source_file:
            for source_function in source_file.function_list:
                if source_function:
                    to_json['files'].append({
                        "NLOC": source_function.nloc,
                        "CCN": source_function.cyclomatic_complexity,
                        "token": source_function.token_count,
                        "PARAM": len(source_function.parameters),
                        "length": source_function.end_line - source_function.start_line,
                        "location": "{}@{}-{}@{}".format(
                            source_function.name,
                            source_function.start_line,
                            source_function.end_line,
                            source_file.filename
                        ),
                        "file": source_file.filename,
                        "function": source_function.name,
                        "start": source_function.start_line,
                        "end": source_function.end_line
                    })

    print json.dumps(to_json, indent=4)
