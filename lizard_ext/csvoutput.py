"""
This module extends the default output formatting to include CSV.

The output is intended to be the same in structure, with additional
tokens (to reduce the need to post-process), and a reduced verbosity
due to the nature of CSV outputs. The differences are:

 * No summary of the included files, only the output
   respective of each function.
 * No additional output for functions that break any CCN
   thresholds.
 * Each line has four additional, individual tokens:
     * File name
     * Function Name
     * Function line start
     * Function line end
"""


def csv_output(result, options):
    result = result.result

    extension_variables = []
    extension_captions = []
    for extension in options.extensions:
        if extension.__class__.__name__ == 'LizardExtension':
            if hasattr(extension, 'FUNCTION_INFO') and \
                    len(list(extension.FUNCTION_INFO)) == 1:
                extension_variable = list(extension.FUNCTION_INFO)[0]
                extension_variables.append(extension_variable)
                var_extension = extension.FUNCTION_INFO[extension_variable]
                extension_caption = var_extension['caption'] \
                    if 'caption' in var_extension else 'No_caption'
                extension_captions.append(extension_caption)

    if options.verbose:
        extension_caption = ""
        for caption in extension_captions:
            extension_caption = "{},{}".format(extension_caption, caption)
        print("NLOC,CCN,token,PARAM,length,location,file,function," +
              "long_name,start,end{}".format(extension_caption))

    for source_file in result:
        if source_file:
            for source_function in source_file.function_list:
                if source_function:
                    extension_string = ''
                    for variable in extension_variables:
                        extension_string = '{},{}'.\
                            format(extension_string,
                                   source_function.__getattribute__(variable))
                    print('{},{},{},{},{},"{}","{}","{}","{}",{},{}{}'.format(
                        source_function.nloc,
                        source_function.cyclomatic_complexity,
                        source_function.token_count,
                        len(source_function.parameters),
                        source_function.length,
                        "{}@{}-{}@{}".format(
                            source_function.name.replace("\"", "'"),
                            source_function.start_line,
                            source_function.end_line,
                            source_file.filename
                        ),
                        source_file.filename,
                        source_function.name.replace("\"", "'"),
                        source_function.long_name.replace("\"", "'"),
                        source_function.start_line,
                        source_function.end_line,
                        extension_string
                    ))
