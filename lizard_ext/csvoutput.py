'''
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
'''


def csv_output(result, verbose):
    result = result.result
    if verbose:
        print("NLOC,CCN,token,PARAM,length,location,file,function," +
              "long_name,start,end")

    for source_file in result:
        if source_file:
            for source_function in source_file.function_list:
                if source_function:
                    print('{},{},{},{},{},"{}","{}","{}","{}",{},{}'.format(
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
                        source_function.end_line
                    ))
