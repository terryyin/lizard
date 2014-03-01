from lizard import  analyze_file, FileAnalyzer, get_extensions

def get_cpp_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.cpp", source_code)

def get_cpp_function_list_with_extnesion(source_code, extension):
    return FileAnalyzer(get_extensions([extension])).analyze_source_code("a.cpp", source_code).function_list

def get_cpp_function_list(source_code):
    return get_cpp_fileinfo(source_code).function_list

