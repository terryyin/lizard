from lizard import  analyze_file, CLikeReader

def get_cpp_fileinfo(source_code):
    return analyze_file.analyze_source_code_with_parser("", source_code, CLikeReader())


