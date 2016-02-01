from lizard import lizard_main
import languages
import sys

def lizard_for_more_languages(args):
    lans = [languages.JavaReader,
        languages.JavaScriptReader,
        languages.PythonReader,
        languages.ObjCReader,
        languages.TTCNReader,
        languages.RubyReader,
        languages.SwiftReader]
    lizard_main(args, lans)

if __name__ == "__main__":
    lizard_for_more_languages(sys.argv)
