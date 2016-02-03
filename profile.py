from lizard import lizard_main
import cProfile, pstats, StringIO
import sys


if __name__ == "__main__":
    pr = cProfile.Profile()
    pr.enable()
    lizard_main(sys.argv)
    pr.disable()
    s = StringIO.StringIO()
    sortby = 'tottime'
    ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
    ps.print_stats()
    print s.getvalue()


