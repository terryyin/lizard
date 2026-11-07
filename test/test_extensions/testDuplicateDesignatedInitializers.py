import unittest
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector


class TestDuplicateDesignatedInitializers(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()

    def detect(self, code, min_duplicate_tokens = 31):
        list(self.detector.cross_file_process(
                [get_cpp_fileinfo_with_extension(code, self.detector)]
            ))
        return list(self.detector.get_duplicates(min_duplicate_tokens))

    def test_designated_enum_initializer_table_is_not_duplicate(self):
        # Issue #487: designated rows dilute constant density, so values
        # unify and consecutive rows hash as equal; positional form stays clean.
        code = '''
            typedef enum { dog, cat, parrot, rat, fish, snake, hamster } pet;
            typedef enum { garden, house, away } place;
            typedef enum { happy, angry, meh } mood;
            typedef enum { no, yes, not_yet } feed;
            typedef struct {
                pet animal; place where; mood is; feed needs_food;
            } ark;
            static ark noah[] = {
                {.animal=dog,     .where=house,  .is=happy,      .needs_food=no},
                {.animal=fish,    .where=house,  .is=happy,      .needs_food=no},
                {.animal=hamster, .where=house,  .is=happy,      .needs_food=no},
                {.animal=parrot,  .where=garden, .is=meh,        .needs_food=not_yet},
                {.animal=snake,   .where=garden, .is=angry,      .needs_food=not_yet},
                {.animal=rat,     .where=away,   .is=happy,      .needs_food=yes},
                {.animal=cat,     .where=house,  .is=angry,      .needs_food=yes},
            };
            static ark titanic[] = {
                {dog,     house,  happy,      no},
                {fish,    house,  happy,      no},
                {hamster, house,  happy,      no},
                {parrot,  garden, meh,        not_yet},
                {snake,   garden, angry,      not_yet},
                {rat,     away,   happy,      yes},
                {cat,     house,  angry,      yes},
            };
        '''
        self.assertEqual([], self.detect(code))

    def test_designated_numeric_initializer_table_is_not_duplicate(self):
        designated = '''
            typedef struct { int a; int b; int c; int d; } four;
            static four t[] = {
                {.a=1, .b=2, .c=3, .d=4},
                {.a=5, .b=6, .c=7, .d=8},
                {.a=9, .b=10, .c=11, .d=12},
                {.a=13, .b=14, .c=15, .d=16},
                {.a=17, .b=18, .c=19, .d=20},
                {.a=21, .b=22, .c=23, .d=24},
                {.a=25, .b=26, .c=27, .d=28},
            };
        '''
        positional = '''
            typedef struct {
                int a; int b; int c; int d; int e;
                int f; int g; int h; int i; int j;
            } wide;
            static wide w[] = {
                {1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
                {2, 3, 4, 5, 6, 7, 8, 9, 10, 11},
                {3, 4, 5, 6, 7, 8, 9, 10, 11, 12},
                {4, 5, 6, 7, 8, 9, 10, 11, 12, 13},
                {5, 6, 7, 8, 9, 10, 11, 12, 13, 14},
                {6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
                {7, 8, 9, 10, 11, 12, 13, 14, 15, 16},
            };
        '''
        self.assertEqual([], self.detect(designated))
        self.assertEqual([], self.detect(positional))
