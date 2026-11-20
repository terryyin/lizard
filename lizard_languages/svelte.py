'''
Language parser for Svelte components
'''

from .vue import VueReader


class SvelteReader(VueReader):
    # pylint: disable=R0903

    ext = ['svelte']
    language_names = ['svelte']
