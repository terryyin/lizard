'''The offside layout of F# light syntax items.'''

CONTAINER_HEADINGS = frozenset({'type', 'module', 'namespace'})


class OffsideItems:
    '''The column and heading kind of each open `type` / `module` /
    `namespace` container and of the current item, innermost last. An
    `and` that continues a `type` group heads a type too.'''

    def __init__(self):
        self._items = []
        self._sibling = None

    def starts_item(self, column, heading):
        '''Whether a line whose first code token, `heading`, is at
        `column` with brackets closed heads a new item: it is at or left
        of the current item, or deeper than a container heading.'''
        items = self._items
        self._sibling = None
        while items and column <= items[-1][0]:
            self._sibling = items.pop()
        if items and self._sibling is None and items[-1][1] == 'item':
            return False
        items.append((column, self._kind(column, heading)))
        return True

    def reheaded(self, heading):
        '''The current item's heading follows its attribute group.'''
        column = self._items[-1][0]
        self._items[-1] = (column, self._kind(column, heading))

    def in_container(self):
        '''Whether the current item heads a container.'''
        return self._items[-1][1] != 'item'

    def _kind(self, column, heading):
        if heading == 'type' or heading == 'and' and \
                self._sibling == (column, 'type'):
            return 'type'
        return 'container' if heading in CONTAINER_HEADINGS else 'item'
