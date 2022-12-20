#!/usr/bin/python3

import itertools
import sys

def neighbours(pos):
    x, y, z = pos

    yield x-1, y, z
    yield x, y-1, z
    yield x, y, z-1
    yield x+1, y, z
    yield x, y+1, z
    yield x, y, z+1

lava = set(tuple(map(int, line.strip().split(','))) for line in sys.stdin)

surface = 0

for pos in lava:
    for npos in neighbours(pos):
        if npos not in lava:
            surface += 1

print(surface)

min_x = max_x = min_y = max_y = min_z = max_z = None

for x, y, z in lava:
    if min_x is None or min_x > x:
        min_x = x
    if max_x is None or max_x < x:
        max_x = x
    if min_y is None or min_y > y:
        min_y = y
    if max_y is None or max_y < y:
        max_y = y
    if min_z is None or min_z > z:
        min_z = z
    if max_z is None or max_z < z:
        max_z = z

class UnionFind:
    def __init__(self):
        self._parent = None
        self._rank = 0

    def unify(self, other):
        root1 = self.root()
        root2 = other.root()

        if root1 == root2:
            return

        if root1._rank < root2._rank:
            root1, root2 = root2, root1

        root2._parent = root1

        if root1._rank == root2._rank:
            root1._rank += 1

    def root(self):
        node = self
        while node._parent is not None:
            node = node._parent
        return node

air = {}

for x in range(min_x-1, max_x+2):
    for y in range(min_y-1, max_y+2):
        for z in range(min_z-1, max_z+2):
            pos = (x, y, z)
            if pos not in lava:
                air[pos] = UnionFind()

for pos, node in air.items():
    for npos in neighbours(pos):
        if npos in air:
            node.unify(air[npos])

outside_root = air[min_x-1, min_y-1, min_z-1].root()

bubbles = set()

for pos, node in air.items():
    if node.root() != outside_root:
        bubbles.add(pos)

outer_surface = 0

for pos in lava:
    for npos in neighbours(pos):
        if npos not in lava and npos not in bubbles:
            outer_surface += 1

print(outer_surface)
