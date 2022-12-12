#!/usr/bin/python3

import heapq
import sys

input = sys.stdin.read().rstrip('\n').split('\n')

heights = {}

start = None
end = None

for y, row in enumerate(input):
    for x, c in enumerate(row):
        pos = (x, y)
        if c == 'S':
            start = pos
            c = 'a'
        elif c == 'E':
            end = pos
            c = 'z'
        heights[pos] = ord(c) - ord('a')

def neighbours(pos):
    x, y = pos
    return [(x, y-1), (x+1, y), (x, y+1), (x-1, y)]

def distance(pos1, pos2):
    x1, y1 = pos1
    x2, y2 = pos2
    return abs(x1-x2) + abs(y1-y2)

def find_route(starts):
    heap = []
    gscores = {}

    for pos in starts:
        heapq.heappush(heap, (0, pos))
        gscores[pos] = 0

    while heap:
        steps, pos = heapq.heappop(heap)
        if pos == end:
            break
        for new_pos in neighbours(pos):
            if new_pos not in heights or heights[new_pos] > heights[pos] + 1:
                continue
            gscore = gscores[pos] + 1
            if new_pos in gscores and gscore >= gscores[new_pos]:
                continue
            gscores[new_pos] = gscore
            fscore = gscore + distance(new_pos, end)
            heapq.heappush(heap, (fscore, new_pos))

    return steps

print(find_route([start]))
print(find_route([pos for pos, height in heights.items() if height == 0]))
