#!/usr/bin/python3

import heapq
import itertools
import re
import sys

from collections import namedtuple

pattern = re.compile(r'Valve (?P<valve>[A-Z]{2}) has flow rate=(?P<rate>\d+); tunnels? leads? to valves? (?P<tunnels>[A-Z]{2}(?:, [A-Z]{2})*)\n?')

valves = []
rates = {}
tunnels = {}
intset = {}

for i, line in enumerate(sys.stdin):
    match = pattern.fullmatch(line)
    valve = match.group('valve')
    valves.append(valve)
    rates[valve] = int(match.group('rate'))
    tunnels[valve] = frozenset(match.group('tunnels').split(', '))
    intset[valve] = 1 << i

dists = {}

# Floyd-Warshall

for valve, tunnel in tunnels.items():
    dists[(valve, valve)] = 0
    for new_valve in tunnel:
        dists[(valve, new_valve)] = 1

for k in valves:
    for i in valves:
        for j in valves:
            x = dists.get((i, j))
            y = dists.get((i, k))
            z = dists.get((k, j))
            if y is not None and z is not None and (x is None or x > y + z):
                dists[(i, j)] = y + z

max_rate = sum(rates.values())
end_time = 30

State = namedtuple('State', ['time', 'rate', 'pos', 'open_valves'])
Action = namedtuple('Action', ['pos', 'opening_valve'])

def neighbour_states(state):
    our_actions = []
    for pos in state.pos:
        my_our_actions = []
        open_valve = intset[pos]
        if rates[pos] > 0 and (open_valve & state.open_valves) == 0:
            my_our_actions.append(Action(pos, True))
        for new_pos in tunnels[pos]:
            my_our_actions.append(Action(new_pos, False))
        if not my_our_actions:
            my_our_actions.append(Action(pos, False))
        our_actions.append(my_our_actions)
    new_time = state.time + 1
    for actions in itertools.product(*our_actions):
        all_pos = []
        all_open_valves = state.open_valves
        new_rate = state.rate
        skip = False
        for action in actions:
            all_pos.append(action.pos)
            if action.opening_valve:
                open_valve = intset[action.pos]
                if (open_valve & all_open_valves) != 0:
                    skip = True # The elephant and I tried to open the same valve.
                    break
                all_open_valves |= open_valve
                new_rate -= rates[action.pos]
        if not skip:
            yield State(new_time, new_rate, tuple(sorted(all_pos)), all_open_valves)

def heuristic(gscore, state):
    time_left = end_time - state.time
    fscore = gscore + state.rate * time_left
    for valve, rate in rates.items():
        open_valve = intset[valve]
        if (open_valve & state.open_valves) == 0:
            time_taken = min(dists[(pos, valve)] for pos in state.pos) + 1
            fscore -= rate * max(time_left - time_taken, 0)
    return fscore

def find_best(start_time, start_pos):
    heap = []
    gscores = {}
    fscores = {}

    start_state = State(start_time, 0, tuple(sorted(start_pos)), 0)
    heapq.heappush(heap, (heuristic(0, start_state), start_state))
    gscores[start_state] = 0

    while heap:
        pressure, state = heapq.heappop(heap)
        if state.time == end_time or state.rate == -max_rate:
            break
        gscore = gscores[state] + state.rate
        for new_state in neighbour_states(state):
            if new_state in gscores and gscore >= gscores[new_state]:
                continue
            gscores[new_state] = gscore
            fscore = heuristic(gscore, new_state)
            if new_state in fscores and fscore >= fscores[new_state]:
                continue
            fscores[new_state] = fscore
            heapq.heappush(heap, (fscore, new_state))

    return -pressure

print(find_best(0, ('AA',)))
print(find_best(4, ('AA', 'AA')))
