#!/usr/bin/python3

import math
import operator
import re
import sys

class Monkey:
    pattern = re.compile(
        r'Monkey (?P<id>\d+):\n'
        r'  Starting items: (?P<items>\d+(?:, \d+)*)\n'
        r'  Operation: new = (?P<lhs>\d+|old) (?P<op>[+*]) (?P<rhs>\d+|old)\n'
        r'  Test: divisible by (?P<mod>\d+)\n'
        r'    If true: throw to monkey (?P<true>\d+)\n'
        r'    If false: throw to monkey (?P<false>\d+)\n'
    )

    ops = {'+': operator.add, '*': operator.mul}

    def __init__(self, items, inspect, throw_to, mod):
        self.items = list(items)
        self.inspect = inspect
        self._throw_to = throw_to
        self.mod = mod
        self.inspections = 0

    def take_turn(self, manage_worry):
        for item in self.items:
            item = self.inspect(item)
            item = manage_worry(item)
            self.throw(item)
        self.inspections += len(self.items)
        self.items.clear()

    def throw(self, item):
        self._throw_to(item % self.mod == 0).items.append(item)

    @staticmethod
    def parse_all(string):
        monkeys = []

        def to_inspect(lhs, op, rhs):
            return lambda old: op(old if lhs == 'old' else int(lhs), old if rhs == 'old' else int(rhs))

        def to_throw_to(true, false):
            return lambda truth: monkeys[true if truth else false]

        for match in Monkey.pattern.finditer(string):
            items = map(int, match.group('items').split(', '))
            lhs = match.group('lhs')
            op = Monkey.ops[match.group('op')]
            rhs = match.group('rhs')
            mod = int(match.group('mod'))
            true = int(match.group('true'))
            false = int(match.group('false'))
            inspect = to_inspect(lhs, op, rhs)
            throw_to = to_throw_to(true, false)
            monkeys.append(Monkey(items, inspect, throw_to, mod))

        return monkeys

    @staticmethod
    def business(monkeys):
        first, second, *rest = sorted((monkey.inspections for monkey in monkeys), reverse=True)
        return first * second

def part1(input):
    monkeys = Monkey.parse_all(input)

    for round in range(20):
        for monkey in monkeys:
            monkey.take_turn(lambda worry: worry // 3)

    return Monkey.business(monkeys)

def part2(input):
    monkeys = Monkey.parse_all(input)
    mod = math.lcm(*(monkey.mod for monkey in monkeys))

    for round in range(10000):
        for monkey in monkeys:
            monkey.take_turn(lambda worry: worry % mod)

    return Monkey.business(monkeys)

if __name__ == '__main__':
    input = sys.stdin.read()
    print(part1(input))
    print(part2(input))
