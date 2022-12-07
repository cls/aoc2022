#!/usr/bin/python3

import sys

class Dir:
    def __init__(self, parent):
        self.parent = parent
        self.entries = {}

    def total_size(self):
        return sum(node.total_size() for node in self.entries.values())

    def subdirs(self):
        dirs = []
        for node in self.entries.values():
            if isinstance(node, Dir):
                dirs.append(node)
                dirs.extend(node.subdirs())
        return dirs

class File:
    def __init__(self, size):
        self.size = size

    def total_size(self):
        return self.size

root = Dir(None)
pwd = None

for line in sys.stdin:
    if line.startswith("$ "):
        argv = line[2:].split()
        argc = len(argv)
        if argv[0] == "cd":
            assert argc == 2
            path = argv[1]
            if path.startswith("/"):
                pwd = root
                path = path[1:]
            if path:
                for comp in path.split("/"):
                    if comp == ".":
                        pass
                    elif comp == "..":
                        pwd = pwd.parent
                    else:
                        pwd = pwd.entries[comp]
        elif argv[0] == "ls":
            assert argc == 1
            continue
    else:
        assert argv[0] == "ls"
        stat, name = line.split()
        if stat == "dir":
            pwd.entries[name] = Dir(pwd)
        else:
            pwd.entries[name] = File(int(stat))

# Part 1

sizes = list(map(Dir.total_size, root.subdirs()))

size_limit = 100000

print(sum(filter(lambda size: size <= size_limit, sizes)))

# Part 2

whole_disk = 70000000
need_space = 30000000

disk_space = whole_disk - root.total_size()
reclaiming = need_space - disk_space

print(min(filter(lambda size: size >= reclaiming, sizes)))
