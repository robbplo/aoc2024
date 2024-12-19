from collections import deque
import sys
import re
import functools


sys.setrecursionlimit(10000)

def puzzle(file = "data/day19"):
    f = open(file, "r")
    input = f.read()

    lines = input.strip().split("\n\n")
    towels = lines[0].split(", ")
    designs = lines[1].split("\n")

    @functools.cache
    def design_fits(design):
        if design == "":
            return 1

        options = []
        for t in towels:
            if design.startswith(t):
                options.append( design_fits(design[len(t):]) )

        return sum(options)

    i = 0
    for l in designs:
        i += design_fits(l)

puzzle()

#puzzle("18.test")




