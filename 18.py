from collections import deque
import sys

sys.setrecursionlimit(10000)

def puzzle(file = "data/day18"):
    f = open(file, "r")
    input = f.read()

    lines = input.strip().split("\n\n")
    towels = lines[0].split(", ")
    designs = lines[1].split("\n")
    maxlen = 0
    for t in towels:
        maxlen = max(len(t), maxlen)

    def design_fits(design, subtowels):
        if design == "":
            return True
        if not subtowels:
            return False

        t = subtowels.pop()
        if design.startswith(t):
            return design_fits(design[len(t):], towels.copy())
        return design_fits(design, subtowels)


    i = 0
    for l in designs:
        if design_fits(l, towels.copy()):
            i += 1
    print(i)



puzzle()

puzzle("18.test")




