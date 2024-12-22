from collections import deque, defaultdict
import sys
import itertools
import functools

sys.setrecursionlimit(10000)

def puzzle(file = "data/day21"):
    f = open(file, "r")
    input = f.read()

    def grid(s):
        return [c for row in s for c in row]

    codes = input.strip().split("\n")

   


    def minPath(pad, dirs, start, to):
        q = deque()
        visit = set()

        def add(pos, steps):
            r, c = pos
            if (r not in range(0, len(pad)) or
                c not in range(0, len(pad[0])) or
                (r,c) in visit or
                pad[r][c] == None
            ):
                return False
            visit.add((r,c))
            q.append([pos, steps])


        results = []
        q.append([start, []])

        while q:
            for i in range(len(q)):
                pos, steps = q.popleft()
                r, c = pos
                if pad[r][c] == to:
                    steps.append("A")
                    return steps
                for arrow, dir in dirs:
                    rd, cd = dir
                    newSteps = steps.copy()
                    newSteps.append(arrow)
                    add([r + rd, c + cd], newSteps)

    numpad = [
        ["7", "8", "9"],
        ["4", "5", "6"],
        ["1", "2", "3"],
        [None, "0", "A"]
    ]
    def numPath(start, to):
        dirs1 = [
            ["v", [1, 0]],
            ["^", [-1, 0]],
            ["<", [0, -1]],
            [">", [0, 1]],
        ]
        dirs2 = [
            ["^", [-1, 0]],
            [">", [0, 1]],
            ["<", [0, -1]],
            ["v", [1, 0]],
        ]
        print(minPath(numpad, dirs1, start, to))
        print(minPath(numpad, dirs2, start, to))
        return min(
            minPath(numpad, dirs1, start, to),
            minPath(numpad, dirs2, start, to),
            key=len
        )


    dirpad = [
        [None, "^", "A"],
        ["<", "v", ">"]
    ]
    def dirPath(start, to):
        
        dirs = [
            [">", [0, 1]],
            ["v", [1, 0]],
            ["^", [-1, 0]],
            ["<", [0, -1]],
        ]
        return minPath(dirpad, dirs, start, to)


    @functools.cache
    def numPos(num):
        for r, row in enumerate(numpad):
            for c, col in enumerate(row):
                if col == num:
                    return [r,c]
        print("error num", num)

    @functools.cache
    def dirPos(dir):
        for r, row in enumerate(dirpad):
            for c, col in enumerate(row):
                if col == dir:
                    return [r,c]
        print("error dir", dir)





    def mapNums(code):
        numpadStart = [3, 2]
        codeRes = []
        for char in code:
            codeRes.append("".join(numPath(numpadStart, char)))

            numpadStart = numPos(char)
        return "".join(codeRes)

    def mapDirs(dirs):
        dirpadStart = [0, 2]
        codeRes = []
        for char in dirs:
            codeRes.append("".join(dirPath(dirpadStart, char)))

            dirpadStart = dirPos(char)
        return "".join(codeRes)


    numpadInstructions = []
    for code in codes:
        mappedCode = mapNums(code)
        mapTwo = mapDirs(mappedCode)
        mapThree = mapDirs(mapTwo)

        # print(mapThree)
        # print(mapTwo)
        # print(mappedCode)
        # print(code)

        number = int(code[:len(code)-1])
        numpadInstructions.append([number, mapThree])

    i = 0
    for code, seq in numpadInstructions:
        print(code, len(seq), seq)
        i += code * (len(seq))
    print(i)




# <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
# v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A

# v<<A>^>A<A>A<AAv>A^Av<AAA^>A
# v<<A>>^A<A>AvA<^AA>A<vAAA>^A
# <A^A^^>AvvvA
# 029A




file = "data/day21"
if (len(sys.argv) > 1):
    file = sys.argv[1]


puzzle(file)




