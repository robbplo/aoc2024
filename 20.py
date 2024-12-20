from collections import deque, defaultdict
import sys
import itertools

sys.setrecursionlimit(10000)


def puzzle(file = "data/day17"):
    f = open(file, "r")
    input = f.read()

    grid = input.strip().split("\n")
    rows, cols = len(grid), len(grid[0])
    minDists = {}

    def getDists():
        end = False
        for r, row in enumerate(grid):
            for c, col in enumerate(row):
                if col == 'E':
                    end = (r,c)
        q = deque()
        visit = set()

        def add(r,c,d):
            if (r not in range(0, rows) or
                c not in range(0, cols) or
                (r, c) in visit or
                grid[r][c] == '#'
            ):
                return
            visit.add((r,c))
            q.append((r,c,d))

        (r,c) = end
        add(r,c,0)

        while q:
            for i in range(len(q)):
                r, c, d = q.popleft()
                minDists[(r,c)] = d

                if grid[r][c] == 'S':
                    return d

                neighbours = [(r + r2, c + c2) for (r2, c2) in [[0,1], [0, -1], [1,0], [-1,0]]]
                for r, c in neighbours:
                    add(r, c, d+1)

        return "error"


    defaultDist = getDists()

    minSaved = 100
    visitCheat = set()
    cheats = []
    i = 0
    def cheatDists(r, c, d):
        nonlocal i
        if (r not in range(0, rows) or
            c not in range(0, cols) or
            (r, c) in visitCheat or
            grid[r][c] == '#'
        ):
            return []

        visitCheat.add((r,c))
        neighbours = [(r + r2, c + c2) for (r2, c2) in [[0,1], [0, -1], [1,0], [-1,0]]]
        cheatNeighbours = [(r + r2, c + c2) for (r2, c2) in [[0,2], [0, -2], [2,0], [-2,0]]]

        for r_cheat, c_cheat in cheatNeighbours:
            if ((r_cheat, c_cheat) not in minDists):
                continue

            saved = defaultDist - (d + minDists[(r_cheat, c_cheat)]) - 2
            if saved > 0:
                cheats.append(saved)
            if saved >= 100:
                i += 1

        for r, c in neighbours:
            cheatDists(r, c, d+1)



    start = False
    for r, row in enumerate(grid):
        for c, col in enumerate(row):
            if col == 'S':
                print(cheatDists(r, c, 0))
                cheats.sort()
                for a, b in itertools.groupby(cheats):
                    print(f"{a}: {len(list(b))}")

    print("gte100:")
    print(i)









file = "data/day20"
if (len(sys.argv) > 1):
    file = sys.argv[1]


puzzle(file)




