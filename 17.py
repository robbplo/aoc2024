from collections import deque


def puzzle(size = 70, file = "data/day17"):
    f = open(file, "r")
    input = f.read()

    lines = input.strip().split("\n")
    pairs = []

    rows = cols = size + 1


    def run(bytes ):
        grid = [[0 for c in range(cols)] for r in range(rows)]

        for l in lines[:bytes]:
            parts = l.split(",")
            x, y = int(parts[0]), int(parts[1])
            grid[y][x] = '#'


        q = deque([(0,0)])
        visit = set()

        def add(x,y,dist):
            if (x not in range(0, cols) or
                y not in range(0, rows) or
                (x, y) in visit or
                grid[y][x] == '#'
            ):
                return
            visit.add((x,y))
            q.append((x,y))
            grid[y][x] = dist

        while q:
            for i in range(len(q)):
                x, y = q.popleft()
                dist = grid[y][x] + 1
                neighbours = [(x + x2, y + y2) for (x2, y2) in [[0,1], [0, -1], [1,0], [-1,0]]]

                for x, y in neighbours:
                    add(x, y, dist)

        return grid[size][size]

    i = 1500
    while run(i) != 0:
        print(lines[i])
        i += 1

puzzle()

puzzle(6, "17.test")




