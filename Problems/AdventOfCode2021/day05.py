import numpy as np
from numpy.typing import NDArray


class Line:
    def __init__(self, x1, y1, x2, y2):
        self.x1 = int(x1)
        self.y1 = int(y1)
        self.x2 = int(x2)
        self.y2 = int(y2)

    def __repr__(self) -> str:
        return f"({self.x1}, {self.y1} -> {self.x2}, {self.y2})"


class Map:
    def __init__(self, size: int):
        # Square of size lines.max x lines.max
        self.map = np.zeros((size + 1, size + 1), dtype=np.uint8)

    def draw_line(self, line: Line, draw_diagonals: bool = False) -> None:
        # add a 1 to every square traversed by a line
        # (x1, y1) -> (x2, y2)

        # Horizontal line
        if line.x1 == line.x2:
            # Coordinates need to be ordered, we may have to reverse them
            if line.y1 > line.y2:
                line.y1, line.y2 = line.y2, line.y1
            self.map[line.y1 : line.y2 + 1, line.x1] += 1
        # Vertical line
        elif line.y1 == line.y2:
            if line.x1 > line.x2:
                line.x1, line.x2 = line.x2, line.x1
            self.map[line.y1, line.x1 : line.x2 + 1] += 1
        # Diagonal line
        elif draw_diagonals:
            y = line.y1
            x = line.x1
            for _ in range(abs(line.x2 - line.x1) + 1):
                self.map[y, x] += 1
                # Diagonals can either go up or down
                # left or right so we account for the
                # order of x1/y1 and x2/y2
                x += 1 if line.x1 < line.x2 else -1
                y += 1 if line.y1 < line.y2 else -1

    def count_vents(self) -> int:
        "counts the number of spots where the value is >= 2"
        return np.count_nonzero(self.map >= 2)

    def __repr__(self) -> str:
        return str(self.map)


inputs: NDArray = np.empty((500), dtype=Line)
with open("./inputs/day5.txt", "r") as f:
    max_value = -1
    for i, line in enumerate(f):
        start, end = line.split("->")
        start = list(map(int, start.strip().split(",")))
        end = list(map(int, end.strip().split(",")))
        max_value = max(max_value, max(*start, *end))
        line = Line(*start, *end)
        inputs[i] = line

# -----------*** Part 1 ***-----------

# Create the map and draw all lines that are not diagonals
map = Map(size=max_value)
for line in inputs:
    map.draw_line(line, draw_diagonals=False)

print(f"Part 1: {map.count_vents()}")

# -----------*** Part 2 ***-----------

# This time, we take the diagonals into account
map = Map(size=max_value)
for line in inputs:
    map.draw_line(line, draw_diagonals=True)

print(f"Part 2: {map.count_vents()}")
