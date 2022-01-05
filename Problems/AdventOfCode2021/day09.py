import numpy as np

# -----------*** Part 1 ***-----------
map = np.array(
    [list(map(int, list(line.strip()))) for line in open("./inputs/day9.txt")]
)
x, y = map.shape

# go through each value of the map and check the values
# of its neighbours (horizontal and vertical)
# if current value is smaller than all the neighbours,
# add current value to the sum result
result = 0
for i in range(x):
    for j in range(y):
        neighbours = []
        if i > 0:
            neighbours.append(map[i - 1, j])
        if i < x - 1:
            neighbours.append(map[i + 1, j])
        if j > 0:
            neighbours.append(map[i, j - 1])
        if j < y - 1:
            neighbours.append(map[i, j + 1])
        if all(map[i, j] < n for n in neighbours):
            result += map[i, j] + 1

print(f"Part 1: {result}")

# -----------*** Part 2 ***-----------
