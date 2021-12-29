positions = [int(x) for x in open("./inputs/day7.txt").readline().split(",")]
from statistics import median, mean

# -----------*** Part 1 ***-----------

best_position = int(median(positions))
fuel_cost = [abs(x - best_position) for x in positions]
print(f"Part 1: {sum(fuel_cost)}")

# -----------*** Part 2 ***-----------


def sum_of_first_integers(n):
    return n * (n + 1) // 2


max = max(positions) + 1
# brute-force:
min_cost = float("inf")
for pos in range(0, max):
    tmp = sum([sum_of_first_integers(abs(x - pos)) for x in positions])
    if tmp < min_cost:
        min_cost = tmp

print(f"Part 2: {min_cost}")
