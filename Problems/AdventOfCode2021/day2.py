# Would be great for Python 3.10 match statement
# Still on 3.9 though...

commands = [
    (line.split()[0], int(line.split()[1])) for line in open("./inputs/day2.txt")
]

# -----------*** Parts 1&2 ***-----------

horizontal = 0
depth = 0
# Used for part 2 only
aim = 0
for direction, distance in commands:
    if direction == "forward":
        horizontal += distance
        # Used for part 2 only
        depth += aim * distance
    elif direction == "down":
        aim += distance
    elif direction == "up":
        aim -= distance
    else:
        raise ValueError(f"Unknown direction {direction}")

# After reading part 2, first result is equivalent to this:
print(f"Part 1: {horizontal * aim}")
# Updated formula
print(f"Part 2: {horizontal * depth}")
