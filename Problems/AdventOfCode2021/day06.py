# -----------*** Part 1 ***-----------

initial_state = [int(x) for x in open("./inputs/day6.txt").readline().split(",")]

next_step = initial_state[:]
for i in range(1, 80 + 1):
    if i % 10 == 0:
        print(i)
    for j in range(len(next_step)):
        if next_step[j] == 0:
            next_step[j] = 6
            next_step.append(8)
        else:
            next_step[j] -= 1

print(len(next_step))

# -----------*** Part 2 ***-----------

# This time, we only keep the count of each fish state (0 to 8)
# We use it to get the count of each fish state after each iteration
from collections import Counter, defaultdict

# count occurences of each digit in initial state
occurences = Counter(initial_state)
for i in range(1, 256 + 1):
    # initialize empty dict at each iteration
    next_count = defaultdict(int)
    for days_left in occurences:
        if days_left > 0:
            # for all other fishes, decrease cycle by 1
            next_count[days_left - 1] += occurences[days_left]
        else:
            # restart the cycle for the fishes that gave birth
            next_count[6] += occurences[0]
            # add the newborn fishes
            next_count[8] += occurences[0]
    # save results for next iteration
    occurences = next_count.copy()

print(sum(occurences.values()))
