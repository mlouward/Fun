import numpy as np

# Use a numpy array for more efficient transposition
bits = np.array(
    [
        list(map(np.int16, list(line.strip())))
        for line in open("./inputs/day3.txt", "r")
    ],
    dtype=np.int8,
)

# -----------*** Part 1 ***-----------

# transpose the matrix to get array of each column
transpose_bits = bits.T
print(transpose_bits.shape)
# 12 lines, 1000 columns
# Get most common value in each row and concatenate into a string
gamma = ""
for i, row in enumerate(transpose_bits):
    gamma += str(np.bincount(row).argmax())
print("Gamma:", gamma)

# compute xor of gamma and 111111111111 in binary
epsilon = np.binary_repr(int(gamma, 2) ^ int("1" * len(bits[0]), 2), width=12)
print("Epsil:", epsilon)

# Print the final answer
print("Answer:", int(gamma, base=2) * int(epsilon, base=2))

# -----------*** Part 2 ***-----------
oxygen = bits.copy()
co2 = bits.copy()
for i in range(len(transpose_bits)):
    # If both arrays are of size 1, we are finished
    if co2.shape[0] <= 1 and oxygen.shape[0] <= 1:
        break

    # get most/least common digit for oxygen and co2
    zeros_ones_count_co2 = np.bincount(co2[:, i])
    if zeros_ones_count_co2[0] > zeros_ones_count_co2[1]:
        least_common = 1
    else:
        # if counts are equal, default to 0
        least_common = 0
    zeros_ones_count_oxygen = np.bincount(oxygen[:, i])
    if zeros_ones_count_oxygen[0] > zeros_ones_count_oxygen[1]:
        most_common = 0
    else:
        # if counts are equal, default to 1
        most_common = 1
    # For oxygen, we want to filter bits that are the most common
    if oxygen.shape[0] > 1:
        oxygen = oxygen[oxygen[:, i] == int(most_common)].copy()
    # For co2, we want to filter bits that are the least common
    if co2.shape[0] > 1:
        co2 = co2[co2[:, i] == int(least_common)].copy()

oxygen_generator_rating = "".join([str(x) for x in oxygen[0]])
print("Oxygen Generator Rating:", oxygen_generator_rating)

co2_generator_rating = "".join([str(x) for x in co2[0]])
print("Co2 Generator Rating:", co2_generator_rating)

print(
    "Answer:", int(oxygen_generator_rating, base=2) * int(co2_generator_rating, base=2)
)
