from numpy import sign


signals = []
with open("./inputs/day8.txt") as f:
    for line in f:
        inputs, outputs = line.strip().split("|")
        inputs = inputs.strip().split()
        outputs = outputs.strip().split()
        signals.append((inputs, outputs))

# -----------*** Part 1 ***-----------

# Count occurences of length 2/3/4/7 in output signals
print(
    "Part 1: {}".format(
        len(
            [
                chars
                for signal in signals
                for chars in signal[1]  # only output signals
                if len(chars) in (2, 4, 3, 7)  # filter length
            ]
        )
    )
)

# -----------*** Part 2 ***-----------

test = signals[0][0]
# mapping of digits on the quadrant to their corresponding chars
digit_to_letters = {
    0: set("abcefg"),
    1: set("cf"),
    2: set("acdeg"),
    3: set("acdfg"),
    4: set("bcdf"),
    5: set("abdfg"),
    6: set("abdefg"),
    7: set("acf"),
    8: set("abcdefg"),
    9: set("abcdfg"),
}
length_to_digit = {2: [1], 3: [7], 4: [4], 5: [2, 3, 5], 6: [0, 6, 9], 7: [8]}
default_letter_to_actual_letter = {
    "a": set(),
    "b": set(),
    "c": set(),
    "d": set(),
    "e": set(),
    "f": set(),
    "g": set(),
}

print(test)
test = sorted(test, key=lambda x: (len(x)))
# todo
