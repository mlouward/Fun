from typing import Iterable, Generator

# -----------*** Part 1 ***-----------
depths = [int(line.strip()) for line in open("./inputs/day1.txt")]

# Count the number of times the value increases in an iterable
def count_increases(iterable: Iterable[int]) -> int:
    """
    Counts the number of times the value increases in an iterable
    >>> count_increases([1, 5, 2, 4, 1])
        2
    >>> count_increases([1, 5, 6, 4, 5])
        3
    """
    previous = -1
    count = 0
    for item in iterable:
        if item > previous:
            count += 1
        previous = item
    return count - 1


print(count_increases(depths))
# 1713

# -----------*** Part 2 ***-----------
from itertools import islice
import collections

# From https://docs.python.org/3/library/itertools.html#itertools.islice
def sliding_window_sum(iterable, n) -> Generator[int, None, None]:
    """
    Returns an iterable over the sum of values on a sliding window
    of size n over the iterable
    >>> list(sliding_window_sum([1, 2, 3, 4, 1], 2))
        [3, 5, 7, 4]
    """
    # sliding_window('ABCDEFG', 4) -> ABCD BCDE CDEF DEFG
    it = iter(iterable)
    window = collections.deque(islice(it, n), maxlen=n)
    if len(window) == n:
        yield sum(window)
    for x in it:
        window.append(x)
        yield sum(window)


print(count_increases(list(sliding_window_sum(depths, 3))))
# 1734
