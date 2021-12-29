import numpy as np

with open("./inputs/day4.txt", "r") as f:
    # First line is the list of drawn numbers
    drawn_numbers = map(int, f.readline().strip().split(","))
    # 100 boards of size 5*5 in the input file
    boards = np.empty((100, 5, 5), dtype=np.int8)

    # Starts with an empty line, and every 6 lines is a new board
    # We 'consume' the empty line to check for end of file
    board_nb = 0
    while f.readline():
        for i in range(5):
            boards[board_nb][i] = np.array(list(map(int, f.readline().strip().split())))
        board_nb += 1

# -----------*** Part 1 ***-----------


def part_1(boards, drawn_numbers):
    # Read drawn numbers in order, check if any row or columns in the array contains all the numbers
    # If so, we have a bingo and we can stop

    # Create a mask of zeros with the same shape as the boards
    boards_mask = np.zeros_like(boards)
    for number in drawn_numbers:
        # Update the mask where the number is present
        boards_mask += boards == number
        # check 5 rows of each mask for bingo and return index of board
        for i in range(5):
            if np.any(np.all(boards_mask[:, i], axis=1)):
                bingo_board_index = np.where(np.all(boards_mask[:, i], axis=1))[0]
                print("BINGO ({}) in row of board {}".format(number, bingo_board_index))
                return number, bingo_board_index, boards_mask
        # check columns for bingo and return index of board + last number drawn
        if np.any(np.all(boards_mask, axis=1)):
            bingo_board_index = np.where(np.all(boards_mask, axis=1))[0]
            print("BINGO ({}) in column of board {}".format(number, bingo_board_index))
            return number, bingo_board_index, boards_mask


number, bingo_board_index, boards_mask = part_1(boards, drawn_numbers)
# We get the following winning board
print("Winning board:\n", boards[bingo_board_index][0])
# With this mask:
print("Winning board mask:\n", boards_mask[bingo_board_index][0])
# which means the unmarked number are these ones:
unmarked_nbs = boards[bingo_board_index][0][boards_mask[bingo_board_index][0] == 0]
# print(unmarked_nbs)

print("Part 1: {}".format(unmarked_nbs.sum() * number))

# -----------*** Part 2 ***-----------
