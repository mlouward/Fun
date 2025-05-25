# -----------*** Part 1 ***-----------

def syntax_error_score(lines):
    bracket_pairs = {'(': ')', '[': ']', '{': '}', '<': '>'}
    error_scores = {')': 3, ']': 57, '}': 1197, '>': 25137}
    total_score = 0
    for line in lines:
        stack = []
        for char in line.strip():
            if char in bracket_pairs:
                stack.append(char)
            elif char in error_scores:
                if not stack or bracket_pairs[stack[-1]] != char:
                    total_score += error_scores[char]
                    break
                else:
                    stack.pop()
    return total_score

# Sample input from the problem description
sample_lines = [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "((((<>{}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
]
# -----------*** Part 2 ***-----------

def autocomplete_middle_score(lines):
    bracket_pairs = {'(': ')', '[': ']', '{': '}', '<': '>'}
    error_scores = {')', ']', '}', '>'}
    autocomplete_points = {')': 1, ']': 2, '}': 3, '>': 4}
    scores = []
    for line in lines:
        stack = []
        corrupted = False
        for char in line.strip():
            if char in bracket_pairs:
                stack.append(char)
            elif char in error_scores:
                if not stack or bracket_pairs[stack[-1]] != char:
                    corrupted = True
                    break
                else:
                    stack.pop()
        if not corrupted and stack:
            completion = [bracket_pairs[ch] for ch in reversed(stack)]
            score = 0
            for c in completion:
                score = score * 5 + autocomplete_points[c]
            scores.append(score)
    scores.sort()
    return scores[len(scores)//2] if scores else 0

if __name__ == "__main__":
    with open("./inputs/day10.txt") as f:
        lines = f.readlines()
    print("Sample syntax error score:", syntax_error_score(lines))
    print("Sample autocomplete middle score:", autocomplete_middle_score(lines))
