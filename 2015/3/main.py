from typing import List, Tuple

DEBUG = False
DAY = 3


def get_input() -> [str]:
    if DEBUG:
        return '^v^v^v^v^v'
    else:
        with open(str(DAY) + '/input') as f:
            return f.readline()


def get_move(token: str) -> Tuple[int, int]:
    if token == '^':
        return (-1, 0)
    elif token == '>':
        return (0, 1)
    elif token == 'v':
        return (1, 0)
    elif token == '<':
        return (0, -1)


def do_move(grid: List[List[int]], pos: Tuple[int, int], token: str) -> List[List[int]]:
    curr_row, curr_col = pos
    row_inc, col_inc = get_move(token)
    row = curr_row + row_inc
    col = curr_col + col_inc

    total_rows = len(grid)
    total_cols = len(max(grid, key=lambda row: len(row)))

    if row >= total_rows:
        grid.append([0 for i in range(total_cols)])
    elif row < 0:
        row = 0
        grid.insert(0, [0 for i in range(total_cols)])

    if col >= total_cols:
        for r in grid:
            r.append(0)
    elif col < 0:
        col = 0
        for r in grid:
            r.insert(0, 0)

    grid[row][col] += 1
    return (row, col)


def part1():
    input = get_input()
    grid = [[1]]
    pos = (0, 0)
    for token in input:
        pos = do_move(grid, pos, token)

    s = 0
    for row in grid:
        for house in row:
            if house > 0:
                s += 1
    return s


def part2():
    input = get_input()
    grid = [[1]]
    pos = (0, 0)
    orig = (0, 0)
    for santa_token in input[::2]:
        pos = do_move(grid, pos, santa_token)
        orig = tuple((x - y for x, y in zip(orig, get_move(santa_token))))

    pos = tuple((x + y for x, y in zip(pos, orig)))
    for robot_token in input[1::2]:
        pos = do_move(grid, pos, robot_token)

    s = 0
    for row in grid:
        for house in row:
            if house > 0:
                s += 1
    return s


if __name__ == "__main__":
    # print(part1())
    print(part2())
