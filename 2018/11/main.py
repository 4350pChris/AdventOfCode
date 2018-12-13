import numpy as np
from math import log10

DEBUG = False
DAY = 11


def get_input():
    if DEBUG:
        return 18
    else:
        return 3628


def make_transformer(serial):
    def transform(x, y):
        rack_id = x + 10
        power_level = rack_id * y
        power_level += serial
        power_level *= rack_id
        hundreds = power_level % 1000 // 100
        return hundreds - 5
    return transform


def part1():
    serial = get_input()
    transformer = make_transformer(serial)
    grid = np.fromfunction(transformer, (300, 300))
    windows = sum(grid[x:x-3 or None, y:y-3 or None]
                  for x in range(3) for y in range(3))
    m = int(windows.max())
    loc = np.where(windows == m)
    return (loc[0][0], loc[1][0])


def part2():
    serial = get_input()
    transformer = make_transformer(serial)
    grid = np.fromfunction(transformer, (300, 300))
    curr_max = (-1, -1)
    loc = []
    for width in range(1, 300):
        windows = sum(grid[x:x-width or None, y:y-width or None]
                      for x in range(width) for y in range(width))
        m = int(windows.max())
        if m > curr_max[0]:
            curr_max = (m, width)
            loc = np.where(windows == m)
        if m < -1000:
            # max becomes negative as width gets too large, so quit early
            break
    return (loc[0][0], loc[1][0], curr_max[1])


if __name__ == "__main__":
    print(part1())
    print(part2())
