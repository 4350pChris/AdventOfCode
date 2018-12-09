from typing import List


def getInput():
    with open('2' + '/input') as f:
        return f.readlines()


def parseLine(input: str) -> (int, int, int):
    return [int(x) for x in input.split('x')]


def getWrapping(dimensions: (int, int, int)) -> int:
    l, w, h = dimensions
    lw = l*w
    wh = w*h
    lh = l*h
    return sum([2*lw, 2*wh, 2*lh, min([lw, wh, lh])])


def part1():
    input = getInput()
    parsed = [parseLine(l) for l in input]
    res = 0
    for p in parsed:
        res += getWrapping(p)
    return res


def get_ribbon(dimensions: (int, int, int)) -> int:
    l, w, h = dimensions
    x, y = sorted(dimensions)[:2]
    return 2*(x + y) + (l * w * h)


def part2():
    input = getInput()
    parsed = [parseLine(l) for l in input]
    return sum((get_ribbon(p) for p in parsed))


def main():
    # print(part1())
    print(part2())


if __name__ == "__main__":
    main()
