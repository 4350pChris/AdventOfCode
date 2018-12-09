from os import path


def getBrackets():
    with open('1' + '/input') as f:
        all = [bracket for bracket in f.readline()]
        return all


def part1():
    all = getBrackets()
    res = sum((1 if b == '(' else -1 for b in all))
    return res


def part2():
    all = getBrackets()
    level = 0
    for i, b in enumerate(all):
        if b == '(':
            level += 1
        else:
            level -= 1
        if level < 0:
            return i


def main():
    print(part1())
    print(part2())


if __name__ == "__main__":
    main()
