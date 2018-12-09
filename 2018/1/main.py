def getInput():
    with open('1/input') as f:
        return [int(x) for x in f.readlines() if x is not '']


def part1():
    input = getInput()
    return sum(input)


def part2():
    input = getInput()
    inputLength = len(input)
    seen = set()
    s = 0
    i = 0
    while s not in seen:
        seen.add(s)
        n = input[i % inputLength]
        s += n
        i += 1
    return s


if __name__ == "__main__":
    p1 = part1()
    print(p1)
    p2 = part2()
    print(p2)
