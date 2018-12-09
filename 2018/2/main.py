def getInput():
    with open('2/input') as f:
        return f.readlines()


def getOccurenceDict(line: str) -> dict:
    return {char: line.count(char) for char in line}


def part1():
    input = getInput()
    parsed = [getOccurenceDict(l) for l in input]
    two = [p for p in parsed if 2 in p.values()]
    three = [p for p in parsed if 3 in p.values()]
    return len(two) * len(three)


def isCorrect(first: str, second: str) -> bool:
    return sum((1 for f, s in zip(first, second) if f != s)) == 1


def part2():
    input = getInput()
    for i in range(len(input)):
        f = input[i]
        for j in range(i + 1, len(input)):
            s = input[j]
            if isCorrect(f, s):
                return ''.join([c for c, x in zip(f, s) if c == x])


def main():
    print(part1())
    print(part2())


if __name__ == "__main__":
    main()
