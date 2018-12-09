from string import ascii_lowercase

DEBUG = False


def get_input():
    if DEBUG:
        return 'dabAcCaCBAcCcaDA'
    else:
        with open('5/input') as f:
            return f.readline()


def reduce_string(input: str) -> str:
    i = 0
    while i < len(input) - 1:
        if input[i].swapcase() == input[i + 1]:
            input = input[:i] + input[i + 2:]
            i -= 1
        else:
            i += 1
    return input.rstrip('\n')


def part1():
    input = get_input()
    res = reduce_string(input)
    return len(res)


def part2():
    input = get_input()
    res = reduce_string(input)
    tries = [''.join((c for c in res if c.lower() != alpha))
             for alpha in ascii_lowercase]
    reduced = [reduce_string(s) for s in tries]
    return min(map(len, reduced))


def main():
    # print(part1())
    print(part2())


if __name__ == "__main__":
    main()
