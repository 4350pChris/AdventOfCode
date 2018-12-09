import hashlib

DEBUG = False
DAY = '4'


def get_input():
    if DEBUG:
        return 'abcdef'
    else:
        return 'iwrupvqb'


def part1():
    s = get_input()
    n = 0
    while True:
        test = s + str(n)
        m = hashlib.md5()
        m.update(test.encode('UTF-8'))
        h = m.hexdigest()
        if h.startswith('000000'):
            return n
        n += 1


def part2():
    pass


if __name__ == "__main__":
    print(part1())
    print(part2())
