import re

DEBUG = False
DAY = '5'


def get_input():
    if DEBUG:
        return ['ugknbfddgicrmopn', 'jchzalrnumimnmhp', 'haegwjzuvuyypxyu', 'dvszwmarrgswjxmb']
    else:
        with open(DAY + '/input') as f:
            return f.readlines()


def is_nice_one(s: str) -> bool:
    vowels = 'aeiou'
    vowels_in_word = list(filter(lambda c: c in vowels, s))
    if len(vowels_in_word) < 3:
        return False

    regex = r"([a-z])\1"
    m = re.finditer(regex, s)
    if len(list(m)) == 0:
        return False

    naughty = ['ab', 'cd', 'pq', 'xy']
    for n in naughty:
        if s.find(n) > -1:
            return False

    return True


def is_nice_two(s: str) -> bool:
    regex = r'([a-z])\1(?!\1)'
    m = re.finditer(regex, s)
    
    return True


def part1():
    words = get_input()
    return len(list(filter(is_nice_one, words)))


def part2():
    words = get_input()
    return len(list(filter(is_nice_two, words)))


if __name__ == "__main__":
    print(part1())
    print(part2())
