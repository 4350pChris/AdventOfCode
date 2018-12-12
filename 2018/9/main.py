from collections import deque, defaultdict

DEBUG = False
DAY = 9


def get_input():
    if DEBUG:
        return [
            '10 players; last marble is worth 1618 points',
            '13 players; last marble is worth 7999 points',
            '9 players; last marble is worth 25 points'
        ]
    else:
        with open(str(DAY) + '/input') as f:
            return f.readlines()


def get_parsed_input():
    txt = get_input()[0].split()
    return (int(txt[0]), int(txt[-2]))


def part1():
    players, marbles = get_parsed_input()
    circle = deque([0])
    max_score = 0
    scores = defaultdict(int)
    for i in range(1, (marbles * 100) + 1):
        if i % 23 == 0:
            circle.rotate(7)
            scores[i % players] += i + circle.pop()
            circle.rotate(-1)
        else:
            circle.rotate(-1)
            circle.append(i)
    return max(scores.values())


def part2():
    pass


if __name__ == "__main__":
    print(part1())
    print(part2())
