from typing import List, Tuple, Dict

DEBUG = False
DAY = 12


def get_input():
    if DEBUG:
        with open(str(DAY) + '/test') as f:
            return f.readlines()
    else:
        with open(str(DAY) + '/input') as f:
            return f.readlines()


def evolve(rules: Dict[str, str], llcrr: str) -> str:
    return rules.get(llcrr, '.')


def main():
    inp = get_input()
    state = {i: v for i, v in enumerate(inp[0].split()[-1])}
    rules = {s[0]: s[1] for s in [row.strip().split(' => ')
                                  for row in inp[2:]]}
    generations = 500
    old_state = state
    diffs = []
    for g in range(generations + 1):
        if g % 100 == 0:
            diffs.append(sum([k for k, v in old_state.items() if v == '#']))
        state = {}
        keys = list(old_state.keys())
        for k in range(min(keys) - 2, max(keys) + 3):
            llcrr = ''.join([old_state.get(i, '.') for i in range(k-2, k+3)])
            state[k] = evolve(rules, llcrr)
        old_state = state
    diff = (diffs[5] - diffs[4]) // 100
    ans = (50000000000 - generations) * diff + diffs[-1]
    print("Sum: " + str(ans))
    return


if __name__ == "__main__":
    main()
