from typing import List, Tuple, Callable, Dict
import datetime

debug = False


def getInput() -> List[str]:
    if debug:
        return test_input().split('\n')
    else:
        with open('4/input') as f:
            return f.readlines()


def parse_line(line: str) -> Tuple[datetime.datetime, str]:
    time, action = line.strip('[\n').split('] ')
    parsed_time = datetime.datetime.strptime(time, '%Y-%m-%d %H:%M')
    return (parsed_time, action)


def parse_input() -> List[Tuple[datetime.datetime, str]]:
    input = getInput()
    parsed = [parse_line(l) for l in input]
    return sorted(parsed, key=lambda t: t[0])


def get_guards(input: List[Tuple[datetime.datetime, str]]) -> Dict[int, Tuple[int, int]]:
    guards = {}
    start = 0
    end = 0
    for line in input:
        txt = line[1]
        if txt.startswith('Guard'):
            n = int(txt.split(' ')[1].strip('#'))
            if n not in guards:
                guards[n] = []
        elif txt.startswith('falls'):
            start = line[0].minute
        elif txt.startswith('wakes'):
            end = line[0].minute
            shift = (start, end)
            guards[n].append(shift)
    return guards


def get_sleep_by_minutes(shifts: List[Tuple[int, int]]) -> Dict[int, int]:
    minutes = {m: 0 for m in range(0, 60)}
    for start, end in shifts:
        for m in range(start, end):
            minutes[m] += 1
    return minutes


def part1():
    input = parse_input()
    guards = get_guards(input)
    sleep_minutes = {n: get_sleep_by_minutes(shifts)
                     for n, shifts in guards.items()}
    n, max_minutes_asleep = max(((n, sum(minutes.values()))
                                 for n, minutes in sleep_minutes.items()), key=lambda x: x[1])
    minute_most_asleep, time_asleep = max(
        sleep_minutes[n].items(), key=lambda x: x[1])
    return (n * minute_most_asleep)


def part2():
    input = parse_input()
    guards = get_guards(input)
    sleep_minutes = {n: get_sleep_by_minutes(shifts)
                     for n, shifts in guards.items()}
    most_asleep = (0, 0, 0)
    for n, minutes in sleep_minutes.items():
        minute, total = max(minutes.items(), key=lambda x: x[1])
        if total > most_asleep[2]:
            most_asleep = (n, minute, total)
    return most_asleep


def test_input():
    return """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""


def main():
    print(part1())
    print(part2())


if __name__ == "__main__":
    main()
