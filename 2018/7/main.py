from typing import List, Dict, Tuple

DEBUG = False
DAY = 7

Steps = Dict[str, List[str]]


class Worker:
    def __init__(self):
        self.step = ''
        self.time_to_finish = 0

    def assign_step(self, step: str):
        self.step = step
        self.time_to_finish = ord(step) - ord('A') + 1 + (0 if DEBUG else 60)

    def tick(self):
        self.time_to_finish -= 1

    def is_idle(self):
        return self.time_to_finish <= 0


def get_input():
    if DEBUG:
        return ['Step C must be finished before step A can begin.',
                'Step C must be finished before step F can begin.',
                'Step A must be finished before step B can begin.',
                'Step A must be finished before step D can begin.',
                'Step B must be finished before step E can begin.',
                'Step D must be finished before step E can begin.',
                'Step F must be finished before step E can begin.']
    else:
        with open(str(DAY) + '/input') as f:
            return f.readlines()


def parse_line(line: str) -> Tuple[str, str]:
    spl = line.split()
    return (spl[1], spl[7])


def add_step(steps: Steps, parent: str, child: str) -> Steps:
    if child in steps:
        steps[child].append(parent)
        steps[child].sort()
    else:
        steps[child] = [parent]
    if parent not in steps:
        steps[parent] = []


def get_ready_steps(steps: Steps) -> str:
    return [k for k, v in steps.items() if not v]


def reduce_steps(steps: Steps, next_step: str) -> Steps:
    steps.pop(next_step)
    for k, v in steps.items():
        if next_step in v:
            v.remove(next_step)
    return next_step


def part1():
    parsed = [parse_line(l) for l in get_input()]
    steps = {}
    for p, s in parsed:
        add_step(steps, p, s)

    sol = ''
    while steps:
        ready_steps = get_ready_steps(steps)
        ready_steps.sort()
        next_step = ready_steps[0]
        sol += reduce_steps(steps, next_step)
    return sol


def tick(steps: Steps, workers: List[Worker]) -> None:
    ready_steps = get_ready_steps(steps)
    ready_steps.sort(reverse=True)
    for i, worker in enumerate(workers):
        if ready_steps and worker.is_idle():
            next_step = ready_steps.pop()
            worker.assign_step(next_step)
            steps.pop(next_step)

    for worker in workers:
        worker.tick()
        if worker.is_idle() and worker.step:
            for v in steps.values():
                if worker.step in v:
                    v.remove(worker.step)


def part2():
    parsed = [parse_line(l) for l in get_input()]
    steps = {}
    for p, s in parsed:
        add_step(steps, p, s)

    worker_no = 2 if DEBUG else 5
    workers = [Worker() for i in range(worker_no)]
    ticks = 0
    while steps or not all((w.is_idle() for w in workers)):
        ticks += 1
        tick(steps, workers)
    return ticks


if __name__ == "__main__":
    print(part1())
    print(part2())
