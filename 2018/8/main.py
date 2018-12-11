from typing import Tuple, List

DEBUG = False
DAY = 8


def get_input():
    if DEBUG:
        return ['2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2']
    else:
        with open(str(DAY) + '/input') as f:
            return f.readlines()


def get_parsed_input() -> List[int]:
    return list(map(int, get_input()[0].split()))


class Node:
    def __init__(self, children: List, metadata: List[int]):
        self.children = children
        self.metadata = metadata

    def sum_metadata(self):
        s = sum(self.metadata)
        if self.children:
            s += sum([n.sum_metadata() for n in self.children])
        return s

    def sum_indexed_metadata(self):
        s = 0
        if self.children:
            for i in map(lambda x: x-1, self.metadata):
                if i < len(self.children):
                    n = self.children[i]
                    s += n.sum_indexed_metadata()
        else:
            s = sum(self.metadata)
        return s


def build_node(data: List[int]) -> Tuple[Node, List[int]]:
    children_no, metadata_length, *tail = data
    children = []
    for i in range(children_no):
        n, tail = build_node(tail)
        children.append(n)
    metadata = tail[:metadata_length]
    tail = tail[metadata_length:]
    return (Node(children, metadata), tail)


def part1():
    l = get_parsed_input()
    root, _ = build_node(l)
    return root.sum_metadata()


def part2():
    l = get_parsed_input()
    root, _ = build_node(l)
    return root.sum_indexed_metadata()


if __name__ == "__main__":
    print(part1())
    print(part2())
