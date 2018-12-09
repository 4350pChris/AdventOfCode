from typing import List, Dict, Tuple
import numpy as np
from scipy import sparse, spatial
from string import ascii_letters

DEBUG = True
DAY = '6'


def get_input() -> List[Tuple[int, int]]:
    if DEBUG:
        out = ['1, 1', '1, 6', '8, 3', '3, 4', '5, 5', '8, 9']
    else:
        with open(DAY + '/input') as f:
            out = f.readlines()

    points = [tuple((map(int, line.split(', ')))) for line in out]
    return points


def get_sparse_matrix(points: List[Tuple[int, int]]) -> sparse.coo_matrix:
    col_ind = np.array([col for col, row in points])
    row_ind = np.array([row for col, row in points])
    data = np.array([ord(ascii_letters[i])
                     for i in range(len(points))], dtype=int)
    return sparse.coo_matrix((data, (row_ind, col_ind)))


def get_distances(node: Tuple[int, int], nodes: sparse.csc_matrix) -> List[int]:
    non_zero = [coord for coord in zip(*nodes.nonzero())]
    distances = spatial.distance.cdist(
        [node], non_zero, metric='cityblock').flatten().tolist()
    return distances


def closest_node_value(node: Tuple[int, int], nodes: sparse.csc_matrix) -> str:
    distances = get_distances(node, nodes)
    winner = np.argwhere(distances == np.min(distances)).flatten().tolist()
    if len(winner) == 1:
        coord = non_zero[winner[0]]
        return chr(nodes[coord])
    else:
        return '.'


def part1():
    points = get_input()
    sparse = get_sparse_matrix(points).tocsc()
    dense = np.asarray(sparse.toarray(), dtype=str)
    for coord, _ in np.ndenumerate(dense):
        v = closest_node_value(coord, sparse)
        dense[coord] = v
    borders = [dense[0], dense[-1], dense[:, 0], dense[:, -1]]
    infinite = set()
    for line in borders:
        for c in line:
            infinite.add(c)
    char, counts = np.unique(dense, return_counts=True)
    possible = {c: cnt for c, cnt in zip(char, counts) if c not in infinite}
    return max(possible.items(), key=lambda x: x[1])


def part2():
    points = np.loadtxt(DAY + '/input', delimiter=', ')
    xmin, ymin = points.min(axis=0) - 1
    xmax, ymax = points.max(axis=0) + 2
    xgrid, ygrid = np.meshgrid(np.arange(xmin, xmax), np.arange(ymin, ymax))
    targets = np.dstack([xgrid, ygrid]).reshape(-1, 2)
    cityblock = spatial.distance.cdist(points, targets, metric='cityblock')
    distances = cityblock.sum(axis=0)
    region = np.where(distances < 10000, 1, 0)
    return region.sum()


if __name__ == "__main__":
    # print(part1())
    print(part2())
