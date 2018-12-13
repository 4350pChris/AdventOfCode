import numpy as np
import scipy.spatial.distance as distance
import matplotlib.lines
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import re


DEBUG = False
DAY = 10


def get_input():
    if DEBUG:
        return [
            'position = < 9,  1 > velocity = < 0,  2 >',
            'position = < 7,  0 > velocity = <-1,  0 >',
            'position = < 3, -2 > velocity = <-1,  1 >',
            'position = < 6, 10 > velocity = <-2, -1 >',
            'position = < 2, -4 > velocity = < 2,  2 >',
            'position = <-6, 10 > velocity = < 2, -2 >',
            'position = < 1,  8 > velocity = < 1, -1 >',
            'position = < 1,  7 > velocity = < 1,  0 >',
            'position = <-3, 11 > velocity = < 1, -2 >',
            'position = < 7,  6 > velocity = <-1, -1 >',
            'position = <-2,  3 > velocity = < 1,  0 >',
            'position = <-4,  3 > velocity = < 2,  0 >',
            'position = <10, -3 > velocity = <-1,  1 >',
            'position = < 5, 11 > velocity = < 1, -2 >',
            'position = < 4,  7 > velocity = < 0, -1 >',
            'position = < 8, -2 > velocity = < 0,  1 >',
            'position = <15,  0 > velocity = <-2,  0 >',
            'position = < 1,  6 > velocity = < 1,  0 >',
            'position = < 8,  9 > velocity = < 0, -1 >',
            'position = < 3,  3 > velocity = <-1,  1 >',
            'position = < 0,  5 > velocity = < 0, -1 >',
            'position = <-2,  2 > velocity = < 2,  0 >',
            'position = < 5, -2 > velocity = < 1,  2 >',
            'position = < 1,  4 > velocity = < 2,  1 >',
            'position = <-2,  7 > velocity = < 2, -2 >',
            'position = < 3,  6 > velocity = <-1, -1 >',
            'position = < 5,  0 > velocity = < 1,  0 >',
            'position = <-6,  0 > velocity = < 2,  0 >',
            'position = < 5,  9 > velocity = < 1, -2 >',
            'position = <14,  7 > velocity = <-2,  0 >',
            'position = <-3,  6 > velocity = < 2, -1 >'
        ]
    else:
        with open(str(DAY) + '/input') as f:
            return f.readlines()


def get_parsed_input() -> ([[int, int]], [[int, int]]):
    inp = get_input()
    pattern = r'<(.*?)>'
    matches = [re.findall(pattern, s) for s in inp]
    points = []
    velocities = []
    for match in matches:
        for arr, s in zip([points, velocities], match):
            spl = s.split(',')
            arr.append([int(x) for x in spl])
    return (points, velocities)


points, velocities = [np.array(x) for x in get_parsed_input()]


def part1():
    global points
    fig, ax = plt.subplots()
    ln, = plt.plot([], [], 'ro', animated=True)
    r = 10658
    for i in range(r):
        points += velocities

    def init():
        global points
        size = 1200
        ax.set_xlim([140, 220])
        ax.set_ylim([120, 80])
        x = points[:, 0]
        y = points[:, 1]
        ln.set_data(x, y)
        return ln,

    def update(frame):
        global points
        points += velocities
        x = points[:, 0]
        y = points[:, 1]
        ln.set_data(x, y)
        return ln,

    ani = animation.FuncAnimation(
        fig, update, init_func=init, frames=np.linspace(1, 200, num=200), blit=True)
    plt.show()

    return points


def part2():
    pass


if __name__ == "__main__":
    print(part1())
    print(part2())
