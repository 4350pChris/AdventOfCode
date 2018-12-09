def getInput():
    with open('3/input') as f:
        lines = f.readlines()
        res = []
        for line in lines:
            spl = line.split()
            number = spl[0][1:]
            offset = tuple((int(x) for x in spl[2].rstrip(':').split(',')))
            size = tuple((int(x) for x in spl[3].split('x')))
            res.append((number, offset, size))
        return res


def fillGrid(grid: list, line: tuple) -> list:
    number, offset, size = line
    colOffset, rowOffset = offset
    width, height = size
    for row in range(rowOffset, rowOffset + height):
        for col in range(colOffset, colOffset + width):
            cell = grid[row][col]
            cell.add(int(number))
    return grid


def getFilledGrid(input):
    grid = [[set() for x in range(1000)] for x in range(1000)]
    for line in input:
        grid = fillGrid(grid, line)
    return grid


def part1():
    input = getInput()
    grid = getFilledGrid(input)

    s = 0
    for row in grid:
        for cell in row:
            if len(cell) > 1:
                s += 1
    return s


def part2():
    input = getInput()
    grid = getFilledGrid(input)
    non_overlapping = set(i for i in range(1, len(input)))

    for row in grid:
        for cell in row:
            if len(cell) > 1:
                non_overlapping.difference_update(cell)

    return non_overlapping


def main():
    # print(part1())
    print(part2())


if __name__ == "__main__":
    main()
