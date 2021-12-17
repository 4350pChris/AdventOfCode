export type Position = [x: number, y: number];
export type Pipe = [start: Position, end: Position];
export type Field = number[][];
export type Result = Generator<
	| number
	| {
			intersections: Position[];
			pipe: Pipe;
	  },
	void,
	unknown
>;

function makePipe(line: string): Pipe {
	const [left, right] = line.split(' -> ');
	const start = left.split(',').map((n) => Number(n)) as Position;
	const end = right.split(',').map((n) => Number(n)) as Position;
	return [start, end];
}

function getSlope(pipe: Pipe) {
	const [start, end] = pipe;
	return (end[1] - start[1]) / (end[0] - start[0]);
}

function pointIsOnLine(point: Position, pipe: Pipe) {
	const [start, end] = pipe;
	const minX = Math.min(start[0], end[0]);
	const maxX = Math.max(start[0], end[0]);
	const minY = Math.min(start[1], end[1]);
	const maxY = Math.max(start[1], end[1]);

	// define plane that the pipe inhabits
	if (minX <= point[0] && point[0] <= maxX && minY <= point[1] && point[1] <= maxY) {
		// (y2 - y1) / (x2 - x1)
		const slope = getSlope(pipe);
		// horizontal or vertical pipe - the plane is actually a line, therefore the point is on it
		if (slope === -Infinity || slope === Infinity || slope === 0) {
			return true;
		}
		// make point's coordinates relative to slope encompassed by pipe
		const [px, py]: Position = [point[0] - minX, point[1] - minY];
		// a positive slope means the line goes from (minX|minY) to (maxX, maxY)
		// a negative one means it's (minX|maxY) to (maxX| minY)
		// when negative the function is y = m*x + maxY
		return py === slope * px + (slope < 0 ? maxY - minY : 0);
	}
	return false;
}

function makeField(pointFn: (point: Position) => number = () => 0): Field {
	return Array.from(Array(1000)).map((_, y) =>
		Array.from(Array(1000)).map((_, x) => pointFn([Number(x), Number(y)]))
	);
}

function getIntersectedField(field: Field, pipe: Pipe): Field {
	return field.map((row, y) => row.map((n, x) => n + Number(pointIsOnLine([x, y], pipe))));
}

function getIntersections(field: Field): Position[] {
	return field.flatMap(
		(row, y) =>
			row.reduce((rowIntersections, count, x) => {
				if (count > 1) {
					const pos: Position = [x, y];
					return [...rowIntersections, pos];
				}
				return rowIntersections;
			}, [] as Position[]),
		[] as Position[]
	);
}

function* main(pipes: Pipe[]): Result {
	let intersections: Position[];
	let field: Field = makeField();
	for (const pipe of pipes) {
		field = getIntersectedField(field, pipe);
		intersections = getIntersections(field);
		yield { intersections, pipe };
	}
	yield intersections.length;
}

export function part1(lines: string[]): Result {
	const p = lines
		.map((line) => makePipe(line))
		.filter((p) => [-Infinity, 0, Infinity].includes(getSlope(p)));
	return main(p);
}

export function part2(lines: string[]): Result {
	const p = lines.map((line) => makePipe(line));
	return main(p);
}
