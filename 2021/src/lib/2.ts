enum TYPE {
	DEPTH,
	POSITION
}

type SingleArg = (val: number) => number;
type TwoArgs = (pos: number, aim: number) => { position: number; depth: number };
export type GeneratorResult = { depth: number; aim?: number; horizontal: number };

function getFnFromLinePart1(line: string): [t: TYPE, fn: SingleArg] {
	const [start, n] = line.split(' ');
	switch (start) {
		case 'forward':
			return [TYPE.POSITION, (val: number) => val + Number(n)];
		case 'down':
			return [TYPE.DEPTH, (val: number) => val + Number(n)];
		case 'up':
			return [TYPE.DEPTH, (val: number) => val - Number(n)];
	}
}

export function* part1(lines: string[]): Generator<GeneratorResult> {
	let horizontal = 0;
	let depth = 0;
	for (const line of lines) {
		const [t, fn] = getFnFromLinePart1(line);
		if (t === TYPE.POSITION) {
			horizontal = fn(horizontal);
		} else {
			depth = fn(depth);
		}
		yield { depth, horizontal };
	}
}

function getFnFromLinePart2(line: string): [t: TYPE, fn: SingleArg | TwoArgs] {
	const [start, n] = line.split(' ');
	switch (start) {
		case 'forward':
			return [
				TYPE.POSITION,
				(pos: number, aim: number) => ({
					position: pos + Number(n),
					depth: Number(n) * aim
				})
			];
		case 'down':
			return [TYPE.DEPTH, (val: number) => val + Number(n)];
		case 'up':
			return [TYPE.DEPTH, (val: number) => val - Number(n)];
	}
}

export function* part2(lines: string[]): Generator<GeneratorResult> {
	let horizontal = 0;
	let aim = 0;
	let depth = 0;
	for (const line of lines) {
		const [t, fn] = getFnFromLinePart2(line);
		if (t === TYPE.POSITION) {
			const { position, depth: d } = fn(horizontal, aim) as { position: number; depth: number };
			horizontal = position;
			depth += d;
		} else {
			aim = (fn as SingleArg)(aim);
		}
		yield { depth, aim, horizontal };
	}
}
