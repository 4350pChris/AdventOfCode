type Answer = { increases: number[]; result: number; lines: number[] };

export function part1(lines: number[]): Answer {
	const increases: number[] = [];
	const res = lines.reduce(
		([count, prev], current, i) => {
			const increase = current > prev;
			if (increase) {
				increases.push(i);
			}
			return [increase ? count + 1 : count, current];
		},
		[0, Infinity]
	);
	return {
		lines,
		increases,
		result: res[0]
	};
}

export function part2(lines: number[]): Answer {
	const windows: number[][] = [];
	for (const n of lines) {
		windows.push([n]);
		const wLen = windows.length - 1;
		if (windows[wLen - 1]) {
			windows[wLen - 1].push(n);
		}
		if (windows[wLen - 2]) {
			windows[wLen - 2].push(n);
		}
	}
	// remove last 2 entries since they contain only 2 and 1 item respectively
	windows.splice(-2, 2);
	const sums = windows.flatMap((w) => w.reduce((acc, val) => acc + val, 0));
	return part1(sums);
}
