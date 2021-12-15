function mostCommonBit(bucket: string[]) {
	const ones = bucket.filter((v) => v === '1');
	return ones.length >= bucket.length / 2 ? '1' : '0';
}

function leastCommonBit(bucket: string[]) {
	const zeroes = bucket.filter((v) => v === '0');
	return zeroes.length <= bucket.length / 2 ? '0' : '1';
}

export function invertBits(n: string[]): Array<'1' | '0'> {
	return n.map((v) => (v === '1' ? '0' : '1'));
}

export function* part1(lines: string[]): Generator<Array<'1' | '0'>, number, unknown> {
	const buckets = Array(12)
		.fill(null)
		.map(() => []);
	lines.forEach((bit) => Array.from(bit).forEach((b, i) => buckets[i].push(b)));
	const mostCommon: Array<'1' | '0'> = [];
	for (const bucket of buckets) {
		mostCommon.push(mostCommonBit(bucket));
		yield mostCommon;
	}
	const gamma = parseInt(mostCommon.join(''), 2);
	const epsilon = parseInt(invertBits(mostCommon).join(''), 2);
	return gamma * epsilon;
}

export function* part2(lines: string[]): Generator<
	{
		oxCandidates: string[];
		coCandidates: string[];
		mostCommon: Array<'1' | '0'>;
		leastCommon: Array<'1' | '0'>;
	},
	number,
	unknown
> {
	let oxCandidates = [...lines];
	let coCandidates = [...lines];
	const mostCommon: Array<'1' | '0'> = [];
	const leastCommon: Array<'1' | '0'> = [];

	for (let i = 0; oxCandidates.length > 1 || coCandidates.length > 1; i++) {
		if (oxCandidates.length > 1) {
			const oxBucket = oxCandidates.map((line) => line[i]);
			mostCommon.push(mostCommonBit(oxBucket));
			oxCandidates = oxCandidates.filter((line) => line[i] === mostCommon[i]);
		}
		if (coCandidates.length > 1) {
			const coBucket = coCandidates.map((line) => line[i]);
			leastCommon.push(leastCommonBit(coBucket));
			coCandidates = coCandidates.filter((line) => line[i] === leastCommon[i]);
		}
		yield { oxCandidates, mostCommon, leastCommon, coCandidates };
	}

	return parseInt(oxCandidates[0], 2) * parseInt(coCandidates[0], 2);
}
