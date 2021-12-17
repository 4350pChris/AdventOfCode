export type Result = Generator<number, void, unknown>;

const memoize = (fn) => {
	const cache = {};
	return (...args: any[]) => {
		const key = args.join('_');
		if (key in cache) {
			return cache[key];
		} else {
			const result = fn(...args);
			cache[key] = result;
			return result;
		}
	};
};

const populationAfterNDays = memoize((countdown: number, days: number) => {
	if (days === 0) {
		return 1;
	}
	if (countdown === 0) {
		return populationAfterNDays(6, days - 1) + populationAfterNDays(8, days - 1);
	}
	return populationAfterNDays(countdown - 1, days - 1);
});

export function* main(squids: number[], days: number): Result {
	for (let day = 0; day <= days; day++) {
		yield squids.reduce((sum, squid) => sum + populationAfterNDays(squid, day), 0);
	}
}
