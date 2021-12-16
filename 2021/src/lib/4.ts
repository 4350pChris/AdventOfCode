type Data = [n: number, set: boolean][];
export type Field = Data[];

function transposeField(field: Field) {
	const columns: Field = [];
	field.forEach((row) => {
		row.forEach(([n, isSet], i) => {
			if (!columns[i]) {
				columns.push([]);
			}
			columns[i].push([n, isSet]);
		});
	});
	return columns;
}

function setMaxCount(field: Field) {
	const trueCount = (row: Data) => row.filter((v) => v[1]).length;

	const columns = transposeField(field);
	const counts = [...field.map((row) => trueCount(row)), ...columns.map((col) => trueCount(col))];
	return Math.max(...counts);
}

function setNumber(field: Field, n: number): Field {
	return field.map((row) => row.map(([num, isSet]) => [num, isSet || num === n]));
}

export function makeFields(input: string[]): Field[] {
	return input
		.filter((v) => v !== '')
		.reduce((acc, v, i) => {
			const bucket = Math.floor(i / 5);
			if (!acc[bucket]) {
				acc.push([]);
			}
			const row: Data = v
				.split(' ')
				.filter((v) => v !== '')
				.reduce((row, v) => [...row, [Number(v), false]], [] as Data);

			acc[bucket].push(row);
			return acc;
		}, [] as Field[]);
}

export function makeNumbers(line: string): number[] {
	return line.split(',').map((v) => Number(v));
}

export function* part1(
	numbers: number[],
	fields: Field[]
): Generator<
	[drawn: number, fields: { field: Field; count: number }[]],
	[drawn: number, fields: { field: Field; count: number }[], result: number],
	unknown
> {
	const localFields = fields.map((field) => ({ field, count: 0 }));
	for (const number of numbers) {
		for (const i in localFields) {
			localFields[i].field = setNumber(localFields[i].field, number);
			localFields[i].count = setMaxCount(localFields[i].field);
		}
		const winner = localFields.find(({ count }) => count === 5);
		if (winner) {
			const sum = winner.field.reduce(
				(acc, row) => acc + row.reduce((rowSum, [n, set]) => (set ? rowSum : rowSum + n), 0),
				0
			);
			return [number, localFields, sum * number];
		}
		yield [number, localFields];
	}
}
