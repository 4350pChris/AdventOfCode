import { openFile } from '../helper.mjs';

const data = await openFile('4');

const [numbersLine, ...rest] = data.split('\n');

const numbers = numbersLine.split(',').map((v) => Number(v));

type Data = { [n: number]: boolean };
type Field = Data[];

const fields = rest
	.filter((v) => v !== '')
	.reduce((acc, v, i) => {
		const bucket = Math.floor(i / 5);
		if (!acc[bucket]) {
			acc.push([]);
		}
		const row: Data = v
			.split(' ')
			.filter((v) => v !== '')
			.reduce((row, v) => {
				row[Number(v)] = false;
				return row;
			}, {} as Data);

		acc[bucket].push(row);
		return acc;
	}, [] as Field[]);

function checkWin(field: Field) {
	const allTrue = (row: Data) => !Object.values(row).some((v) => !v);

	for (let row of field) {
		if (allTrue(row)) {
			return true;
		}
	}
	let columns: Data[] = [];
	field.forEach((v, i) => {
		if (!columns[i]) {
			columns.push({});
		}
		const [n, isSet] = Object.entries(v)[i];
		columns[i][n] = isSet;
	});
	for (let col of columns) {
		if (allTrue(col)) {
			return true;
		}
	}

	return false;
}

function setNumber(field: Field, n: number) {
	for (let row in field) {
		for (let col in row) {
			if (col === n) {
				field[row][col] = true;
			}
		}
	}
}

for (let number of numbers) {
	setNumber(number);
	for (let field of fields) {
		if (checkWin(field)) {
			console.log('Winning');
			let sum = 0;
			for (let row of field) {
				for (let col of row) {
					if (col) {
						sum += 1;
					}
				}
			}
		}
	}
}

console.log(sum);
