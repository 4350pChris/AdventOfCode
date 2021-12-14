const fs = require("fs");

function part1(lines) {
  return lines.reduce(
    ([count, prev], current) => [current > prev ? count + 1 : count, current],
    [0, Infinity]
  );
}

function part2(lines) {
  const windows = [];
  for (let line of lines) {
    windows.push([line]);
    const wLen = windows.length - 1;
    if (windows[wLen - 1]) {
      windows[wLen - 1].push(line);
    }
    if (windows[wLen - 2]) {
      windows[wLen - 2].push(line);
    }
  }
  // remove last 2 entries since they contain only 2 and 1 item respectively
  windows.splice(-2, 2);
  const sums = windows.flatMap((w) => w.reduce((acc, val) => acc + val, 0));
  const res = part1(sums);
  return res;
}

fs.readFile("input.txt", "utf-8", (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const lines = data.split("\n").map((line) => Number(line));
  const higherTimes = part1(lines);
  console.log(higherTimes);
  const p2 = part2(lines);
  console.log(p2);
});
