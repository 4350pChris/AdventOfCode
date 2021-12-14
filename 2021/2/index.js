const fs = require("fs");

const TYPE = {
  DEPTH: 1,
  POSITION: 2,
};

function getFnFromLine(line) {
  const [start, n] = line.split(" ");
  switch (start) {
    case "forward":
      return [
        TYPE.POSITION,
        (pos, aim) => ({
          position: pos + Number(n),
          depth: Number(n) * aim,
        }),
      ];
    case "down":
      return [TYPE.DEPTH, (val) => val + Number(n)];
    case "up":
      return [TYPE.DEPTH, (val) => val - Number(n)];
  }
}

/**
 *
 *
 * @param {string[]} lines
 */
function part1(lines) {
  let horizontal = 0;
  let aim = 0;
  let depth = 0;
  for (let line of lines) {
    console.log(horizontal, aim, depth);
    const [t, fn] = getFnFromLine(line);
    if (t === TYPE.POSITION) {
      const { position, depth: d } = fn(horizontal, aim);
      horizontal = position;
      depth += d;
    } else {
      aim = fn(aim);
    }
  }
  console.log(depth, horizontal);
  return depth * horizontal;
}

fs.readFile("./input.txt", "utf-8", (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const lines = data.split("\n");
  const p1 = part1(lines);
  console.log(p1);
});
