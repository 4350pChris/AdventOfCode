import { openFile } from "../helper.mjs";

/**
 *
 *
 * @param {string[]} bucket
 */
function mostCommonBit(bucket) {
  const ones = bucket.filter((v) => v === "1");
  return ones.length >= bucket.length / 2 ? "1" : "0";
}

function leastCommonBit(bucket) {
  const zeroes = bucket.filter((v) => v === "0");
  return zeroes.length <= bucket.length / 2 ? "0" : "1";
}

/**
 *
 *
 * @param {string[]} n
 */
function invertBits(n) {
  return n.map((v) => (v === "1" ? "0" : "1"));
}

/**
 *
 *
 * @param {string[]} lines
 */
function part1(lines) {
  const buckets = Array(12)
    .fill()
    .map(() => []);
  lines.forEach((bit) => Array.from(bit).forEach((b, i) => buckets[i].push(b)));
  const mostCommon = buckets.map((b) => mostCommonBit(b));
  const gamma = parseInt(mostCommon.join(""), 2);
  const epsilon = parseInt(invertBits(mostCommon).join(""), 2);
  return gamma * epsilon;
}

function part2(lines) {
  let oxCandidates = [...lines];

  for (let i = 0; oxCandidates.length > 1; i++) {
    const bucket = oxCandidates.map((line) => line[i]);
    const mostCommon = mostCommonBit(bucket);
    oxCandidates = oxCandidates.filter((line) => line[i] === mostCommon);
  }

  let coCandidates = [...lines];

  for (let i = 0; coCandidates.length > 1; i++) {
    const bucket = coCandidates.map((line) => line[i]);
    const leastCommon = leastCommonBit(bucket);
    coCandidates = coCandidates.filter((line) => line[i] === leastCommon);
  }

  return parseInt(oxCandidates[0], 2) * parseInt(coCandidates[0], 2);
}

const lines = (await openFile("3")).split("\n");
const p1 = part2(lines);
console.log(p1);
