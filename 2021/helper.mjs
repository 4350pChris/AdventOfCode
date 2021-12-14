import { readFile } from "fs";
import { join } from "path";

/**
 *
 *
 * @export
 * @param {string} dir
 * @returns {Promise<string>}
 */
export function openFile(dir) {
  return new Promise((resolve, reject) => {
    readFile(join(process.cwd(), dir, "input.txt"), "utf-8", (err, data) => {
      if (err) {
        console.error(err);
        reject(err);
      }
      resolve(data);
    });
  });
}
