import { part1, part2 } from "./src/day2/day2.ts";
import { aoc } from "#utils/aoc.ts";

const day = "2";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2 });

console.log(result);
