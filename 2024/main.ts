import { part1, part2 } from "./src/day1/day1.ts";
import { aoc } from "#utils/aoc.ts";

const day = "1";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2 });

console.log(result);
