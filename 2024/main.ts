import { part1, part2 } from "./src/day6/day6.ts";
import { aoc } from "#utils/aoc.ts";

const day = "6";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2, onlySimple: false });

console.log(result);
