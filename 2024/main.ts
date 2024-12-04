import { part1, part2 } from "./src/day4/day4.ts";
import { aoc } from "#utils/aoc.ts";

const day = "4";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2, onlySimple: false });

console.log(result);
