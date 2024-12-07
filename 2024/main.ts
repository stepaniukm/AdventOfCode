import { part1, part2 } from "./src/day7/day7.ts";
import { aoc } from "#utils/aoc.ts";

const day = "7";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2, onlySimple: false });

console.log(result);
