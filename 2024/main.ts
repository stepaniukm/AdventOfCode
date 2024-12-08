import { part1, part2 } from "./src/day8/day8.ts";
import { aoc } from "#utils/aoc.ts";

const day = "8";
const baseUrl = import.meta.dirname!;

const result = await aoc({ day, baseUrl, part1, part2, onlySimple: false });

console.log(result);
