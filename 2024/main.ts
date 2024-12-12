import { part1, part2 } from "./src/day12/day12.ts";
import { aoc } from "#utils/aoc.ts";

const day = "12";
const baseUrl = import.meta.dirname!;

const result = await aoc({
  day,
  baseUrl,
  part1,
  part2,
  onlySimple: false,
  onlyPart1: false,
});

console.log(result);
