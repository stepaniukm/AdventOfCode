import { part1, part2 } from "./src/day19/day19.ts";
import { aoc } from "#utils/aoc.ts";

const day = "19";
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
