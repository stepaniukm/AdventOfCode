import { part1, part2 } from "./src/day1/day1.ts";

const inputText = await Deno.readTextFile("./src/day1/input.txt");
const lines = inputText.split("\n");

const result1 = part1(lines);
const result2 = part2(lines);

console.log({ result1, result2 });
