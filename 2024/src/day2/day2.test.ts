import { part1, part2 } from "./day2.ts";
import { assertEquals } from "@std/assert";
import { join } from "@std/path";
import { getLines } from "#utils/string.ts";

Deno.test("part1 - simple input", async () => {
  const simplePath = join(import.meta.dirname!, "simple-input.txt");
  const inputText = await Deno.readTextFile(simplePath);
  const lines = getLines(inputText);

  const result = part1(lines);

  assertEquals(result, 2);
});

Deno.test("part1 - input", async () => {
  const simplePath = join(import.meta.dirname!, "input.txt");
  const inputText = await Deno.readTextFile(simplePath);
  const lines = getLines(inputText);

  const result = part1(lines);

  assertEquals(result, 230);
});

Deno.test("part2 - simple input", async () => {
  const simplePath = join(import.meta.dirname!, "simple-input.txt");
  const inputText = await Deno.readTextFile(simplePath);
  const lines = getLines(inputText);

  const result = part2(lines);

  assertEquals(result, 4);
});

Deno.test("part2 - input", async () => {
  const simplePath = join(import.meta.dirname!, "input.txt");
  const inputText = await Deno.readTextFile(simplePath);
  const lines = getLines(inputText);

  const result = part2(lines);

  assertEquals(result, 301);
});
