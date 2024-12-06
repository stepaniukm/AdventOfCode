import { part1, part2 } from "./day6.ts";
import { assertEquals } from "@std/assert";
import { join } from "@std/path";
import { getLines } from "#utils/string.ts";
import { describe, it } from "@std/testing/bdd";

describe("Day6", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEquals(result, 41);
  });

  it("should work for part 1 with real input", async () => {
    const simplePath = join(import.meta.dirname!, "input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEquals(result, 5269);
  });
  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEquals(result, 6);
  });
  it("should work for part 2 with real input", async () => {
    const simplePath = join(import.meta.dirname!, "input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEquals(result, 1957);
  });
});
