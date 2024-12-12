import { part1, part2 } from "./day12.ts";
import { assertEquals } from "@std/assert";
import { join } from "@std/path";
import { getLines } from "#utils/input.ts";
import { describe, it } from "@std/testing/bdd";

const onlySimple = (Deno.env.get("TEST_SIMPLE") ?? "0") === "1";

describe("Day12", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEquals(result, 1930);
  });

  it(
    { ignore: onlySimple, name: "should work for part 1 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part1(lines);

      assertEquals(result, 1461806);
    },
  );

  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEquals(result, 1206);
  });

  it(
    { ignore: onlySimple, name: "should work for part 2 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part2(lines);

      assertEquals(result, 887932);
    },
  );
});
