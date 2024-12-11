import { part1, part2 } from "./day11.ts";
import { assertEquals } from "@std/assert";
import { join } from "@std/path";
import { getLines } from "#utils/input.ts";
import { describe, it } from "@std/testing/bdd";

const onlySimple = (Deno.env.get("TEST_SIMPLE") ?? "0") === "1";

describe("Day11", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEquals(result, 55312);
  });

  it(
    { ignore: onlySimple, name: "should work for part 1 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part1(lines);

      assertEquals(result, 220722);
    },
  );

  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEquals(result, 65601038650482);
  });

  it(
    { ignore: onlySimple, name: "should work for part 2 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part2(lines);

      assertEquals(result, 261952051690787);
    },
  );
});
