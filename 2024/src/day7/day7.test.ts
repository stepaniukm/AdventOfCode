import { part1, part2 } from "./day7.ts";
import { assertEquals } from "@std/assert";
import { join } from "@std/path";
import { getLines } from "#utils/input.ts";
import { describe, it } from "@std/testing/bdd";

const onlySimple = (Deno.env.get("TEST_SIMPLE") ?? "0") === "1";

describe("Day7", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEquals(result, 3749);
  });

  it(
    { ignore: onlySimple, name: "should work for part 1 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part1(lines);

      assertEquals(result, 7885693428401);
    },
  );

  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEquals(result, 11387);
  });

  it(
    { ignore: onlySimple, name: "should work for part 2 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part2(lines);

      assertEquals(result, 348360680516005);
    },
  );
});
