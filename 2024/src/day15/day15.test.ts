import { part1, part2 } from "./day15.ts";
import { join } from "@std/path";
import { getLines } from "#utils/input.ts";
import { describe, it } from "@std/testing/bdd";
import { assertEqualToBigIntOrNumber } from "#utils/test.ts";

const onlySimple = (Deno.env.get("TEST_SIMPLE") ?? "0") === "1";

describe("Day15", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part1(lines);

    assertEqualToBigIntOrNumber(result, "10092");
  });

  it(
    { ignore: onlySimple, name: "should work for part 1 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part1(lines);

      assertEqualToBigIntOrNumber(result, "1294459");
    },
  );

  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    const result = part2(lines);

    assertEqualToBigIntOrNumber(result, "9021");
  });

  it(
    { ignore: onlySimple, name: "should work for part 2 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      const result = part2(lines);

      assertEqualToBigIntOrNumber(result, "1319212");
    },
  );
});
