import { part1, part2 } from "./day18.ts";
import { join } from "@std/path";
import { getLines } from "#utils/input.ts";
import { describe, it } from "@std/testing/bdd";
import { assertEqualToBigIntOrNumber } from "#utils/test.ts";

const onlySimple = (Deno.env.get("TEST_SIMPLE") ?? "0") === "1";

describe("Day18", () => {
  it("should work for part 1 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    Deno.env.set("IS_SIMPLE", "1");
    const result = part1(lines);

    assertEqualToBigIntOrNumber(result, "22");
  });

  it(
    { ignore: onlySimple, name: "should work for part 1 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      Deno.env.set("IS_SIMPLE", "0");
      const result = part1(lines);

      assertEqualToBigIntOrNumber(result, "374");
    },
  );

  it("should work for part 2 with simple input", async () => {
    const simplePath = join(import.meta.dirname!, "simple-input.txt");
    const inputText = await Deno.readTextFile(simplePath);
    const lines = getLines(inputText);

    Deno.env.set("IS_SIMPLE", "1");
    const result = part2(lines);

    assertEqualToBigIntOrNumber(result, "6,1");
  });

  it(
    { ignore: onlySimple, name: "should work for part 2 with real input" },
    async () => {
      const simplePath = join(import.meta.dirname!, "input.txt");
      const inputText = await Deno.readTextFile(simplePath);
      const lines = getLines(inputText);

      Deno.env.set("IS_SIMPLE", "10");
      const result = part2(lines);

      assertEqualToBigIntOrNumber(result, "30,12");
    },
  );
});
