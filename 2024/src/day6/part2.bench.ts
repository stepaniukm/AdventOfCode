import { join } from "@std/path";
import { part2 } from "./day6.ts";
import { getLines } from "#utils/input.ts";

Deno.bench("Day6 part2", async (b) => {
  // Setup
  const simplePath = join(import.meta.dirname!, "input.txt");
  const inputText = await Deno.readTextFile(simplePath);
  const lines = getLines(inputText);
  // Execute
  b.start();
  await part2(lines);
  b.end();
});
