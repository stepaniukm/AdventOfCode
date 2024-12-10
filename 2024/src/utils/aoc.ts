import { readLines } from "#utils/io.ts";
import { join } from "@std/path";

type AoC = {
  day: string;
  baseUrl: string;
  part1: (lines: string[]) => number | bigint | Promise<number | bigint>;
  part2: (lines: string[]) => number | bigint | Promise<number | bigint>;
  onlySimple?: boolean;
  onlyPart1?: boolean;
};
export const aoc = async ({
  day,
  baseUrl,
  part1,
  part2,
  onlySimple = false,
  onlyPart1 = false,
}: AoC) => {
  const simplePath = join(baseUrl, "src", `day${day}`, "simple-input.txt");
  const inputPath = join(baseUrl, "src", `day${day}`, "input.txt");

  const simpleLines = await readLines(simplePath);
  const inputLines = await readLines(inputPath);

  const part1ResultSimple = await part1(simpleLines);
  const part2ResultSimple = onlyPart1 ? "skipped" : await part2(simpleLines);
  const part1Result = onlySimple ? "skipped" : await part1(inputLines);
  const part2Result = onlySimple || onlyPart1
    ? "skipped"
    : await part2(inputLines);

  return {
    part1ResultSimple,
    part1Result,
    part2ResultSimple,
    part2Result,
  } as const;
};
