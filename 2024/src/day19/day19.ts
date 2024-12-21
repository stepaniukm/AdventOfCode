import { SupportedPartResultTypes } from "#utils/aoc.ts";
import { parseTwoPartsSeparatedBySpace } from "#utils/input.ts";

const cache = new Map<string, number>();

export const part1 = (input: string[]): SupportedPartResultTypes => {
  cache.clear();
  const { partA: towelsNested, partB: patterns } =
    parseTwoPartsSeparatedBySpace({
      input,
      partAParser: (line) => line.split(", "),
      partBParser: (line) => line,
    });

  const towels = towelsNested[0];

  const possiblePatterns = patterns.filter((pattern) => {
    const result = howManyPatternsPossible(pattern, towels);
    return result > 0;
  });

  return possiblePatterns.length;
};

export const part2 = (input: string[]): SupportedPartResultTypes => {
  cache.clear();
  const { partA: towelsNested, partB: patterns } =
    parseTwoPartsSeparatedBySpace({
      input,
      partAParser: (line) => line.split(", "),
      partBParser: (line) => line,
    });

  const towels = towelsNested[0];

  const possiblePatterns = patterns.reduce((acc, pattern) => {
    const result = howManyPatternsPossible(pattern, towels);
    return acc + result;
  }, 0);

  return possiblePatterns;
};

const howManyPatternsPossible = (
  pattern: string,
  towels: string[],
): number => {
  if (cache.has(pattern)) {
    return cache.get(pattern)!;
  }

  if (pattern.length === 0) {
    return 1;
  }

  const correctOnes = towels.reduce((acc, towel) => {
    if (!pattern.endsWith(towel)) {
      return acc;
    }

    const newPattern = pattern.slice(0, pattern.length - towel.length);
    return acc + howManyPatternsPossible(newPattern, towels);
  }, 0);

  cache.set(pattern, correctOnes);

  return correctOnes;
};
