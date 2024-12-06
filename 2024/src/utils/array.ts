import { slidingWindows } from "@std/collections";
import { Position } from "#utils/misc.ts";

export const countOccurrences = <T extends PropertyKey>(
  array: Array<T>
): Record<T, number> => {
  const occurrencesGroups = array.reduce((acc, curr) => {
    if (curr in acc) {
      acc[curr]++;
    } else {
      acc[curr] = 1;
    }
    return acc;
  }, {} as Record<T, number>);

  return occurrencesGroups;
};

export const getDifferences = (numbers: Array<number>) =>
  slidingWindows(numbers, 2).map(([a, b]) => {
    return b - a;
  });

export const getCharMap = (lines: string[]) => {
  return lines.map((line) => {
    return [...line].map((char) => {
      return char;
    });
  });
};

export const getStartingPositions = (map: string[][], startingChar: string) => {
  const positions = map.flatMap((currentRow, rowIndex) => {
    const charIndex = currentRow.indexOf(startingChar);

    if (charIndex !== -1) {
      return [[rowIndex, charIndex] as Position];
    }

    return [];
  });

  if (!positions.length) {
    throw new Error("Start position not found");
  }

  return positions as [Position, ...Position[]];
};
