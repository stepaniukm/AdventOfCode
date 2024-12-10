import { slidingWindows } from "@std/collections";
import { Position } from "#utils/misc.ts";

export const countOccurrences = <T extends PropertyKey>(
  array: Array<T>,
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
    const columnIndexes = currentRow.flatMap((char, columnIndex) => {
      if (char === startingChar) {
        return [columnIndex];
      } else {
        return [];
      }
    });

    return columnIndexes.map((columnIndex) => {
      return [rowIndex, columnIndex] as Position;
    });
  });

  if (!positions.length) {
    throw new Error("Start position not found");
  }

  return positions as [Position, ...Position[]];
};

export const getCharsPositionsGroupedByChar = (
  map: string[][],
  neutralChar: string,
) => {
  return map.reduce(
    (acc, currentRow, rowIndex) => {
      currentRow.forEach((char, charIndex) => {
        if (char === neutralChar) {
          return;
        }

        if (char in acc) {
          acc[char].push([rowIndex, charIndex]);
        } else {
          acc[char] = [[rowIndex, charIndex]];
        }
      });

      return acc;
    },
    {} as Record<string, Position[]>,
  );
};
