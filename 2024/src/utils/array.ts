import { slidingWindows } from "@std/collections";

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
