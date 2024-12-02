import { zip, sumOf, slidingWindows } from "@std/collections";
import {} from "#utils/array.ts";
import { identity } from "#utils/misc.ts";

export const part1 = (input: string[]): number => {
  const parsedInput = input.map((line) =>
    line.split(" ").map((num) => Number(num))
  );

  const amountOfSafeReports = parsedInput.reduce((acc, report) => {
    const differences = slidingWindows(report, 2).map(([a, b]) => {
      return b - a;
    });

    const allPositive = differences.every((diff) => diff > 0);
    const allNegative = differences.every((diff) => diff < 0);
    const allBelowOrEqual3 = differences.every((diff) => Math.abs(diff) <= 3);

    if ((allPositive && allBelowOrEqual3) || (allNegative && allBelowOrEqual3))
      return acc + 1;

    return acc;
  }, 0);

  return amountOfSafeReports;
};
export const part2 = (input: string[]): number => {
  const parsedInput = input.map((line) =>
    line.split(" ").map((num) => Number(num))
  );

  const amountOfSafeReports = parsedInput.reduce((acc, report) => {
    const differences = slidingWindows(report, 2).map(([a, b]) => {
      return b - a;
    });

    const allPositive = differences.every((diff) => diff > 0);
    const allNegative = differences.every((diff) => diff < 0);
    const allBelowOrEqual3 = differences.every((diff) => Math.abs(diff) <= 3);

    if ((allPositive && allBelowOrEqual3) || (allNegative && allBelowOrEqual3))
      return acc + 1;

    let index = 0;

    while (index < report.length) {
      const leftArray = report.slice(0, index);
      const rightArray = report.slice(index + 1);

      const combined = leftArray.concat(rightArray);

      const combinedDifferences = slidingWindows(combined, 2).map(([a, b]) => {
        return b - a;
      });

      const allCombinedPositive = combinedDifferences.every((diff) => diff > 0);
      const allCombinedNegative = combinedDifferences.every((diff) => diff < 0);
      const allCombinedBelowOrEqual3 = combinedDifferences.every(
        (diff) => Math.abs(diff) <= 3
      );

      if (
        (allCombinedPositive && allCombinedBelowOrEqual3) ||
        (allCombinedNegative && allCombinedBelowOrEqual3)
      )
        return acc + 1;

      index++;
    }

    return acc;
  }, 0);

  return amountOfSafeReports;
};
