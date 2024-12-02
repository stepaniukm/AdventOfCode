import { getDifferences } from "#utils/array.ts";

export const part1 = (input: string[]): number => {
  const parsedInput = getParsedInput(input);

  const amountOfSafeReports = parsedInput.reduce((acc, report) => {
    const differences = getDifferences(report);

    const { allPositive, allBelowOrEqual3, allNegative } =
      getDifferencesStats(differences);

    if ((allPositive && allBelowOrEqual3) || (allNegative && allBelowOrEqual3))
      return acc + 1;

    return acc;
  }, 0);

  return amountOfSafeReports;
};
export const part2 = (input: string[]): number => {
  const parsedInput = getParsedInput(input);
  const amountOfSafeReports = parsedInput.reduce((acc, report) => {
    const differences = getDifferences(report);

    const { allPositive, allBelowOrEqual3, allNegative } =
      getDifferencesStats(differences);

    if ((allPositive && allBelowOrEqual3) || (allNegative && allBelowOrEqual3))
      return acc + 1;

    let index = 0;

    while (index < report.length) {
      const leftArray = report.slice(0, index);
      const rightArray = report.slice(index + 1);

      const combined = leftArray.concat(rightArray);

      const combinedDifferences = getDifferences(combined);

      const { allPositive, allNegative, allBelowOrEqual3 } =
        getDifferencesStats(combinedDifferences);

      if (
        (allPositive && allBelowOrEqual3) ||
        (allNegative && allBelowOrEqual3)
      )
        return acc + 1;

      index++;
    }

    return acc;
  }, 0);

  return amountOfSafeReports;
};

const getDifferencesStats = (differences: Array<number>) => {
  const allPositive = differences.every((diff) => diff > 0);
  const allNegative = differences.every((diff) => diff < 0);
  const allBelowOrEqual3 = differences.every((diff) => Math.abs(diff) <= 3);

  return { allPositive, allNegative, allBelowOrEqual3 } as const;
};

const getParsedInput = (input: Array<string>) => {
  const parsedInput = input.map((line) =>
    line.split(" ").map((num) => Number(num))
  );

  return parsedInput;
};
