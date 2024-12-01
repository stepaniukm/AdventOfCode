import { zip, sumOf } from "@std/collections";
import { countOccurrences } from "#utils/array.ts";
import { identity } from "#utils/misc.ts";

const common = (input: string[]) => {
  const parsedInput = input
    .map((line) => {
      const [num1, num2] = line.split("   ").map((num) => Number(num));
      return [num1, num2] as const;
    })
    .reduce(
      (acc, b) => {
        acc.a.push(b[0]);
        acc.b.push(b[1]);
        return acc;
      },
      { a: [] as number[], b: [] as number[] }
    );

  return parsedInput;
};

export const part1 = (input: string[]): number => {
  const parsedInput = common(input);
  const sortedA = parsedInput.a.toSorted();
  const sortedB = parsedInput.b.toSorted();
  const differences = zip(sortedA, sortedB).map(([a, b]) => Math.abs(a - b));
  const sumOfDifferences = sumOf(differences, identity);

  return sumOfDifferences;
};
export const part2 = (input: string[]): number => {
  const parsedInput = common(input);

  const occurrencesByNumber = countOccurrences(parsedInput.b);

  const productsOfOccurrencesAndNumbers = parsedInput.a.flatMap((num) => {
    const occurrencesOfNum = occurrencesByNumber[num];
    if (!occurrencesOfNum) {
      return [];
    }

    return [occurrencesOfNum * num];
  });

  const sum = sumOf(productsOfOccurrencesAndNumbers, identity);

  return sum;
};
