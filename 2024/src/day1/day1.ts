import { zip } from "@std/collections";

const common = (input: string[]) => {
  const parsedInput = input
    .map((line) => {
      const [num1, num2] = line.split("   ");
      return [Number(num1), Number(num2)] as const;
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
  const zipped = zip(sortedA, sortedB);
  const mapped = zipped.map(([a, b]) => Math.abs(a - b));
  const sum = mapped.reduce((acc, b) => acc + b, 0);

  return sum;
};
export const part2 = (input: string[]): number => {
  const parsedInput = common(input);

  const occurancesByNumber = parsedInput.b.reduce((acc, curr) => {
    if (curr in acc) {
      acc[curr]++;
    } else {
      acc[curr] = 1;
    }
    return acc;
  }, {} as Record<number, number>);

  const result = parsedInput.a
    .map((num) => {
      const occurancesOfNum = occurancesByNumber[num];
      console.log({ occurancesOfNum });
      if (!occurancesOfNum) {
        return 0;
      }

      return occurancesOfNum * num;
    })
    .reduce((acc, curr) => acc + curr, 0);

  return result;
};
