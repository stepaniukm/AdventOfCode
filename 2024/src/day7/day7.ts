export const part1 = (input: string[]): number | bigint => {
  const parsedInput = input.map((line) => {
    const [resultString, numbersString] = line.split(":");
    const result = Number(resultString);
    const numbers = numbersString.trim().split(" ").map(Number);

    return {
      result,
      numbers,
    } as const;
  });

  return parsedInput.reduce((acc, { result, numbers }) => {
    const holes = numbers.length - 1;
    const combinations = Array.from({ length: 2 ** holes }, (_, i) =>
      i.toString(2).padStart(holes, "0").split("").map(Number)
    );

    const correctCombinations = combinations.filter((combination) => {
      const combinationIterator = combination.values();
      const combinationResult = numbers.reduce((acc, number) => {
        const next = combinationIterator.next();
        const isAdding = next.value === 0;

        if (isAdding) {
          return acc + number;
        }
        return acc * number;
      });
      return combinationResult === result;
    }).length;

    if (correctCombinations > 0) {
      return acc + result;
    }

    return acc;
  }, 0);
};
export const part2 = (input: string[]): number | bigint => {
  const parsedInput = input.map((line) => {
    const [resultString, numbersString] = line.split(":");
    const result = BigInt(resultString);
    const numbers = numbersString.trim().split(" ").map(BigInt);

    return {
      result,
      numbers,
    } as const;
  });

  return parsedInput.reduce((acc, { result, numbers }) => {
    const holes = numbers.length - 1;
    const combinations = Array.from({ length: 3 ** holes }, (_, i) =>
      i.toString(3).padStart(holes, "0").split("").map(BigInt)
    );

    const correctCombinations = combinations.filter((combination) => {
      const combinationIterator = combination.values();
      const combinationResult = numbers.reduce((acc, number) => {
        const next = combinationIterator.next();
        const isAdding = next.value === 0n;
        const isMultiplying = next.value === 1n;

        if (isAdding) {
          return acc + number;
        }
        if (isMultiplying) {
          return acc * number;
        }
        return BigInt(acc.toString() + number.toString());
      });

      return combinationResult === result;
    }).length;

    if (correctCombinations > 0) {
      return acc + result;
    }

    return acc;
  }, 0n);
};
