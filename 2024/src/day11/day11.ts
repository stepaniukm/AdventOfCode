export const part1 = (input: string[]): number | bigint => {
  const numbers = input[0].split(" ").map(Number);
  return numbers.reduce((acc, curr) => {
    return acc + blink(curr, 25);
  }, 0);
};

export const part2 = (input: string[]): number | bigint => {
  const numbers = input[0].split(" ").map(Number);

  return numbers.reduce((acc, curr) => {
    return acc + blink(curr, 75);
  }, 0);
};

const cache: Map<string, number> = new Map();

const blink = (
  number: number,
  times: number,
): number => {
  if (times === 0) {
    return 1;
  }

  const key = cacheKey(number.toString(), times);
  const cacheResult = cache.get(key);

  if (cacheResult) {
    return cacheResult;
  }

  if (number === 0) {
    const key = cacheKey("1", times - 1);
    const result = blink(1, times - 1);
    cache.set(key, result);
    return result;
  }

  const numberString = number.toString();

  if (numberString.length % 2 === 0) {
    const halfIndex = Math.floor(numberString.length / 2);
    const firstHalf = numberString.slice(0, halfIndex);
    const secondHalf = numberString.slice(halfIndex);
    const leftCacheKey = cacheKey(firstHalf, times - 1);
    const rightCacheKey = cacheKey(secondHalf, times - 1);

    const resultLeft = blink(Number(firstHalf), times - 1);
    cache.set(leftCacheKey, resultLeft);

    const resultRight = blink(Number(secondHalf), times - 1);
    cache.set(rightCacheKey, resultRight);

    return resultLeft + resultRight;
  } else {
    const newNumber = number * 2024;
    const newNumberCacheKey = cacheKey(newNumber.toString(), times - 1);
    const result = blink(newNumber, times - 1);
    cache.set(newNumberCacheKey, result);
    return result;
  }
};

const cacheKey = (number: string, times: number) => {
  return number + ":" + times.toString();
};
