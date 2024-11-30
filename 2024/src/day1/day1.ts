import { Range, Record } from "immutable";

export const part1 = (_input: string[]): number => {
  return Range(0, 10)
    .toSeq()
    .reduce((a, b) => a + b);
};
export const part2 = (_input: string[]): number => {
  return Record({ a: 1, b: 2, c: 3 })()
    .toSeq()
    .reduce((a, b) => a + b);
};
