import { Range, Map, set } from "immutable";
import { zip } from "@std/collections";
import { RedBlackTree } from "@std/data-structures";

const map = Map({ a: 1, b: 2, c: 3 });

export const part1 = (_input: string[]): number => {
  return Range(0, 10)
    .toSeq()
    .reduce((a, b) => a + b);
};
export const part2 = (_input: string[]): number => {
  const jsMap = set(map, "d", 10).toJS();
  const transformed = zip(Object.values(jsMap), ["jeden", "dwa", "trzy"]);
  const newMap = Map(transformed).toSeq().toArray();
  const tree = new RedBlackTree<[number, string]>();

  newMap.forEach((v) => tree.insert(v));

  return [...tree].reduce((a, b) => a + b[1].length, 0);
};
