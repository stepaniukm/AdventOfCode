import { parseTwoPartsSeparatedBySpace } from "#utils/input.ts";

type Rule = {
  before: number;
  after: number;
};

export const part1 = (input: string[]): number => {
  const { partA: rules, partB: rows } = parseTwoPartsSeparatedBySpace({
    input,
    partAParser,
    partBParser,
  });

  const correctRows = rows.filter((row) => isRowCorrect(row, rules));

  return correctRows.reduce((acc, row) => {
    return acc + row[Math.floor(row.length / 2)];
  }, 0);
};
export const part2 = (input: string[]): number => {
  const { partA: rules, partB: rows } = parseTwoPartsSeparatedBySpace({
    input,
    partAParser,
    partBParser,
  });

  const correctRows = rows.filter((row) => !isRowCorrect(row, rules));

  return correctRows.reduce((acc, row) => {
    const rowSorted = row.toSorted((a, b) => {
      const rule = rules.find((rule) => rule.before === a && rule.after === b);
      return rule ? -1 : 1;
    });

    return acc + rowSorted[Math.floor(row.length / 2)];
  }, 0);
};

const partAParser = (line: string) => {
  const [before, after] = line.split("|");

  return {
    before: Number(before),
    after: Number(after),
  };
};
const partBParser = (line: string) =>
  line.split(",").map((char) => Number(char));

const isRowCorrect = (row: number[], rules: Rule[]) => {
  return rules.every((rule) => {
    const { before, after } = rule;
    const beforeIndex = row.indexOf(before);
    const afterIndex = row.indexOf(after);
    if (beforeIndex === -1 || afterIndex === -1) {
      return true;
    }
    return beforeIndex < afterIndex;
  });
};
