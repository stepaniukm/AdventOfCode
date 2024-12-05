type Rule = {
  before: number;
  after: number;
};

export const part1 = (input: string[]): number => {
  const { rules, rows } = parseInput(input);

  const correctRows = rows.filter((row) => isRowCorrect(row, rules));

  return correctRows.reduce((acc, row) => {
    return acc + row[Math.floor(row.length / 2)];
  }, 0);
};
export const part2 = (input: string[]): number => {
  const { rows, rules } = parseInput(input);

  const correctRows = rows.filter((row) => !isRowCorrect(row, rules));

  return correctRows.reduce((acc, row) => {
    const rowSorted = row.toSorted((a, b) => {
      const rule = rules.find((rule) => rule.before === a && rule.after === b);
      return rule ? -1 : 1;
    });

    return acc + rowSorted[Math.floor(row.length / 2)];
  }, 0);
};

const parseInput = (input: string[]) => {
  const { rules, rows } = input.reduce(
    (acc, curr) => {
      if (curr.length === 0) {
        acc.pauseEncountered = true;
        return acc;
      }
      if (acc.pauseEncountered) {
        const line = curr.split(",").map((char) => Number(char));
        acc.rows.push(line);
      }
      if (!acc.pauseEncountered) {
        const [before, after] = curr.split("|");
        acc.rules.push({
          before: Number(before),
          after: Number(after),
        });
      }
      return acc;
    },
    {
      rules: [] as Rule[],
      rows: [] as Array<Array<number>>,
      pauseEncountered: false,
    }
  );

  return { rules, rows } as const;
};

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
