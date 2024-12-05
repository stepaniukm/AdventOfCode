type ParseArgs<PA, PB> = {
  input: string[];
  partAParser: (line: string) => PA;
  partBParser: (line: string) => PB;
};

export const parseTwoPartsSeparatedBySpace = <PA, PB>({
  input,
  partAParser,
  partBParser,
}: ParseArgs<PA, PB>) => {
  const { partA, partB } = input.reduce(
    (acc, curr) => {
      if (curr.length === 0) {
        acc.pauseEncountered = true;
        return acc;
      }
      if (acc.pauseEncountered) {
        const parsedPartB = partBParser(curr);
        acc.partB.push(parsedPartB);
      }
      if (!acc.pauseEncountered) {
        const parsedPartA = partAParser(curr);
        acc.partA.push(parsedPartA);
      }
      return acc;
    },
    {
      partA: [] as PA[],
      partB: [] as PB[],
      pauseEncountered: false,
    }
  );

  return { partA, partB } as const;
};
