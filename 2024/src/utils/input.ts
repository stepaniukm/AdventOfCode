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
    },
  );

  return { partA, partB } as const;
};

export const getLines = (str: string) => str.split("\n");

export const parseLinesIntoTwoParts = <PA, PB>({
  input,
  separateString,
  partAParser,
  partBParser,
}: {
  input: string[];
  separateString: string;
  partAParser: (partAString: string) => PA;
  partBParser: (partAString: string) => PB;
}) => {
  return input.map((line) => {
    const [partAString, partBString] = line.split(separateString);
    const partA = partAParser(partAString);
    const partB = partBParser(partBString);

    return {
      partA,
      partB,
    } as const;
  });
};
