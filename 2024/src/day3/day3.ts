const mulRegex = /(?<mul>mul\((?<first>\d+),(?<second>\d+)\))/g;
const doRegex = /(?<do>do\(\))/g;
const dontRegex = /(?<do>don't\(\))/g;
export const part1 = (input: string[]): number => {
  return input.reduce((acc, line) => {
    const matches = [...line.matchAll(mulRegex)];
    return (
      acc +
      matches.reduce((acc, match) => {
        return acc + Number(match.groups!.first) * Number(match.groups!.second);
      }, 0)
    );
  }, 0);
};
export const part2 = (input: string[]): number => {
  const oneBigString = input.reduce((acc, curr) => acc + curr, "");
  const muls = [...oneBigString.matchAll(mulRegex)];
  const dos = [...oneBigString.matchAll(doRegex)].map((group) => group.index);
  const donts = [...oneBigString.matchAll(dontRegex)].map(
    (group) => group.index
  );

  const ranges = donts.flatMap((dontIndex) => {
    const firstBiggerDo = dos.find((doIndex) => doIndex > dontIndex);
    if (!firstBiggerDo) {
      const lastMulIndex = muls.at(-1)!.index!;
      if (lastMulIndex > dontIndex) {
        return [[dontIndex, oneBigString.length] as const];
      }
      return [];
    }

    return [[dontIndex, firstBiggerDo] as const];
  });

  return muls.reduce((acc, mulMatch) => {
    const range = ranges.find((range) => {
      return range[0] < mulMatch.index && mulMatch.index < range[1];
    });
    if (range) return acc;

    return (
      acc + Number(mulMatch.groups!.first) * Number(mulMatch.groups!.second)
    );
  }, 0);
};
