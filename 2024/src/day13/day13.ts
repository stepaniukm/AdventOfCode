import * as math from "mathjs";

type Section = {
  A: [number, number];
  B: [number, number];
  R: [number, number];
};

type SectionBig = {
  A: [bigint, bigint];
  B: [bigint, bigint];
  R: [bigint, bigint];
};

const buttonLineRegex = /Button\s(?<name>\w):\sX\+(?<x>\d+),\sY\+(?<y>\d+)/gm;
const prizeLineRegex = /Prize: X=(?<x>\d+), Y=(?<y>\d+)/gm;

export const part1 = (input: string[]): number | bigint => {
  const parsedInput = input.reduce((acc, curr) => {
    if (curr === "") {
      acc.sections.push(acc.currentSection);
      acc.currentSection = {} as Section;
    } else {
      const buttonLineMatch = [...curr.trim().matchAll(buttonLineRegex)][0];

      if (buttonLineMatch) {
        const buttonLineMatchGroups = buttonLineMatch.groups!;
        acc.currentSection[buttonLineMatchGroups.name as keyof Section] = [
          parseInt(buttonLineMatchGroups.x),
          parseInt(buttonLineMatchGroups.y),
        ];
      } else {
        const prizeLineMatch = [...curr.trim().matchAll(prizeLineRegex)][0];
        if (prizeLineMatch) {
          const prizeLineMatchGroups = prizeLineMatch.groups!;
          acc.currentSection.R = [
            parseInt(prizeLineMatchGroups.x),
            parseInt(prizeLineMatchGroups.y),
          ];
        }
      }
    }

    return acc;
  }, {
    sections: [] as Section[],
    currentSection: {} as Section,
  });

  parsedInput.sections.push(parsedInput.currentSection);
  const { sections } = parsedInput;

  const result = sections.reduce((acc, section) => {
    const A = math.matrix([[section.A[0], section.B[0]], [
      section.A[1],
      section.B[1],
    ]]);
    const B = math.matrix([[section.R[0]], [section.R[1]]]);

    const A_inv = math.inv(A);
    const X = math.multiply(A_inv, B);

    const ATimes = Number(X.get([0, 0]).toPrecision(5));
    const BTimes = Number(X.get([1, 0]).toPrecision(5));

    if (
      ATimes >= 100 || BTimes >= 100 || !Number.isInteger(ATimes) ||
      !Number.isInteger(BTimes)
    ) return acc;

    const result = 3 * ATimes + BTimes;

    return acc + result;
  }, 0);

  return result;
};

export const part2 = (input: string[]): number | bigint => {
  const parsedInput = input.reduce((acc, curr) => {
    if (curr === "") {
      acc.sections.push(acc.currentSection);
      acc.currentSection = {} as SectionBig;
    } else {
      const buttonLineMatch = [...curr.trim().matchAll(buttonLineRegex)][0];

      if (buttonLineMatch) {
        const buttonLineMatchGroups = buttonLineMatch.groups!;
        acc.currentSection[buttonLineMatchGroups.name as keyof SectionBig] = [
          BigInt(buttonLineMatchGroups.x),
          BigInt(buttonLineMatchGroups.y),
        ];
      } else {
        const prizeLineMatch = [...curr.trim().matchAll(prizeLineRegex)][0];
        if (prizeLineMatch) {
          const prizeLineMatchGroups = prizeLineMatch.groups!;
          acc.currentSection.R = [
            BigInt(prizeLineMatchGroups.x) + 10_000_000_000_000n,
            BigInt(prizeLineMatchGroups.y) + 10_000_000_000_000n,
          ];
        }
      }
    }

    return acc;
  }, {
    sections: [] as SectionBig[],
    currentSection: {} as SectionBig,
  });

  parsedInput.sections.push(parsedInput.currentSection);
  const { sections } = parsedInput;

  const result = sections.reduce((acc, section) => {
    const detA = section.A[0] * section.B[1] - section.A[1] * section.B[0];
    if (detA === 0n) {
      return acc;
    }

    const detB1 = section.R[0] * section.B[1] - section.B[0] * section.R[1];
    const detB2 = section.A[0] * section.R[1] - section.A[1] * section.R[0];

    const ATimes = detB1 / detA;
    const ACheck = ATimes * detA;

    const BTimes = detB2 / detA;
    const BCheck = BTimes * detA;

    if (detB2 !== BCheck || ACheck !== detB1) {
      return acc;
    }

    const result = 3n * ATimes + BTimes;

    return acc + result;
  }, 0n);

  return result;
};
