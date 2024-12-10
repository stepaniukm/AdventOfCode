const NEUTRAL_CHAR = ".";

export const part1 = (input: string[]): number | bigint => {
  const line = input[0];

  let nextId = -1;

  const string = line.split("").reduce((acc, curr, idx) => {
    const currNumber = Number(curr);
    if (idx % 2 === 0) {
      nextId++;
      return acc.concat(
        Array.from({ length: currNumber }, () => nextId.toString()),
      );
    } else {
      return acc.concat(NEUTRAL_CHAR.repeat(currNumber).split(""));
    }
  }, [] as string[]);

  const mappedString: string[] = [];
  let startIndex = 0;
  let endIndex = string.length;

  while (startIndex < endIndex) {
    const currentChar = string[startIndex];

    if (currentChar !== NEUTRAL_CHAR) {
      mappedString.push(currentChar);
      startIndex++;
      continue;
    }

    let nextChars = string[--endIndex];
    while (nextChars === NEUTRAL_CHAR) {
      nextChars = string[--endIndex];
    }

    if (startIndex >= endIndex) {
      break;
    }

    mappedString.push(nextChars);
    startIndex++;
  }

  return mappedString.reduce(
    (acc, curr, idx) => {
      return acc + Number(curr) * idx;
    },
    0,
  );
};

export const part2 = (input: string[]): number | bigint => {
  const line = input[0];

  let nextId = -1;

  const string = line.split("").reduce((acc, curr, idx) => {
    const currNumber = Number(curr);
    if (idx % 2 === 0) {
      nextId++;
      return acc.concat(
        Array.from({ length: currNumber }, () => nextId.toString()),
      );
    } else {
      return acc.concat(NEUTRAL_CHAR.repeat(currNumber).split(""));
    }
  }, [] as string[]);

  let compressedString = string;
  let matchIndex: number = -1;

  do {
    const nextIdString = nextId.toString();
    const nextIdLength = nextIdString.length;

    const currentGroupIndex = compressedString.indexOf(nextIdString);
    const currentGroupIndexEnd = compressedString.lastIndexOf(
      nextIdString,
    );
    const currentGroup = compressedString.slice(
      currentGroupIndex,
      currentGroupIndex +
        (currentGroupIndexEnd - currentGroupIndex + 1) * nextIdLength,
    );
    const currentGroupLength = currentGroup.length;

    ////////////////////////// Above is 100% correct //////////////////////////

    matchIndex = 0;
    let found = false;

    while (matchIndex < currentGroupIndex - 3) {
      const currentChar = compressedString[matchIndex];

      if (currentChar !== NEUTRAL_CHAR) {
        matchIndex++;
        continue;
      } else {
        const items = compressedString.slice(
          matchIndex,
          matchIndex + currentGroupLength,
        );

        if (
          items.every((item) => item === NEUTRAL_CHAR)
        ) {
          found = true;
          break;
        } else {
          matchIndex++;
        }
      }
    }

    // console.log({
    //   matchIndex,
    //   currentGroup,
    //   compressedString: compressedString.join(""),
    // });

    if (
      matchIndex !== -1 && matchIndex < currentGroupIndex && nextId > 0 && found
    ) {
      const newCompressedString = compressedString.slice(0, matchIndex)
        .concat(currentGroup)
        .concat(compressedString.slice(
          matchIndex + currentGroupLength,
          currentGroupIndex,
        ))
        .concat(...NEUTRAL_CHAR.repeat(currentGroupLength))
        .concat(compressedString.slice(currentGroupIndexEnd + 1));

      compressedString = newCompressedString;
    }
    nextId--;
  } while ((matchIndex !== -1 && nextId > 0) || nextId > 0);

  return compressedString.reduce(
    (acc, curr, idx) => {
      return acc + Number(curr === NEUTRAL_CHAR ? 0 : curr) * idx;
    },
    0,
  );
};
