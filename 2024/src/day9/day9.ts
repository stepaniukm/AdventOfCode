import { slidingWindows } from "@std/collections";

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

type Entry = { type: "file"; id: string; sizeInBlocks: number } | {
  type: "space";
  sizeInBlocks: number;
};

export const part2 = (input: string[]): number | bigint => {
  const line = input[0];

  let nextId = -1;

  const groups = line.split("").reduce((acc, size, idx) => {
    const sizeNumber = Number(size);
    if (idx % 2 === 0) {
      nextId++;
      return acc.concat(
        { type: "file", id: nextId.toString(), sizeInBlocks: sizeNumber },
      );
    } else {
      return acc.concat({
        type: "space",
        sizeInBlocks: sizeNumber,
      });
    }
  }, [] as Entry[]);

  let compressedGroups = groups;
  let fileToMoveIndex: number = -1;
  let spaceToMoveIndex: number = -1;

  do {
    fileToMoveIndex = compressedGroups.findLastIndex((group) =>
      group.type === "file" && Number(group.id) <= nextId
    );

    if (fileToMoveIndex === -1) {
      nextId--;
      continue;
    }
    const fileToMove = compressedGroups[fileToMoveIndex];

    spaceToMoveIndex = compressedGroups.findIndex((group) => {
      return group.type === "space" &&
        group.sizeInBlocks >= fileToMove.sizeInBlocks;
    });

    if (spaceToMoveIndex === -1) {
      nextId--;
      continue;
    }

    const spaceToMove = compressedGroups[spaceToMoveIndex];

    const spaceLeft = spaceToMove.sizeInBlocks -
      fileToMove.sizeInBlocks;

    const leftSpaceEntry = spaceLeft > 0
      ? {
        type: "space",
        sizeInBlocks: spaceLeft,
      } as Entry
      : null;

    if (spaceToMoveIndex < fileToMoveIndex) {
      const newCompressedGroupsWithFile = compressedGroups.slice(
        0,
        spaceToMoveIndex,
      )
        .concat(fileToMove);
      const newCompressedGroupsWithNewLeftSpace = leftSpaceEntry
        ? newCompressedGroupsWithFile.concat(
          leftSpaceEntry,
        )
        : newCompressedGroupsWithFile;

      const newCompressedGroupsToNormalize = newCompressedGroupsWithNewLeftSpace
        .concat(
          compressedGroups.slice(
            spaceToMoveIndex + 1,
            fileToMoveIndex,
          ),
        )
        .concat({ type: "space", sizeInBlocks: fileToMove.sizeInBlocks })
        .concat(compressedGroups.slice(fileToMoveIndex + 1));

      const normalizedCompressedGroups = normalizeGroups(
        newCompressedGroupsToNormalize,
      );

      compressedGroups = normalizedCompressedGroups;
    }
    nextId--;
  } while (
    (spaceToMoveIndex !== -1 && fileToMoveIndex !== -1 && nextId > 0) ||
    nextId > 0
  );

  return compressedGroups.reduce(
    (acc, curr) => {
      if (curr.type === "space") {
        acc.globalIndex += curr.sizeInBlocks;
        return acc;
      }

      const indexes = Array.from(
        { length: curr.sizeInBlocks },
        (_, i) => i + acc.globalIndex,
      );

      const sum = indexes.reduce((a, b) => a + b * Number(curr.id), 0);

      acc.sum += sum;
      acc.globalIndex += curr.sizeInBlocks;

      return acc;
    },
    { globalIndex: 0, sum: 0 },
  ).sum;
};

const normalizeGroups = (groups: Entry[]) => {
  let newGroups = groups;
  let slidingWindowsForGroups = slidingWindows(newGroups, 2);

  while (
    slidingWindowsForGroups.some((group) =>
      group.every((groupItem) => groupItem.type === "space")
    )
  ) {
    const spaceGroupIndex = slidingWindowsForGroups.findIndex((group) =>
      group.every((groupItem) => groupItem.type === "space")
    );
    const spaceGroup = slidingWindowsForGroups[spaceGroupIndex];
    const spaceGroupSize = spaceGroup.reduce(
      (acc, curr) => acc + curr.sizeInBlocks,
      0,
    );

    const newGroupsToUpdate = newGroups.slice(0, spaceGroupIndex).concat({
      type: "space",
      sizeInBlocks: spaceGroupSize,
    });

    newGroups = newGroupsToUpdate.concat(newGroups.slice(spaceGroupIndex + 2));
    slidingWindowsForGroups = slidingWindows(newGroups, 2);
  }

  return newGroups;
};
