import { getCharMap, getCharsPositionsGroupedByChar } from "#utils/array.ts";

export const part1 = (input: string[]): number | bigint => {
  const map = getCharMap(input);
  const height = map.length;
  const width = map[0].length;

  const positionsGroupedByFrequency = getCharsPositionsGroupedByChar(map, ".");

  const distinctLocations = Object.values(positionsGroupedByFrequency).reduce(
    (acc, positions) => {
      for (let i = 0; i <= positions.length - 1; i++) {
        for (let j = i + 1; j < positions.length; j++) {
          const positionA = positions[i];
          const positionB = positions[j];

          const difference = [
            positionB[0] - positionA[0],
            positionB[1] - positionA[1],
          ];

          const newPositionA = [
            positionA[0] - difference[0],
            positionA[1] - difference[1],
          ];

          const newPositionB = [
            positionB[0] + difference[0],
            positionB[1] + difference[1],
          ];

          if (
            newPositionA[0] >= 0 &&
            newPositionA[0] < height &&
            newPositionA[1] >= 0 &&
            newPositionA[1] < width
          ) {
            acc.add(newPositionA.join(","));
          }

          if (
            newPositionB[0] >= 0 &&
            newPositionB[0] < height &&
            newPositionB[1] >= 0 &&
            newPositionB[1] < width
          ) {
            acc.add(newPositionB.join(","));
          }
        }
      }

      return acc;
    },
    new Set<string>(),
  );

  return distinctLocations.size;
};
export const part2 = (input: string[]): number | bigint => {
  const map = getCharMap(input);
  const height = map.length;
  const width = map[0].length;

  const positionsGroupedByFrequency = getCharsPositionsGroupedByChar(map, ".");

  const distinctLocations = Object.values(positionsGroupedByFrequency).reduce(
    (acc, positions) => {
      for (let i = 0; i <= positions.length - 1; i++) {
        for (let j = i + 1; j < positions.length; j++) {
          const positionA = positions[i];
          const positionB = positions[j];

          acc.add(positionA.join(","));
          acc.add(positionB.join(","));

          const difference = [
            positionB[0] - positionA[0],
            positionB[1] - positionA[1],
          ];

          const newPositionA = [
            positionA[0] - difference[0],
            positionA[1] - difference[1],
          ];

          while (
            newPositionA[0] >= 0 &&
            newPositionA[0] < height &&
            newPositionA[1] >= 0 &&
            newPositionA[1] < width
          ) {
            acc.add(newPositionA.join(","));
            newPositionA[0] -= difference[0];
            newPositionA[1] -= difference[1];
          }

          const newPositionB = [
            positionB[0] + difference[0],
            positionB[1] + difference[1],
          ];

          while (
            newPositionB[0] >= 0 &&
            newPositionB[0] < height &&
            newPositionB[1] >= 0 &&
            newPositionB[1] < width
          ) {
            acc.add(newPositionB.join(","));
            newPositionB[0] += difference[0];
            newPositionB[1] += difference[1];
          }
        }
      }

      return acc;
    },
    new Set<string>(),
  );

  return distinctLocations.size;
};
