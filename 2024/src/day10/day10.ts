import { getCharMap, getStartingPositions } from "#utils/array.ts";
import { getNeighborSimpleVectors, type Position } from "#utils/misc.ts";

export const part1 = (input: string[]): number | bigint => {
  const map = getCharMap(input);
  const startingPoints = getStartingPositions(map, "0");

  return startingPoints.reduce((acc, startingPoint) => {
    const startingPointValue = countHikingTrails(startingPoint, map);
    return acc + startingPointValue;
  }, 0);
};

export const part2 = (input: string[]): number | bigint => {
  const map = getCharMap(input);
  const startingPoints = getStartingPositions(map, "0");

  return startingPoints.reduce((acc, startingPoint) => {
    const startingPointValue = countDistinctHikingTrails(startingPoint, map);
    return acc + startingPointValue;
  }, 0);
};

const countHikingTrails = (
  startingPosition: Position,
  map: string[][],
  currentPosition: Position = startingPosition,
  visited: Set<string> = new Set(),
  previousHeight = -1,
  foundTrails: Set<string> = new Set(),
): number => {
  const height = map.length;
  const width = map[0].length;

  const [row, column] = currentPosition;
  const nextExpectedHeight = previousHeight + 1;
  const currentHeight = Number(map[row][column]);

  if (
    currentHeight !== nextExpectedHeight || Number.isNaN(currentHeight)
  ) {
    return 0;
  }

  if (currentHeight === 9) {
    const foundTrailString = [
      startingPosition.join(","),
      currentPosition.join(","),
    ].join("->");
    if (foundTrails.has(foundTrailString)) {
      return 0;
    } else {
      foundTrails.add(foundTrailString);
      return 1;
    }
  }

  const neighbourVectors = getNeighborSimpleVectors(
    { position: currentPosition, height, width },
  );

  const onlyValidNeighbours = neighbourVectors.flatMap(
    ([rowOffset, columnOffset]) => {
      const newPosition = [row + rowOffset, column + columnOffset] as Position;
      const newPositionString = newPosition.join(",");

      if (
        visited.has(newPositionString)
      ) {
        return [];
      } else {
        return [newPosition];
      }
    },
  );

  const counts = onlyValidNeighbours.map((newPosition) => {
    const result = countHikingTrails(
      startingPosition,
      map,
      newPosition,
      new Set([...visited, currentPosition.join(",")]),
      currentHeight,
      foundTrails,
    );

    return result;
  });

  const countsSum = counts.reduce((acc, curr) => acc + curr, 0);

  return countsSum;
};

const countDistinctHikingTrails = (
  startingPosition: Position,
  map: string[][],
  currentPosition: Position = startingPosition,
  visited: Set<string> = new Set(),
  previousHeight = -1,
): number => {
  const height = map.length;
  const width = map[0].length;

  const [row, column] = currentPosition;
  const nextExpectedHeight = previousHeight + 1;
  const currentHeight = Number(map[row][column]);

  if (
    currentHeight !== nextExpectedHeight || Number.isNaN(currentHeight)
  ) {
    return 0;
  }

  if (currentHeight === 9) {
    return 1;
  }

  const neighbourVectors = getNeighborSimpleVectors(
    { position: currentPosition, height, width },
  );

  const onlyValidNeighbours = neighbourVectors.flatMap(
    ([rowOffset, columnOffset]) => {
      const newPosition = [row + rowOffset, column + columnOffset] as Position;
      const newPositionString = newPosition.join(",");

      if (
        visited.has(newPositionString)
      ) {
        return [];
      } else {
        return [newPosition];
      }
    },
  );

  const counts = onlyValidNeighbours.map((newPosition) => {
    const result = countDistinctHikingTrails(
      startingPosition,
      map,
      newPosition,
      new Set([...visited, currentPosition.join(",")]),
      currentHeight,
    );

    return result;
  });

  const countsSum = counts.reduce((acc, curr) => acc + curr, 0);

  return countsSum;
};
