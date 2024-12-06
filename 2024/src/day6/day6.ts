import { getCharMap } from "#utils/array.ts";
import { Direction, Position, rotate90DegreesRight, up } from "#utils/misc.ts";

export const part1 = (input: string[]): number => {
  const map = getCharMap(input);
  const startPosition = map.reduce((acc, currentRow, rowIndex) => {
    if (acc) {
      return acc;
    }

    const charIndex = currentRow.indexOf("^");

    if (charIndex !== -1) {
      return [rowIndex, charIndex] as Position;
    }

    return null;
  }, null as Position | null);
  if (!startPosition) {
    throw new Error("Start position not found");
  }

  const distinctPositions = getDistinctGuardPosition(map, startPosition);

  return distinctPositions.size;
};
export const part2 = (input: string[]): number => {
  const map = getCharMap(input);
  const startPosition = map.reduce((acc, currentRow, rowIndex) => {
    if (acc) {
      return acc;
    }

    const charIndex = currentRow.indexOf("^");

    if (charIndex !== -1) {
      return [rowIndex, charIndex] as Position;
    }

    return null;
  }, null as Position | null);

  if (!startPosition) {
    throw new Error("Start position not found");
  }

  const distinctPositions = getDistinctGuardPosition(map, startPosition);
  distinctPositions.delete(startPosition.join(","));
  const actualDistinctPositions = [...distinctPositions].map((position) => {
    return position.split(",").map(Number) as Position;
  });

  return actualDistinctPositions.filter((position) => {
    const newMap = structuredClone(map);
    newMap[position[0]][position[1]] = "#";

    return checkCycle(newMap, startPosition);
  }).length;
};

export const getDistinctGuardPosition = (
  map: string[][],
  startPosition: Position
): Set<string> => {
  const distinctPositions = new Set<string>();
  distinctPositions.add(startPosition.join(","));

  let direction = up as Direction;

  let currentPosition = startPosition;

  while (
    currentPosition[0] < map.length &&
    currentPosition[0] >= 0 &&
    currentPosition[1] < map[0].length &&
    currentPosition[1] >= 0
  ) {
    const newPosition = [
      currentPosition[0] + direction[0],
      currentPosition[1] + direction[1],
    ] as Position;

    if (
      newPosition[0] < 0 ||
      newPosition[0] >= map.length ||
      newPosition[1] < 0 ||
      newPosition[1] >= map[0].length
    ) {
      break;
    }

    const char = map[newPosition[0]][newPosition[1]];

    if (char === "#") {
      direction = rotate90DegreesRight(direction);
      continue;
    }

    distinctPositions.add(newPosition.join(","));
    currentPosition = newPosition;
  }

  return distinctPositions;
};

export const checkCycle = (
  map: string[][],
  startPosition: Position
): boolean => {
  const distinctPositions = new Set<string>();
  let direction = up as Direction;
  distinctPositions.add(startPosition.join(",") + ":" + direction.join(","));

  let currentPosition = startPosition;

  while (
    currentPosition[0] < map.length &&
    currentPosition[0] >= 0 &&
    currentPosition[1] < map[0].length &&
    currentPosition[1] >= 0
  ) {
    const newPosition = [
      currentPosition[0] + direction[0],
      currentPosition[1] + direction[1],
    ] as Position;

    const newPositionString = newPosition.join(",") + ":" + direction.join(",");

    if (distinctPositions.has(newPositionString)) {
      return true;
    }

    if (
      newPosition[0] < 0 ||
      newPosition[0] >= map.length ||
      newPosition[1] < 0 ||
      newPosition[1] >= map[0].length
    ) {
      break;
    }

    const char = map[newPosition[0]][newPosition[1]];

    if (char === "#") {
      direction = rotate90DegreesRight(direction);
      distinctPositions.add(
        currentPosition.join(",") + ":" + direction.join(",")
      );
      continue;
    }

    distinctPositions.add(newPosition.join(",") + ":" + direction.join(","));
    currentPosition = newPosition;
  }

  return false;
};
