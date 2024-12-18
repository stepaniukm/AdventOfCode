import {
  getNeighborSimpleVectors,
  Position,
  positionToString,
} from "#utils/misc.ts";
import { type SupportedPartResultTypes } from "#utils/aoc.ts";

const SIMPLE_BYTES = 12;
const BYTES = 1024;
const EMPTY = ".";
const BYTE = "#";

export const part1 = (input: string[]): SupportedPartResultTypes => {
  const IS_SIMPLE = Deno.env.get("IS_SIMPLE") === "1";
  const corruptedBytes = input.map((line) => {
    const [column, row] = line.split(",").map(Number);

    return [row, column] as Position;
  });

  const bytesToSimulate = IS_SIMPLE
    ? corruptedBytes.slice(0, SIMPLE_BYTES)
    : corruptedBytes.slice(0, BYTES);

  const width = IS_SIMPLE ? 7 : 71;
  const height = IS_SIMPLE ? 7 : 71;

  const startingPosition = [0, 0] as Position;
  const endingPosition = [height - 1, width - 1] as Position;

  const basicMap = getBasicMap(height, width);

  bytesToSimulate.forEach(([byteRow, byteColumn]) => {
    basicMap[byteRow][byteColumn] = BYTE;
  });

  const shortestPath = getShortestPath(
    basicMap,
    startingPosition,
    endingPosition,
    width,
    height,
  );

  if (shortestPath === null) {
    throw new Error("No path found");
  }

  return shortestPath;
};

export const part2 = (input: string[]): SupportedPartResultTypes => {
  const IS_SIMPLE = Deno.env.get("IS_SIMPLE") === "1";
  const corruptedBytes = input.map((line) => {
    const [column, row] = line.split(",").map(Number);

    return [row, column] as Position;
  });

  let currentOffset = 0;

  while (true) {
    const bytesToSimulate = IS_SIMPLE
      ? corruptedBytes.slice(0, SIMPLE_BYTES + currentOffset)
      : corruptedBytes.slice(0, BYTES + currentOffset);

    const width = IS_SIMPLE ? 7 : 71;
    const height = IS_SIMPLE ? 7 : 71;

    const startingPosition = [0, 0] as Position;
    const endingPosition = [height - 1, width - 1] as Position;

    const basicMap = getBasicMap(height, width);

    bytesToSimulate.forEach(([byteRow, byteColumn]) => {
      basicMap[byteRow][byteColumn] = BYTE;
    });

    const shortestPath = getShortestPath(
      basicMap,
      startingPosition,
      endingPosition,
      width,
      height,
    );

    if (shortestPath === null) {
      return positionToString(bytesToSimulate.at(-1)!.toReversed() as Position);
    }

    currentOffset++;
  }
};

const getBasicMap = (height: number, width: number) => {
  return Array.from(
    { length: height },
    () => Array.from({ length: width }, () => EMPTY),
  );
};

const getShortestPath = (
  map: string[][],
  startingPosition: Position,
  endingPosition: Position,
  width: number,
  height: number,
): number | null => {
  const visited = new Set<string>();

  const endingPositionString = positionToString(endingPosition);
  const queues = new Map<number, Position[]>();
  queues.set(0, [startingPosition]);
  let currentDepth = 0;
  let currentQueue = queues.get(currentDepth)!;

  let currentPositionString;

  do {
    if (currentQueue.length === 0) {
      currentDepth++;
      const nextQueue = queues.get(currentDepth);
      if (nextQueue === undefined) {
        return null;
      }
      currentQueue = nextQueue;
      continue;
    }
    const currentPosition = currentQueue.shift()!;
    currentPositionString = positionToString(currentPosition);

    if (visited.has(currentPositionString)) {
      continue;
    }
    visited.add(positionToString(currentPosition));

    const simpleNeighborsVectors = getNeighborSimpleVectors({
      width,
      height,
      position: currentPosition,
    });

    const neighborsPositions = simpleNeighborsVectors.map(
      ([rowOffset, columnOffset]) => {
        return [
          currentPosition[0] + rowOffset,
          currentPosition[1] + columnOffset,
        ] as Position;
      },
    );

    const unvisitedCorrectNeighborsPositions = neighborsPositions.filter(
      (neighborPosition) => {
        const neighborPositionString = positionToString(neighborPosition);
        const neighbor = map[neighborPosition[0]][neighborPosition[1]];

        return !visited.has(neighborPositionString) && neighbor === EMPTY;
      },
    );

    const nextQueue = queues.get(currentDepth + 1) ?? [];
    nextQueue.push(...unvisitedCorrectNeighborsPositions);
    queues.set(currentDepth + 1, nextQueue);
  } while (currentPositionString !== endingPositionString);

  return currentDepth;
};
