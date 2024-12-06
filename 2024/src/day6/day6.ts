import { List } from "immutable";
import { getCharMap, getStartingPositions } from "#utils/array.ts";
import { Direction, Position, rotate90DegreesRight, up } from "#utils/misc.ts";
import {
  availableParallelism,
  FixedThreadPool,
} from "@poolifier/poolifier-web-worker";

import { MyData, MyResponse } from "./checkCycle.worker.ts";

const workerFileURL = new URL("./checkCycle.worker.ts", import.meta.url);
const fixedPool = new FixedThreadPool<MyData, MyResponse>(
  availableParallelism(),
  workerFileURL,
  {
    errorEventHandler: (e: ErrorEvent) => {
      console.error(e);
    },
  }
);
export const part1 = (input: string[]): number => {
  const map = getCharMap(input);
  const allStartPositions = getStartingPositions(map, "^");
  const startPosition = allStartPositions[0];

  const distinctPositions = getDistinctGuardPosition(map, startPosition);

  return distinctPositions.size;
};
export const part2 = async (input: string[]): Promise<number> => {
  const map = getCharMap(input);
  const listMap = List(map.map((row) => List(row)));
  const allStartPositions = getStartingPositions(map, "^");
  const startPosition = allStartPositions[0];

  const distinctPositions = getDistinctGuardPosition(map, startPosition);
  distinctPositions.delete(startPosition.join(","));
  const actualDistinctPositions = [...distinctPositions].map((position) => {
    return position.split(",").map(Number) as Position;
  });

  const results = await Promise.all(
    actualDistinctPositions.map((position) => {
      const newMap = listMap
        .withMutations((mutableMap) => {
          mutableMap.set(
            position[0],
            mutableMap.get(position[0])!.set(position[1], "#")
          );
        })
        .toJS();

      return fixedPool.execute({ map: newMap, startPosition });
    })
  );

  const result = results
    .map((result) => result.hasCycle)
    .filter(Boolean).length;

  return result;
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
