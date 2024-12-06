import { ThreadWorker } from "@poolifier/poolifier-web-worker"; // x-release-please-version
import { Direction, Position, rotate90DegreesRight, up } from "#utils/misc.ts";

export interface MyData {
  map: string[][];
  startPosition: Position;
}

export interface MyResponse {
  hasCycle: boolean;
}

class MyThreadWorker extends ThreadWorker<MyData, MyResponse> {
  constructor() {
    super((data?: MyData) => this.process(data!), {
      maxInactiveTime: 60000,
    });
  }

  private process(data: MyData): MyResponse {
    const hasCycle = checkCycle(data.map, data.startPosition);
    return { hasCycle };
  }
}

export default new MyThreadWorker();

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
