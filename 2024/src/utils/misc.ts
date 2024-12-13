export const identity = <T>(anything: T) => anything;

export type Position = [row: number, column: number];
export type Direction = [rowOffset: number, columnOffset: number];

export const getNeighborVectors = ({
  position,
  width,
  height,
}: {
  position: Position;
  width: number;
  height: number;
}) => {
  return [
    [-1, -1],
    [-1, 0],
    [-1, 1],
    [0, -1],
    [0, 1],
    [1, -1],
    [1, 0],
    [1, 1],
  ].flatMap(([rowOffset, columnOffset]) => {
    if (position[0] + rowOffset < 0) return [];
    if (position[0] + rowOffset >= width) return [];
    if (position[1] + columnOffset < 0) return [];
    if (position[1] + columnOffset >= height) return [];

    return [[rowOffset, columnOffset] as Position];
  });
};

export const getNeighborSimpleVectors = ({
  position,
  width,
  height,
}: {
  position: Position;
  width: number;
  height: number;
}) => {
  return [
    [-1, 0],
    [0, -1],
    [0, 1],
    [1, 0],
  ].flatMap(([rowOffset, columnOffset]) => {
    if (position[0] + rowOffset < 0) return [];
    if (position[0] + rowOffset >= width) return [];
    if (position[1] + columnOffset < 0) return [];
    if (position[1] + columnOffset >= height) return [];

    return [[rowOffset, columnOffset] as Position];
  });
};

export const up = [-1, 0] as Direction;
export const down = [1, 0] as Direction;
export const left = [0, -1] as Direction;
export const right = [0, 1] as Direction;

export const rotate90DegreesRight = (direction: Direction): Direction => {
  if (direction === up || (direction[0] === up[0] && direction[1] === up[1])) {
    return right;
  }
  if (
    direction === right ||
    (direction[0] === right[0] && direction[1] === right[1])
  ) {
    return down;
  }
  if (
    direction === down ||
    (direction[0] === down[0] && direction[1] === down[1])
  ) {
    return left;
  }
  if (
    direction === left ||
    (direction[0] === left[0] && direction[1] === left[1])
  ) {
    return up;
  }

  throw new Error("Invalid direction");
};

export const positionToString = (position: Position) => {
  return position.join(",");
};
