export const identity = <T>(anything: T) => anything;

export type Position = [row: number, column: number];
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
