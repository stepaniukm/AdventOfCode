import { getCharMap } from "#utils/array.ts";
import { getNeighborSimpleVectors, Position } from "#utils/misc.ts";

type Region = {
  char: string;
  points: Position[];
  area: number;
  perimeter: number;
};

type SidePoint = {
  offsetString: string;
  position: Position;
  direction: "x" | "y";
};

export const part1 = (input: string[]): number | bigint => {
  const map = getCharMap(input);

  const visited = new Set<string>();

  const regions = map.flatMap((row, rowIndex) => {
    return row.flatMap((char, columnIndex) => {
      const position = [rowIndex, columnIndex] as Position;
      const positionString = position.join(",");

      if (visited.has(positionString)) {
        return [];
      } else {
        const region = getCurrentRegion({ map, position, char, visited });
        return [region];
      }
    });
  });

  return regions.reduce((acc, region) => {
    return acc + region.area * region.perimeter;
  }, 0);
};

export const part2 = (input: string[]): number | bigint => {
  const map = getCharMap(input);
  const visited = new Set<string>();

  const regions = map.flatMap((row, rowIndex) => {
    return row.flatMap((char, columnIndex) => {
      const position = [rowIndex, columnIndex] as Position;
      const positionString = position.join(",");

      if (visited.has(positionString)) {
        return [];
      } else {
        const region = getCurrentRegion({ map, position, char, visited });
        return [region];
      }
    });
  });

  const regionsWithSidesCounted = regions.map((region) => {
    const sidePoints: SidePoint[] = region.points.flatMap((point) => {
      const [x, y] = point;

      const allNeighbouringOffsets = [[-1, 0], [1, 0], [0, -1], [0, 1]];

      const sidePoints = allNeighbouringOffsets.flatMap(([offX, offY]) => {
        const [xn, yn] = [x + offX, y + offY] as Position;

        const foundPoint = region.points.find(([xp, yp]) => {
          return xn === xp && yn === yp;
        });

        if (!foundPoint) {
          const direction = offX === 0 ? "y" as const : "x" as const;
          return [{
            position: [
              xn,
              yn,
            ] as Position,
            offsetString: [offX, offY].join(","),
            direction,
          }];
        }

        return [];
      });

      return sidePoints;
    });

    const groupedByDirection = Object.groupBy(
      sidePoints,
      (sidePoint) => sidePoint.direction,
    );

    const sum = Object.values(groupedByDirection).reduce(
      (acc, sidePoints) => {
        const groupedInsideByCommonCoordinate = Object.groupBy(
          sidePoints,
          (sidePoint) => {
            return sidePoint.direction === "x"
              ? sidePoint.position[0]
              : sidePoint.position[1];
          },
        );

        const values = Object.values(groupedInsideByCommonCoordinate);

        return acc +
          values.reduce((acc, curr) => {
            const groupedByOffset = Object.groupBy(
              curr!,
              (sidePoint) => sidePoint.offsetString,
            );

            const groupsScore = Object.values(groupedByOffset).map(
              (offsetGroup) => {
                const currentPositions = offsetGroup!.map((p) =>
                  p.direction === "x" ? p.position[1] : p.position[0]
                );

                const sortedPositions = currentPositions.toSorted((a, b) =>
                  a - b
                );

                let groups = 1;

                for (let i = 1; i < sortedPositions.length; i++) {
                  const current = sortedPositions[i];
                  const prev = sortedPositions[i - 1];
                  if (current - prev !== 1 || current - prev === 0) {
                    groups += 1;
                  }
                }

                return groups;
              },
            );

            return acc + groupsScore.reduce((acc, curr) => acc + curr, 0);
          }, 0);
      },
      0,
    );

    return {
      ...region,
      sides: sum,
    } as const;
  });

  return regionsWithSidesCounted.reduce((acc, region) => {
    return acc + region.area * region.sides;
  }, 0);
};

const getCurrentRegion = (
  {
    map,
    position,
    char,
    visited,
  }: {
    map: string[][];
    position: Position;
    char: string;
    visited: Set<string>;
  },
) => {
  const region = {
    char,
    points: [] as Position[],
    area: 0,
    perimeter: 0,
  } satisfies Region;

  const queue = [position];

  do {
    const currentPosition = queue.shift()!;
    const currentPositionString = currentPosition.join(",");

    if (visited.has(currentPositionString)) {
      continue;
    }

    visited.add(currentPosition.join(","));

    const unprocessedNeighbors = getNeighborSimpleVectors({
      position: currentPosition,
      height: map.length,
      width: map[0].length,
    }).map((neighbourOffsets) => {
      const newPosition = [
        currentPosition[0] + neighbourOffsets[0],
        currentPosition[1] + neighbourOffsets[1],
      ] as Position;

      return newPosition;
    });

    const unvisitedNeighborsWithinTheSameRegion = unprocessedNeighbors.filter(
      (neighbor) => {
        const [row, column] = neighbor;
        const neighborString = neighbor.join(",");
        const neighborChar = map[row][column];
        return neighborChar === char && !visited.has(neighborString);
      },
    );

    const allNeighborsLength = unprocessedNeighbors.length;
    const neighborsWithDifferentChar = unprocessedNeighbors.filter(
      (neighbor) => {
        const [row, column] = neighbor;
        const neighborChar = map[row][column];
        return neighborChar === char;
      },
    );

    const neighborsWithDifferentCharLength = neighborsWithDifferentChar.length;

    const outside = 4 - allNeighborsLength;
    const inside = allNeighborsLength - neighborsWithDifferentCharLength;
    const fence = outside + inside;
    region.perimeter += fence;
    region.area += 1;
    region.points.push(currentPosition);

    queue.push(...unvisitedNeighborsWithinTheSameRegion);
  } while (queue.length > 0);

  return region;
};
