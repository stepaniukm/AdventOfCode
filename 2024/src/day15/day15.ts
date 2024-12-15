import { parseTwoPartsSeparatedBySpace } from "#utils/input.ts";
import { getStartingPositions } from "#utils/array.ts";
import { Position } from "#utils/misc.ts";

const WALL = "#";
const ROBOT = "@";
const BOX = "O";
const EMPTY = ".";

const BOX_START = "[";
const BOX_END = "]";

type BoxPositionPart2 = [x: number, [startY: number, endY: number]];

export const part1 = (input: string[]): number | bigint => {
  const { partA: map, partB: movesLines } = parseTwoPartsSeparatedBySpace({
    input,
    partAParser: (line: string) => line.split(""),
    partBParser: (line: string) => line.split(""),
  });

  const moves = movesLines.flat();
  let currentMap = map;

  const walls = getStartingPositions(currentMap, WALL);
  const robots = getStartingPositions(currentMap, ROBOT);
  let boxes = getStartingPositions(currentMap, BOX);

  const width = currentMap[0].length;
  const height = currentMap.length;

  const robot = robots[0];

  moves.forEach((move) => {
    const moveOffset = getMoveOffset(move);

    const [boxesToMove, stillBoxes] = getBoxesToMove(
      boxes,
      robot,
      moveOffset,
      currentMap,
    );
    const newPotentialBoxesPositions = boxesToMove.map((box) =>
      [
        box[0] + moveOffset[0],
        box[1] + moveOffset[1],
      ] as Position
    );
    const isMoveOfBoxesPossible = newPotentialBoxesPositions.every(
      (newPotentialBoxPosition) =>
        currentMap[newPotentialBoxPosition[0]][newPotentialBoxPosition[1]] !==
          WALL,
    );

    if (isMoveOfBoxesPossible) {
      boxes = [...stillBoxes, ...newPotentialBoxesPositions] as [
        Position,
        ...Position[],
      ];
    }

    const newPotentialRobotPosition = [
      robot[0] + moveOffset[0],
      robot[1] + moveOffset[1],
    ] as Position;

    const isMoveOfRobotPossible =
      currentMap[newPotentialRobotPosition[0]][newPotentialRobotPosition[1]] !==
        WALL && !boxes.some((box) => {
          return box[0] === newPotentialRobotPosition[0] &&
            box[1] === newPotentialRobotPosition[1];
        });

    if (isMoveOfRobotPossible) {
      robot[0] += moveOffset[0];
      robot[1] += moveOffset[1];
    }
    currentMap = visualise(walls, robot, boxes, width, height);
  });

  return boxes.reduce((acc, curr) => {
    return acc + curr[0] * 100 + curr[1];
  }, 0);
};

export const part2 = (input: string[]): number | bigint => {
  const { partA: map, partB: movesLines } = parseTwoPartsSeparatedBySpace({
    input,
    partAParser: (line: string) => line.split(""),
    partBParser: (line: string) => line.split(""),
  });

  const moves = movesLines.flat();
  const resizedMap = resizeMap(map);
  const robots = getStartingPositions(resizedMap, ROBOT);
  const robot = robots[0];
  const walls = getStartingPositions(resizedMap, WALL);
  let boxes = getBoxPositionsPart2(resizedMap);

  let currentMap = resizedMap;

  const width = currentMap[0].length;
  const height = currentMap.length;

  moves.forEach((move) => {
    const moveOffset = getMoveOffset(move);

    const [boxesToMove, stillBoxes] = getBoxesToMovePart2(
      boxes,
      robot,
      moveOffset,
      currentMap,
    );

    const newPotentialBoxesPositions = boxesToMove.map((box) =>
      [
        box[0] + moveOffset[0],
        box[1].map((boxY) => boxY + moveOffset[1]),
      ] as BoxPositionPart2
    );
    const isMoveOfBoxesPossible = newPotentialBoxesPositions.every(
      (newPotentialBoxPosition) =>
        newPotentialBoxPosition[1].every((newPositionY) => {
          return currentMap[newPotentialBoxPosition[0]][newPositionY] !== WALL;
        }),
    );

    if (isMoveOfBoxesPossible) {
      boxes = [...stillBoxes, ...newPotentialBoxesPositions] as [
        BoxPositionPart2,
        ...BoxPositionPart2[],
      ];
    }

    const newPotentialRobotPosition = [
      robot[0] + moveOffset[0],
      robot[1] + moveOffset[1],
    ] as Position;

    const isMoveOfRobotPossible =
      currentMap[newPotentialRobotPosition[0]][newPotentialRobotPosition[1]] !==
        WALL && !boxes.some((box) => {
          return box[1].some((boxY) => {
            return box[0] === newPotentialRobotPosition[0] &&
              newPotentialRobotPosition[1] === boxY;
          });
        });

    if (isMoveOfRobotPossible) {
      robot[0] += moveOffset[0];
      robot[1] += moveOffset[1];
    }
    currentMap = visualisePart2(walls, robot, boxes, width, height);
  });

  return boxes.reduce((acc, curr) => {
    return acc + curr[0] * 100 + curr[1][0];
  }, 0);
};

const getMoveOffset = (move: string) => {
  switch (move) {
    case "^":
      return [-1, 0] as Position;
    case "v":
      return [1, 0] as Position;
    case "<":
      return [0, -1] as Position;
    case ">":
      return [0, 1] as Position;
    default:
      throw new Error(`Unknown move: ${move}`);
  }
};

const getBoxesToMove = (
  boxes: Position[],
  robot: Position,
  moveOffset: Position,
  map: string[][],
) => {
  const boxesToMove = [] as Position[];
  const nextPositionToCheck = [
    robot[0] + moveOffset[0],
    robot[1] + moveOffset[1],
  ];

  let nextPositionChar = map[nextPositionToCheck[0]][nextPositionToCheck[1]];
  if (nextPositionChar === WALL || nextPositionChar === EMPTY) {
    return [[], boxes];
  }

  do {
    const boxToMove = boxes.find((box) =>
      box[0] === nextPositionToCheck[0] && box[1] === nextPositionToCheck[1]
    )!;

    boxesToMove.push(boxToMove);
    nextPositionToCheck[0] += moveOffset[0];
    nextPositionToCheck[1] += moveOffset[1];
    nextPositionChar = map[nextPositionToCheck[0]][nextPositionToCheck[1]];
  } while (nextPositionChar === BOX);

  const stillBoxes = boxes.filter((box) =>
    !boxesToMove.some((boxToMove) => boxToMove === box)
  );

  return [boxesToMove, stillBoxes] as const;
};

const getBoxesToMovePart2 = (
  boxes: BoxPositionPart2[],
  robot: Position,
  moveOffset: Position,
  map: string[][],
) => {
  const allBoxesToMove = [] as BoxPositionPart2[];
  let nextPositionsToCheck = [[
    robot[0] + moveOffset[0],
    robot[1] + moveOffset[1],
  ]];

  const isUp = moveOffset[0] < 0;
  const isDown = moveOffset[0] > 0;
  const isUpOrDown = isUp || isDown;

  let nextPositionChars = nextPositionsToCheck.map((nextPositionToCheck) =>
    map[nextPositionToCheck[0]][nextPositionToCheck[1]]
  );

  if (
    nextPositionChars.some((char) => char === WALL) ||
    nextPositionChars.every((char) => char === EMPTY)
  ) {
    return [[], boxes];
  }

  do {
    const boxesToMove = boxes.filter((box) =>
      nextPositionsToCheck.some((nextPositionToCheck) =>
        box[0] === nextPositionToCheck[0] &&
        box[1].some((boxY) => boxY === nextPositionToCheck[1])
      )
    )!;

    const newBoxesToMove = boxesToMove.filter((box) => {
      return !allBoxesToMove.some((boxToMove) => {
        return boxToMove[0] === box[0] && boxToMove[1][0] === box[1][0] &&
          boxToMove[1][1] === box[1][1];
      });
    });

    allBoxesToMove.push(...newBoxesToMove);

    nextPositionsToCheck = newBoxesToMove.flatMap((box) => {
      return box[1].map((boxY) => {
        return [
          box[0] + moveOffset[0],
          boxY + moveOffset[1],
        ];
      });
    });
    nextPositionChars = nextPositionsToCheck.map((nextPositionToCheck) =>
      map[nextPositionToCheck[0]][nextPositionToCheck[1]]
    );
  } while (
    nextPositionChars.includes(BOX_START) || nextPositionChars.includes(BOX_END)
  );

  const stillBoxes = boxes.filter((box) =>
    !allBoxesToMove.some((boxToMove) => boxToMove === box)
  );

  return [allBoxesToMove, stillBoxes] as const;
};

const visualise = (
  walls: Position[],
  robot: Position,
  boxes: Position[],
  width: number,
  height: number,
) => {
  const map = Array.from(
    { length: height },
    () => Array.from({ length: width }, () => EMPTY),
  );

  walls.forEach(([x, y]) => {
    map[x][y] = WALL;
  });

  map[robot[0]][robot[1]] = ROBOT;

  boxes.forEach(([x, y]) => {
    map[x][y] = BOX;
  });

  return map;
};

const visualisePart2 = (
  walls: Position[],
  robot: Position,
  boxes: BoxPositionPart2[],
  width: number,
  height: number,
) => {
  const map = Array.from(
    { length: height },
    () => Array.from({ length: width }, () => EMPTY),
  );

  walls.forEach(([x, y]) => {
    map[x][y] = WALL;
  });

  map[robot[0]][robot[1]] = ROBOT;

  boxes.forEach(([x, y]) => {
    map[x][y[0]] = BOX_START;
    map[x][y[1]] = BOX_END;
  });

  return map;
};

const resizeMap = (map: string[][]) => {
  return map.map((row) => {
    return row.flatMap((char) => {
      if (char === WALL) {
        return [WALL, WALL];
      } else if (char === BOX) {
        return [BOX_START, BOX_END];
      } else if (char === EMPTY) {
        return [EMPTY, EMPTY];
      } else {
        return [ROBOT, EMPTY];
      }
    });
  });
};

export const getBoxPositionsPart2 = (map: string[][]): BoxPositionPart2[] => {
  const positions = map.flatMap((currentRow, rowIndex) => {
    const columnIndexes = currentRow.flatMap((char, columnIndex) => {
      if (char === "[") {
        return [columnIndex];
      } else {
        return [];
      }
    });

    return columnIndexes.map((columnIndex) => {
      return [rowIndex, [columnIndex, columnIndex + 1]] as BoxPositionPart2;
    });
  });

  if (!positions.length) {
    throw new Error("Start position not found");
  }

  return positions as [BoxPositionPart2, ...BoxPositionPart2[]];
};
