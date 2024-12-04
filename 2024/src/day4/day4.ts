const word = "XMAS";
type Position = [row: number, column: number];

export const part1 = (input: string[]): number => {
  const map = input.map((line) => {
    return [...line].map((char) => {
      return char;
    });
  });

  const startPositions = map.reduce((acc, line, lineIndex) => {
    return acc.concat(
      line.flatMap((char, charIndex) => {
        if (char === word.at(0)) return [[lineIndex, charIndex] as const];
        return [];
      })
    );
  }, [] as Array<Position>);

  const correctWords = startPositions.reduce((acc, startPosition) => {
    const correctNeighborsVectors = getNeighborVectors({
      position: startPosition,
      width: map.length,
      height: map[0].length,
    });

    const correctNumber = correctNeighborsVectors.filter((neighborVector) => {
      return checkPosition({
        map,
        currentPosition: startPosition,
        direction: neighborVector,
        word: "",
        wordToMatch: word,
        width: map.length,
        height: map[0].length,
      });
    });
    return acc + correctNumber.length;
  }, 0);

  return correctWords;
};
export const part2 = (input: string[]): number => {
  const map = input.map((line) => {
    return [...line].map((char) => {
      return char;
    });
  });

  const startPositions = map.slice(1, -1).reduce((acc, line, lineIndex) => {
    return acc.concat(
      line.slice(1, -1).flatMap((char, charIndex) => {
        return char === "A" ? [[lineIndex + 1, charIndex + 1] as const] : [];
      })
    );
  }, [] as Array<Position>);

  const correctWords = startPositions.reduce((acc, startPosition) => {
    const leftTopCorner = map[startPosition[0] - 1][startPosition[1] - 1];
    const leftBottomCorner = map[startPosition[0] + 1][startPosition[1] - 1];
    const rightTopCorner = map[startPosition[0] - 1][startPosition[1] + 1];
    const rightBottomCorner = map[startPosition[0] + 1][startPosition[1] + 1];
    const middle = map[startPosition[0]][startPosition[1]];

    // M . S
    // . A .
    // M . S
    if (
      leftTopCorner === "M" &&
      leftBottomCorner === leftTopCorner &&
      rightTopCorner === "S" &&
      rightBottomCorner === rightTopCorner
    )
      return acc + 1;

    // S . M
    // . A .
    // S . M
    if (
      leftTopCorner === "S" &&
      leftBottomCorner === leftTopCorner &&
      rightTopCorner === "M" &&
      rightBottomCorner === rightTopCorner
    )
      return acc + 1;

    // S . S
    // . A .
    // M . M
    if (
      leftTopCorner === "S" &&
      rightTopCorner === leftTopCorner &&
      leftBottomCorner === "M" &&
      rightBottomCorner === leftBottomCorner
    )
      return acc + 1;

    // M . M
    // . A .
    // S . S
    if (
      leftTopCorner === "M" &&
      rightTopCorner === leftTopCorner &&
      leftBottomCorner === "S" &&
      rightBottomCorner === leftBottomCorner
    )
      return acc + 1;

    return acc;
  }, 0);

  return correctWords;
};

const getNeighborVectors = ({
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

const checkPosition = ({
  map,
  currentPosition,
  direction,
  word,
  wordToMatch,
  width,
  height,
}: {
  map: string[][];
  currentPosition: Position;
  direction: Position;
  word: string;
  wordToMatch: string;
  width: number;
  height: number;
}) => {
  const [row, column] = currentPosition;
  const [rowDirection, columnDirection] = direction;
  const nextIndex = word.length;
  const expectedNextChar = wordToMatch.at(nextIndex);

  if (row < 0 || row >= height) return false;
  if (column < 0 || column >= width) return false;

  const currentChar = map[row][column];

  if (currentChar !== expectedNextChar) return false;
  if (word.length === wordToMatch.length - 1) return true;

  return checkPosition({
    map,
    currentPosition: [row + rowDirection, column + columnDirection],
    direction,
    word: word + currentChar,
    wordToMatch,
    width,
    height,
  });
};
