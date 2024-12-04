import { getCharMap } from "#utils/array.ts";
import { getNeighborVectors, Position } from "#utils/misc.ts";

const WORD_TO_SEARCH = "XMAS";

export const part1 = (input: string[]): number => {
  const map = getCharMap(input);

  const startPositions = map.reduce((acc, line, lineIndex) => {
    return acc.concat(
      line.flatMap((char, charIndex) => {
        if (char === WORD_TO_SEARCH.at(0))
          return [[lineIndex, charIndex] as const];
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
        width: map.length,
        height: map[0].length,
      });
    });
    return acc + correctNumber.length;
  }, 0);

  return correctWords;
};
export const part2 = (input: string[]): number => {
  const map = getCharMap(input);

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

    const words = [
      leftTopCorner.concat("A").concat(rightBottomCorner),
      leftBottomCorner.concat("A").concat(rightTopCorner),
      rightTopCorner.concat("A").concat(leftBottomCorner),
      rightBottomCorner.concat("A").concat(leftTopCorner),
    ];

    const correctNumber = words.filter(
      (word) => word === WORD_TO_SEARCH.slice(1)
    ).length;

    return correctNumber === 2 ? acc + 1 : acc;
  }, 0);

  return correctWords;
};

const checkPosition = ({
  map,
  currentPosition,
  direction,
  word,
  width,
  height,
}: {
  map: string[][];
  currentPosition: Position;
  direction: Position;
  word: string;
  width: number;
  height: number;
}) => {
  const [row, column] = currentPosition;
  const [rowDirection, columnDirection] = direction;
  const nextIndex = word.length;
  const expectedNextChar = WORD_TO_SEARCH.at(nextIndex);

  if (row < 0 || row >= height) return false;
  if (column < 0 || column >= width) return false;

  const currentChar = map[row][column];

  if (currentChar !== expectedNextChar) return false;
  if (word.length === WORD_TO_SEARCH.length - 1) return true;

  return checkPosition({
    map,
    currentPosition: [row + rowDirection, column + columnDirection],
    direction,
    word: word + currentChar,
    width,
    height,
  });
};
