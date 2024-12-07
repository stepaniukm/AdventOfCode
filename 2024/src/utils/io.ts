import { getLines } from "#utils/input.ts";

export const readLines = async (path: string) => {
  const inputText = await Deno.readTextFile(path);
  return getLines(inputText);
};
