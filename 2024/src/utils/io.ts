import { getLines } from "#utils/string.ts";

export const readLines = async (path: string) => {
  const inputText = await Deno.readTextFile(path);
  return getLines(inputText);
};
