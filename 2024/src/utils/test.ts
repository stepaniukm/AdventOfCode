import { assertEquals } from "@std/assert";

export const assertEqualToBigIntOrNumber = (
  value: number | bigint | string,
  expected: string,
) => {
  if (typeof value === "string") {
    assertEquals(value, expected);
  } else if (typeof value === "number") {
    assertEquals(value, Number(expected));
  } else {
    assertEquals(value.toString(), expected);
  }
};
