import { assertEquals } from "@std/assert";

export const assertEqualToBigIntOrNumber = (
  value: number | bigint,
  expected: string,
) => {
  if (typeof value === "number") {
    assertEquals(value, Number(expected));
  } else {
    assertEquals(value.toString(), expected);
  }
};
