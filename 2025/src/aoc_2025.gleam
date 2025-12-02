import day2/day2
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day2/input.txt")
  let result = day2.part1(input)

  io.println("Result of day2_part1_simple_test: " <> int.to_string(result))
}
