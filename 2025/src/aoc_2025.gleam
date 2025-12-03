import day3/day3
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day3/input.txt")
  let result = day3.part2(input)

  io.println("Result: " <> int.to_string(result))
}
