import day7/day7
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day7/input.txt")
  let result = day7.part2(input)

  io.println("Result: " <> int.to_string(result))
}
