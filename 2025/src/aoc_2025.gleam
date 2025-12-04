import day4/day4
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day4/input.txt")
  let result = day4.part2(input)

  io.println("Result: " <> int.to_string(result))
}
