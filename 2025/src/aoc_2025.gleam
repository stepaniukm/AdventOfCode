import day6/day6
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day6/input.txt")
  let result = day6.part2(input)

  io.println("Result: " <> int.to_string(result))
}
