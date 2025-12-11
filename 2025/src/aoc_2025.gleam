import day10/day10
import gleam/int
import gleam/io
import helpers

pub fn main() -> Nil {
  let input = helpers.read_lines_from_file("./src/day10/input.txt")
  let result = day10.part2(input)

  io.println("Result: " <> int.to_string(result))
}
