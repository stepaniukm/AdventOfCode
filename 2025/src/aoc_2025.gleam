import day1/day1
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import simplifile

pub fn main() -> Nil {
  io.println(
    day1.part2(
      simplifile.read("./src/day1/input.txt")
      |> result.unwrap("")
      |> string.split("\n"),
    )
    |> int.to_string(),
  )
}
