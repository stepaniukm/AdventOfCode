import day1/day1
import gleam/result
import gleam/string
import gleeunit
import simplifile

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn day1_part1_simple_test() {
  let input =
    simplifile.read("./src/day1/simple-input.txt")
    |> result.unwrap("")
    |> string.split("\n")

  assert day1.part1(input) == 3
}

pub fn day1_part1_actual_test() {
  let input =
    simplifile.read("./src/day1/input.txt")
    |> result.unwrap("")
    |> string.split("\n")

  assert day1.part1(input) == 1165
}

pub fn day1_part2_simple_test() {
  let input =
    simplifile.read("./src/day1/simple-input.txt")
    |> result.unwrap("")
    |> string.split("\n")

  assert day1.part2(input) == 6
}

pub fn day1_part2_actual_test() {
  let input =
    simplifile.read("./src/day1/input.txt")
    |> result.unwrap("")
    |> string.split("\n")

  assert day1.part2(input) == 6496
}
