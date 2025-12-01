import day1/day1
import helpers

pub fn day1_part1_simple_test() {
  let input = helpers.read_lines_from_file("./src/day1/simple-input.txt")

  assert day1.part1(input) == 3
}

pub fn day1_part1_actual_test() {
  let input = helpers.read_lines_from_file("./src/day1/input.txt")

  assert day1.part1(input) == 1165
}

pub fn day1_part2_simple_test() {
  let input = helpers.read_lines_from_file("./src/day1/simple-input.txt")

  assert day1.part2(input) == 6
}

pub fn day1_part2_actual_test() {
  let input = helpers.read_lines_from_file("./src/day1/input.txt")

  assert day1.part2(input) == 6496
}
