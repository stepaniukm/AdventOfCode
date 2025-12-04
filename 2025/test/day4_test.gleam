import day4/day4
import helpers

pub fn day4_part1_simple_test() {
  let input = helpers.read_lines_from_file("./src/day4/simple-input.txt")
  let result = day4.part1(input)

  assert result == 13
}

pub fn day4_part1_actual_test() {
  let input = helpers.read_lines_from_file("./src/day4/input.txt")
  let result = day4.part1(input)

  assert result == 1516
}

pub fn day4_part2_simple_test() {
  let input = helpers.read_lines_from_file("./src/day4/simple-input.txt")

  assert day4.part2(input) == 43
}

pub fn day4_part2_actual_test() {
  let input = helpers.read_lines_from_file("./src/day4/input.txt")

  assert day4.part2(input) == 9122
}
