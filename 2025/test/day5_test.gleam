import day5/day5
import helpers

pub fn day5_part1_simple_test() {
  let input = helpers.read_lines_from_file("./src/day5/simple-input.txt")
  let result = day5.part1(input)

  assert result == 3
}

pub fn day5_part1_actual_test() {
  let input = helpers.read_lines_from_file("./src/day5/input.txt")
  let result = day5.part1(input)

  assert result == 712
}

pub fn day5_part2_simple_test() {
  let input = helpers.read_lines_from_file("./src/day5/simple-input.txt")

  assert day5.part2(input) == 14
}

pub fn day5_part2_actual_test() {
  let input = helpers.read_lines_from_file("./src/day5/input.txt")

  assert day5.part2(input) == 332_998_283_036_769
}
