import day3/day3
import helpers

pub fn day3_part1_simple_test() {
  let input = helpers.read_lines_from_file("./src/day3/simple-input.txt")
  let result = day3.part1(input)

  assert result == 357
}

pub fn day3_part1_actual_test() {
  let input = helpers.read_lines_from_file("./src/day3/input.txt")
  let result = day3.part1(input)

  assert result == 16_887
}

pub fn day3_part2_simple_test() {
  let input = helpers.read_lines_from_file("./src/day3/simple-input.txt")

  assert day3.part2(input) == 3_121_910_778_619
}

pub fn day3_part2_actual_test() {
  let input = helpers.read_lines_from_file("./src/day3/input.txt")

  assert day3.part2(input) == 167_302_518_850_275
}
