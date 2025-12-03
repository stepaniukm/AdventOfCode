import day2/day2
import helpers

pub fn day2_part1_simple_test() {
  let input = helpers.read_lines_from_file("./src/day2/simple-input.txt")
  let result = day2.part1(input)

  assert result == 1_227_775_554
}

pub fn day2_part1_actual_test() {
  let input = helpers.read_lines_from_file("./src/day2/input.txt")
  let result = day2.part1(input)

  assert result == 35_367_539_282
}

pub fn day2_part2_simple_test() {
  let input = helpers.read_lines_from_file("./src/day2/simple-input.txt")
  let result = day2.part2(input)

  assert result == 4_174_379_265
}

pub fn day2_part2_actual_test() {
  let input = helpers.read_lines_from_file("./src/day2/input.txt")
  let result = day2.part2(input)

  assert result == 45_814_076_230
}
