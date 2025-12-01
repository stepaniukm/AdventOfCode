import day1/day1
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import helpers
import simplifile

type PartDifficulty {
  SimpleInput
  RealInput
}

fn get_part1() {
  day1.part1
}

fn get_input_path(day: Int, difficulty: PartDifficulty) -> String {
  case difficulty {
    SimpleInput ->
      string.concat(["./src/day", int.to_string(day), "/simple_input.txt"])
    RealInput -> string.concat(["./src/day", int.to_string(day), "/input.txt"])
  }
}

pub fn main() -> Nil {
  let day = 1
  let part1: Bool = True
  let part1_difficulty = SimpleInput
  let part2: Bool = False
  let part2_difficulty = SimpleInput

  case part1 {
    True -> {
      let path = get_input_path(day, part1_difficulty)
      let input = helpers.read_lines_from_file(path)
      let result = get_part1()(input)

      io.println(string.concat(["=== Day ", int.to_string(day), " Part 1 ==="]))
      io.println(string.concat(["Part 1 result: ", int.to_string(result)]))
    }

    False -> Nil
  }

  case part2 {
    True -> {
      let path = get_input_path(day, part2_difficulty)
      let input = helpers.read_lines_from_file(path)
      let result = get_part1()(input)

      io.println(string.concat(["=== Day ", int.to_string(day), " Part 2 ==="]))
      io.println(string.concat(["Part 2 result: ", int.to_string(result)]))
    }

    False -> Nil
  }
}
